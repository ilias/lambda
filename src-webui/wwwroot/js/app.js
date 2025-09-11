// Modularized: utilities & networking & theme moved to separate modules.
// This file now focuses on REPL, tabs, logs, streaming, evaluation.
import { __guid, debounce } from './util.js';
import { fetchJSON, evalExpr, loadFile } from './net.js';
import { initTheme } from './theme.js';

// (Optional) service worker registration preserved
(function(){
  const SW_ENABLED = true;
  if(SW_ENABLED && 'serviceWorker' in navigator){
    window.addEventListener('load', ()=>{
      navigator.serviceWorker.getRegistration('sw.js').then(reg=>{ if(!reg){ navigator.serviceWorker.register('sw.js').catch(()=>{}); } }).catch(()=>{});
    });
  }
})();

// Root initialization after DOM ready
window.addEventListener('DOMContentLoaded', () => {
  // Elements
  const inputTabsHost = document.getElementById('inputTabs');
  const inputEditorsHost = document.getElementById('inputEditors');
  let inputTabs = []; // {id,name,el,taEl,history:[],histIndex:number}
  let activeInput = null; // tab object
  let exprEl = null; // alias to active textarea for legacy code
  const TABS_STORE_KEY = 'lambdaInputTabs_v1';

  function createInputTab(name){
    const id = 'in_'+Date.now().toString(36)+Math.random().toString(36).slice(2,7);
    const tabEl = document.createElement('div');
  tabEl.className='tab';
  tabEl.id = id + '-tab';
  tabEl.setAttribute('role','tab');
  tabEl.setAttribute('aria-selected','false');
  tabEl.setAttribute('tabindex','-1');
    tabEl.textContent = name;
    tabEl.dataset.tabId = id;
    // DnD reordering
    tabEl.setAttribute('draggable','true');
    tabEl.addEventListener('dragstart', e=>{ tabEl.classList.add('dragging'); e.dataTransfer.effectAllowed='move'; e.dataTransfer.setData('text/plain', id); e.dataTransfer.setData('text/in-tab','1'); });
    tabEl.addEventListener('dragend', ()=> tabEl.classList.remove('dragging'));
    tabEl.addEventListener('dragover', e=>{ if(e.dataTransfer?.getData('text/in-tab')==='1'){ e.preventDefault(); tabEl.classList.add('drag-over'); } });
    tabEl.addEventListener('dragleave', ()=> tabEl.classList.remove('drag-over'));
    tabEl.addEventListener('drop', e=>{
      const srcId = e.dataTransfer?.getData('text/plain'); if(!srcId || srcId===id) return; e.preventDefault(); tabEl.classList.remove('drag-over');
      reorderInputTabs(srcId, id);
    });
    // Rename on double-click
    tabEl.title = 'Double-click to rename';
    tabEl.addEventListener('dblclick', ()=>{
      const labelNode = tabEl.childNodes[0];
      const currentLabel = (labelNode && labelNode.nodeType===Node.TEXT_NODE) ? labelNode.nodeValue : name;
      const next = prompt('Rename tab', currentLabel || '');
      if(next && next.trim()){
        if(labelNode && labelNode.nodeType===Node.TEXT_NODE) labelNode.nodeValue = next.trim();
        const t = inputTabs.find(t=> t.id===id); if(t){ t.name = next.trim(); persistTabs(); }
      }
    });
    if(inputTabs.length>0){
      const close = document.createElement('span');
      close.textContent='×';
      close.className='close';
      close.title='Close input tab';
      close.addEventListener('click', (e)=>{ e.stopPropagation(); closeInputTab(id); });
      tabEl.appendChild(close);
    }
    tabEl.addEventListener('click', ()=> activateInputTab(id));
    if(typeof addInputBtnRef === 'undefined' || !addInputBtnRef || !addInputBtnRef.parentNode){
      inputTabsHost.appendChild(tabEl);
    } else {
      inputTabsHost.insertBefore(tabEl, addInputBtnRef);
    }
  const ta = document.createElement('textarea');
  ta.className='input-pane';
  ta.setAttribute('role','tabpanel');
  ta.setAttribute('aria-labelledby', tabEl.id);
    ta.rows=8;
    ta.placeholder=':help or succ 41 or let add = x,y -> x + y in add 2 3';
    ta.dataset.tabId = id;
  ta.addEventListener('input', updateContinuationHint);
  ta.addEventListener('input', debounce(()=> persistTabs(), 300));
    ta.addEventListener('keyup', updateContinuationHint);
  ta.addEventListener('keydown', e=>{ if(e.key==='Enter' && (e.ctrlKey||e.metaKey)){ e.preventDefault(); doEval(); } else { handleEditorKeys(e, ta); }});
    ta.addEventListener('keydown', handleHistoryNav);
    inputEditorsHost.appendChild(ta);
    const tabObj = {id,name,el:tabEl,taEl:ta,history:[],histIndex:0};
    inputTabs.push(tabObj);
    loadHistory(tabObj);
  if(inputTabs.length===1){ activateInputTab(id); }
  persistTabs();
  return tabObj;
  }
  function activateInputTab(id){
    const tab = inputTabs.find(t=> t.id===id) || null;
    if(!tab) return;
    inputTabs.forEach(t=>{
      const isActive = t===tab;
      t.el.classList.toggle('active', isActive);
      t.el.setAttribute('aria-selected', isActive? 'true':'false');
      t.el.setAttribute('tabindex', isActive? '0':'-1');
      t.taEl.classList.toggle('active', isActive);
    });
  activeInput = tab; exprEl = tab.taEl; exprEl.focus();
  updateContinuationHint();
  persistTabs();
  }
  function closeInputTab(id){
    if(inputTabs.length===1) return;
    const idx = inputTabs.findIndex(t=> t.id===id);
    if(idx<0) return;
    const tab = inputTabs[idx];
    tab.el.remove(); tab.taEl.remove();
    inputTabs.splice(idx,1);
  if(activeInput && activeInput.id===id){ const newIdx = Math.max(0, idx-1); activateInputTab(inputTabs[newIdx].id); }
  persistTabs();
  }
  const addInputBtnRef = document.createElement('button');
  addInputBtnRef.type='button'; addInputBtnRef.className='secondary tiny'; addInputBtnRef.title='New input tab';
  addInputBtnRef.innerHTML = '<svg class="icon" aria-hidden><use href="#i-plus" xlink:href="#i-plus"/></svg><span class="sr-only">Add</span>';
  addInputBtnRef.addEventListener('click', ()=>{ createInputTab('Input '+(inputTabs.length+1)); });
  inputTabsHost.appendChild(addInputBtnRef);
  (function initInputs(){
    const orig = document.getElementById('expr');
    const saved = (function(){ try{ return JSON.parse(localStorage.getItem(TABS_STORE_KEY)||'null'); } catch { return null; } })();
    if(saved && Array.isArray(saved?.tabs) && saved.tabs.length){
      saved.tabs.forEach((t,i)=>{
        const tab = createInputTab(t?.name || ('Input '+(i+1)));
        if(typeof t?.content === 'string'){ tab.taEl.value = t.content; }
      });
      if(Number.isInteger(saved.active) && saved.active>=0 && saved.active<inputTabs.length){ activateInputTab(inputTabs[saved.active].id); }
    } else {
      const first = createInputTab('Input 1');
      if(orig && orig.value){ first.taEl.value = orig.value; }
    }
    if(orig) orig.remove();
  })();
  // No examples dropdown anymore

  function persistTabs(){
    try{
      const data = { active: inputTabs.findIndex(t=> t===activeInput), tabs: inputTabs.map(t=> ({ name: t.name, content: t.taEl.value })) };
      localStorage.setItem(TABS_STORE_KEY, JSON.stringify(data));
    }catch{}
  }

  // Output tabs
  const outputTabsEl = document.getElementById('outputTabs');
  const outputPanesEl = document.getElementById('outputPanes');
  let tabs = []; // {id,name,preEl}
  let activeTab = null;
  const OUT_TABS_STORE_KEY = 'lambdaOutputTabs_v1';
  let searchHits = []; let searchIndex = -1;
  let searchInput, searchPrev, searchNext, searchClear, searchFloat, sfPrev, sfNext, sfClear, searchCount;

  function createTab(name){
    const id = 'tab_'+Date.now().toString(36)+Math.random().toString(36).slice(2,7);
  const pre = document.createElement('pre');
  pre.className='output-pane'; pre.setAttribute('aria-live','polite'); pre.setAttribute('role','tabpanel'); pre.dataset.tabId = id;
    outputPanesEl.appendChild(pre);
  const tabEl = document.createElement('div'); tabEl.className='tab'; tabEl.id = id + '-tab'; tabEl.setAttribute('role','tab'); tabEl.setAttribute('aria-selected','false'); tabEl.setAttribute('tabindex','-1'); tabEl.textContent = name; tabEl.dataset.tabId = id;
  pre.setAttribute('aria-labelledby', tabEl.id);
    if(tabs.length>0){
      const close = document.createElement('span'); close.textContent='×'; close.className='close'; close.title='Close tab';
      close.addEventListener('click', (e)=>{ e.stopPropagation(); closeTab(id); }); tabEl.appendChild(close);
    }
    // Rename on double-click
    tabEl.title = 'Double-click to rename';
    tabEl.addEventListener('dblclick', ()=>{
      const labelNode = tabEl.childNodes[0];
      const currentLabel = (labelNode && labelNode.nodeType===Node.TEXT_NODE) ? labelNode.nodeValue : name;
      const next = prompt('Rename tab', currentLabel || '');
      if(next && next.trim()){
        const trimmed = next.trim();
        if(labelNode && labelNode.nodeType===Node.TEXT_NODE) labelNode.nodeValue = trimmed;
        const t = tabs.find(t=> t.id===id); if(t){ t.name = trimmed; persistOutputTabs(); }
      }
    });
    tabEl.addEventListener('click', ()=> activateTab(id));
    if(typeof addTabBtnRef === 'undefined' || !addTabBtnRef || !addTabBtnRef.parentNode){
      outputTabsEl.appendChild(tabEl);
    } else {
      outputTabsEl.insertBefore(tabEl, addTabBtnRef);
    }
  const tabObj = {id,name,el:tabEl,preEl:pre};
    tabs.push(tabObj);
    activateTab(id);
  // Attach DnD reordering
  attachOutputTabDnD(tabEl, id);
  persistOutputTabs();
    return tabObj;
  }
  function activateTab(id){
  tabs.forEach(t=>{ const isActive = (t.id===id); t.el.classList.toggle('active', isActive); t.preEl.classList.toggle('active', isActive); t.el.setAttribute('aria-selected', isActive? 'true':'false'); t.el.setAttribute('tabindex', isActive? '0':'-1'); });
    activeTab = tabs.find(t=>t.id===id) || null;
    clearSearch();
    applyLineFilters();
  persistOutputTabs();
  }
  function closeTab(id){
    if(tabs.length===1) return;
    const idx = tabs.findIndex(t=> t.id===id); if(idx<0) return;
    const tab = tabs[idx]; tab.el.remove(); tab.preEl.remove(); tabs.splice(idx,1);
    if(activeTab && activeTab.id===id){ const newIdx = Math.max(0, idx-1); activateTab(tabs[newIdx].id); }
    persistOutputTabs();
  }
  const addTabBtnRef = document.createElement('button');
  addTabBtnRef.id='addTabBtn'; addTabBtnRef.type='button'; addTabBtnRef.className='secondary'; addTabBtnRef.title='New output tab';
  addTabBtnRef.innerHTML = '<svg class="icon" aria-hidden><use href="#i-plus" xlink:href="#i-plus"/></svg><span class="sr-only">Add</span>';
  addTabBtnRef.addEventListener('click', ()=>{ createTab('Session '+(tabs.length+1)); });
  outputTabsEl.appendChild(addTabBtnRef);
  // Initialize output tabs (restore from localStorage if available)
  (function initOutputTabs(){
    const saved = (function(){ try{ return JSON.parse(localStorage.getItem(OUT_TABS_STORE_KEY)||'null'); } catch { return null; } })();
    if(saved && Array.isArray(saved?.tabs) && saved.tabs.length){
      // Recreate in saved order
      saved.tabs.forEach(t=>{ createTab(t.name || 'Session'); });
      if(Number.isInteger(saved.active) && saved.active>=0 && saved.active<tabs.length){ activateTab(tabs[saved.active].id); }
    } else {
      createTab('Session 1');
    }
  })();
  function getOutEl(){ return activeTab? activeTab.preEl : null; }
  function clearActiveOutput(){ const o=getOutEl(); if(o) o.textContent=''; resetTestStats(); }
  function currentOutEl(){ return getOutEl(); }
  function getCurrentLogText(){ const el = currentOutEl(); if(!el) return ''; let out=''; el.childNodes.forEach(n=>{ out += (n.textContent||''); }); return out; }

  function persistOutputTabs(){
    try{
      const data = {
        active: tabs.findIndex(t=> t===activeTab),
        tabs: tabs.map(t=> ({ name: t.name }))
      };
      localStorage.setItem(OUT_TABS_STORE_KEY, JSON.stringify(data));
    }catch{}
  }

  // Test indicator
  const testIndicatorEl = document.getElementById('testIndicator');
  const testStats = { passed:0, total:0 };
  function updateTestIndicator(){
    if(!testIndicatorEl) return;
    const pct = testStats.total ? ((testStats.passed / testStats.total) * 100).toFixed(2) : '0';
    testIndicatorEl.textContent = `Tests: ${testStats.passed}/${testStats.total} (${pct}%)`;
  testIndicatorEl.classList.remove('ti-good','ti-warn');
  if(testStats.total === 0){ /* default */ }
  else if(testStats.passed === testStats.total){ testIndicatorEl.classList.add('ti-good'); }
  else { testIndicatorEl.classList.add('ti-warn'); }
  }
  function resetTestStats(){ testStats.passed=0; testStats.total=0; updateTestIndicator(); }
  updateTestIndicator();

  // Buttons / inputs
  const runBtn = document.getElementById('runBtn');
  const clearBtn = document.getElementById('clearBtn');
  const clearInputBtn = document.getElementById('clearInputBtn');
  const streamToggle = document.getElementById('streamToggle');
  const wsToggle = document.getElementById('wsToggle');
  const loadBtn = document.getElementById('loadBtn');
  const localLoadBtn = document.getElementById('localLoadBtn');
  const localFileInput = document.getElementById('localFile');
  const filePathEl = document.getElementById('filePath');
  const fileStatus = document.getElementById('fileStatus');
  const fileProgress = document.getElementById('fileProgress');
  const fileProgressBar = document.getElementById('fileProgressBar');
  const copyLogBtn = document.getElementById('copyLogBtn');
  const downloadLogBtn = document.getElementById('downloadLogBtn');
  const toastHost = document.getElementById('toastHost');
  const helpOverlay = document.getElementById('helpOverlay');
  const helpClose = document.getElementById('helpClose');
  const settingsBtn = document.getElementById('settingsBtn');
  const settingsOverlay = document.getElementById('settingsOverlay');
  const settingsClose = document.getElementById('settingsClose');
  const settingsClearStorage = document.getElementById('settingsClearStorage');
  const editorFontSize = document.getElementById('editorFontSize');
  const outputFontSize = document.getElementById('outputFontSize');
  const defMaxLines = document.getElementById('defMaxLines');
  const defStreaming = document.getElementById('defStreaming');
  const defWS = document.getElementById('defWS');
  const settingsReset = document.getElementById('settingsReset');
  const defWrapOutput = document.getElementById('defWrapOutput');
  const defLineNumbers = document.getElementById('defLineNumbers');
  const defShowNormalized = document.getElementById('defShowNormalized');
  const defUseMonaco = document.getElementById('defUseMonaco');
  let showNormalized = true;

  const SETTINGS_KEY = 'lambdaUISettings_v1';
  function loadSettings(){
    try{ return JSON.parse(localStorage.getItem(SETTINGS_KEY)||'{}'); } catch { return {}; }
  }
  function saveSettings(s){ try { localStorage.setItem(SETTINGS_KEY, JSON.stringify(s||{})); } catch {}
  }
  function applySettings(s){
    const body = document.body;
    // font sizes via classes
    const efs = Math.min(22, Math.max(12, parseInt(s.editorFontSize||15,10)));
    const ofs = Math.min(20, Math.max(11, parseInt(s.outputFontSize||14,10)));
    (body.className||'').split(/\s+/).forEach(c=>{ if(/^efs-\d+$/.test(c) || /^ofs-\d+$/.test(c)) body.classList.remove(c); });
    body.classList.add(`efs-${efs}`);
    body.classList.add(`ofs-${ofs}`);
    if(Number.isInteger(s.maxLines)){ maxLogLines = s.maxLines; if(maxLinesInput){ maxLinesInput.value = String(s.maxLines); } }
    if(typeof s.streaming==='boolean'){ streaming = s.streaming; streamToggle.dataset.on = streaming? '1':'0'; streamToggle.textContent = 'Streaming: ' + (streaming?'On':'Off'); }
  if(typeof s.useWS==='boolean'){ useWS = s.useWS; wsToggle.dataset.on = useWS? '1':'0'; wsToggle.textContent = 'WS: ' + (useWS?'On':'Off'); }
  body.classList.toggle('wrap-output', s.wrapOutput !== false);
  body.classList.toggle('line-numbers', !!s.lineNumbers);
  if(typeof s.showNormalized === 'boolean') showNormalized = s.showNormalized; else showNormalized = true;
  // Monaco visibility toggle
  const monacoHost = document.getElementById('monacoHost');
  const useMonaco = s.useMonaco !== false; // default true
  if(monacoHost){ monacoHost.classList.toggle('hidden', !useMonaco); }
  }
  function initSettingsUI(){
    const s = loadSettings();
    // Defaults
  const ef = Number.isInteger(s.editorFontSize)? s.editorFontSize : 15;
  const of = Number.isInteger(s.outputFontSize)? s.outputFontSize : 14;
    const ml = Number.isInteger(s.maxLines)? s.maxLines : (parseInt(maxLinesInput?.value,10)||4000);
    const st = typeof s.streaming==='boolean'? s.streaming : false;
    const uw = typeof s.useWS==='boolean'? s.useWS : false;
    const wo = typeof s.wrapOutput==='boolean'? s.wrapOutput : true;
  const ln = typeof s.lineNumbers==='boolean'? s.lineNumbers : false;
  const sn = typeof s.showNormalized==='boolean'? s.showNormalized : true;
  const um = typeof s.useMonaco==='boolean'? s.useMonaco : true;
  applySettings({ editorFontSize:ef, outputFontSize:of, maxLines:ml, streaming:st, useWS:uw, wrapOutput:wo, lineNumbers:ln, showNormalized:sn, useMonaco:um });
    if(editorFontSize) editorFontSize.value = String(ef);
    if(outputFontSize) outputFontSize.value = String(of);
    if(defMaxLines) defMaxLines.value = String(ml);
    if(defStreaming) defStreaming.checked = !!st;
  if(defWS) defWS.checked = !!uw;
    if(defWrapOutput) defWrapOutput.checked = !!wo;
  if(defLineNumbers) defLineNumbers.checked = !!ln;
  if(defShowNormalized) defShowNormalized.checked = !!sn;
  }
  function openSettings(){ settingsOverlay?.classList.remove('hidden'); }
  function closeSettings(){ settingsOverlay?.classList.add('hidden'); }
  settingsBtn?.addEventListener('click', openSettings);
  settingsClose?.addEventListener('click', closeSettings);
  settingsOverlay?.addEventListener('click', (e)=>{ if(e.target===settingsOverlay) closeSettings(); });
  // Bind controls
  editorFontSize?.addEventListener('input', ()=>{ const s=loadSettings(); s.editorFontSize = parseInt(editorFontSize.value,10); saveSettings(s); applySettings(s); });
  outputFontSize?.addEventListener('input', ()=>{ const s=loadSettings(); s.outputFontSize = parseInt(outputFontSize.value,10); saveSettings(s); applySettings(s); });
  defMaxLines?.addEventListener('change', ()=>{ const v=parseInt(defMaxLines.value,10); if(!isNaN(v)&&v>0){ const s=loadSettings(); s.maxLines=v; saveSettings(s); applySettings(s); showToast('Max lines saved'); }});
  defStreaming?.addEventListener('change', ()=>{ const s=loadSettings(); s.streaming = !!defStreaming.checked; saveSettings(s); applySettings(s); showToast('Streaming default updated'); });
  defWS?.addEventListener('change', ()=>{ const s=loadSettings(); s.useWS = !!defWS.checked; saveSettings(s); applySettings(s); showToast('WS default updated'); });
  defWrapOutput?.addEventListener('change', ()=>{ const s=loadSettings(); s.wrapOutput = !!defWrapOutput.checked; saveSettings(s); applySettings(s); });
  defLineNumbers?.addEventListener('change', ()=>{ const s=loadSettings(); s.lineNumbers = !!defLineNumbers.checked; saveSettings(s); applySettings(s); });
  defShowNormalized?.addEventListener('change', ()=>{ const s=loadSettings(); s.showNormalized = !!defShowNormalized.checked; saveSettings(s); applySettings(s); });
  defUseMonaco?.addEventListener('change', ()=>{ const s=loadSettings(); s.useMonaco = !!defUseMonaco.checked; saveSettings(s); applySettings(s); });
  settingsReset?.addEventListener('click', ()=>{
  const defaults = { editorFontSize:15, outputFontSize:14, maxLines:4000, streaming:false, useWS:false, wrapOutput:true, lineNumbers:false, showNormalized:true, useMonaco:true };
    saveSettings(defaults);
    applySettings(defaults);
    if(editorFontSize) editorFontSize.value = String(defaults.editorFontSize);
    if(outputFontSize) outputFontSize.value = String(defaults.outputFontSize);
    if(defMaxLines) defMaxLines.value = String(defaults.maxLines);
    if(defStreaming) defStreaming.checked = defaults.streaming;
    if(defWS) defWS.checked = defaults.useWS;
    if(defWrapOutput) defWrapOutput.checked = defaults.wrapOutput;
  if(defLineNumbers) defLineNumbers.checked = defaults.lineNumbers;
  if(defShowNormalized) defShowNormalized.checked = defaults.showNormalized;
  if(defUseMonaco) defUseMonaco.checked = defaults.useMonaco;
    showToast('Settings reset');
  });

  // Clear local storage option
  settingsClearStorage?.addEventListener('click', ()=>{
    if(!confirm('This will clear UI tabs, history, settings, and theme from local storage. Continue?')) return;
    try {
      // Known keys
      const keys = [
        'lambdaInputTabs_v1',
        'lambdaOutputTabs_v1',
        'lambdaUISettings_v1',
  'lambdaTheme'
      ];
      // History keys: lambdaHistoryTab_*
      for(let i=0;i<localStorage.length;i++){
        const k = localStorage.key(i);
        if(!k) continue;
        if(k.startsWith('lambdaHistoryTab_')) keys.push(k);
      }
      // Remove all collected keys
      keys.forEach(k=>{ try{ localStorage.removeItem(k); }catch{} });
      // Reset in-memory state to defaults
      try{
        applySettings({ editorFontSize:15, outputFontSize:14, maxLines:4000, streaming:false, useWS:false, wrapOutput:true, lineNumbers:false, showNormalized:true });
        if(editorFontSize) editorFontSize.value = '15';
        if(outputFontSize) outputFontSize.value = '14';
        if(defMaxLines) defMaxLines.value = '4000';
        if(defStreaming) defStreaming.checked = false;
        if(defWS) defWS.checked = false;
        if(defWrapOutput) defWrapOutput.checked = true;
        if(defLineNumbers) defLineNumbers.checked = false;
        if(defShowNormalized) defShowNormalized.checked = true;
        // Reset theme select to Auto and re-init theme
        const themeSel = document.getElementById('themeSel');
        if(themeSel){ themeSel.value = 'auto'; }
        document.body.setAttribute('data-theme','auto');
      } catch {}
      showToast('Local storage cleared');
    } catch(e){ showToast('Failed to clear storage: '+ e.message, 'error'); }
  });

  // Toasts
  function showToast(msg, kind){
    if(!toastHost) return; const div=document.createElement('div'); div.className='toast'+(kind?(' '+kind):''); div.textContent=msg; toastHost.appendChild(div);
    setTimeout(()=>{ div.remove(); }, 2500);
  }

  // Search refs
  searchInput = document.getElementById('searchInput');
  searchPrev = document.getElementById('searchPrev');
  searchNext = document.getElementById('searchNext');
  searchClear = document.getElementById('searchClear');
  searchFloat = document.getElementById('searchFloat');
  sfPrev = document.getElementById('sfPrev');
  sfNext = document.getElementById('sfNext');
  sfClear = document.getElementById('sfClear');
  searchCount = document.getElementById('searchCount');

  // Log handling
  const maxLinesInput = document.getElementById('maxLinesInput');
  let maxLogLines = parseInt(maxLinesInput?.value,10) || 4000;
  if(maxLinesInput){
    maxLinesInput.addEventListener('change', ()=>{
      const v = parseInt(maxLinesInput.value,10);
      if(!isNaN(v) && v>0){
        maxLogLines = v;
        tabs.forEach(t=>{ const el = t.preEl; if(!el) return; while(el.childNodes.length > maxLogLines){ el.removeChild(el.firstChild); } });
      }
    });
  }
  function classify(line){
    const s = line.trimStart();
    if(s.startsWith('PROGRESS::')) return 'log-progress';
    if(s.startsWith('Error:')) return 'log-error';
    if(s.startsWith('#')) return 'log-comment';
    if(s.startsWith('Norm:')) return 'log-norm';
    if(s.startsWith('->')) return 'log-result';
    if(s.startsWith('Step')) return 'log-step';
    if(s.startsWith('Time:')) return 'log-time';
    if(s.startsWith('Name:')) return 'log-name';
    if(s.startsWith('Eval:')) return 'log-eval';
    if(s.startsWith('Print ')) return 'log-print';
    if(s.startsWith('Macro')) return 'log-macro';
    if(s.startsWith('Test:')) { return s.includes('passed') ? 'log-alpha-pass' : s.includes('failed') ? 'log-alpha-fail' : 'log-alpha'; }
    if(s.startsWith('Processing:')) return 'log-processing';
    if(s.startsWith(':')) return 'log-command';
    if(s.includes('Loading')) return 'log-loading';
    if(s.includes('<<')) return 'log-fileline';
    if(s.includes('>>')) return 'log-fileresult';
    return '';
  }
  function decodeUnicodeEscapes(str){
    if(!str || str.indexOf('\\u') === -1) return str;
    return str.replace(/\\u([0-9a-fA-F]{4})/g, (m, hex)=>{ try { return String.fromCharCode(parseInt(hex,16)); } catch { return m; } });
  }
  // ---- Batched log appending (DOM batching for performance) ----
  const logBuffer = []; // elements waiting to flush
  let flushScheduled = false;
  const MAX_BATCH = 120; // flush sooner if many lines
  // Segment tracking between 'line <num>: <<:' markers
  let currentSegmentId = 0;
  const segmentMeta = new Map(); // id -> { failed:boolean }
  function startNewSegment(){ currentSegmentId++; segmentMeta.set(currentSegmentId, { failed:false }); }
  function flushLogs(){
    flushScheduled = false;
    if(!logBuffer.length) return;
    const outEl = currentOutEl(); if(!outEl){ logBuffer.length=0; return; }
    const frag = document.createDocumentFragment();
    for(const el of logBuffer){ frag.appendChild(el); }
    logBuffer.length = 0;
    outEl.appendChild(frag);
    // Trim if exceeding max
    if(outEl.childNodes.length > maxLogLines){ while(outEl.childNodes.length > maxLogLines){ outEl.removeChild(outEl.firstChild); } }
    outEl.scrollTop = outEl.scrollHeight;
  persistOutputTabs();
  }
  function scheduleFlush(){
    if(!flushScheduled){ flushScheduled = true; requestAnimationFrame(flushLogs); }
  }
  function makeSpanLine(decoded, cls){
    const span = document.createElement('span');
  if(cls) span.className = cls;
  span.classList.add('log-line');
    span.textContent = decoded + '\n';
    if(isFilteredSpan(span)) span.classList.add('hidden-log');
    return span;
  }
  function append(text){
    const outEl = currentOutEl(); if(!outEl) return;
    const decoded = decodeUnicodeEscapes(text);
    const cls = classify(decoded);
    const isMarker = /^line\s+\d+\s*/i.test(decoded) && decoded.includes('<<:');
  if(isMarker){ startNewSegment(); }
    if(decoded.startsWith('Test:') && (/(?:\bpassed\b|\bfailed\b)/.test(decoded))){
      testStats.total++;
      if(/\bpassed\b/.test(decoded)) testStats.passed++;
      updateTestIndicator();
    }
    if(cls === 'log-alpha-fail' && currentSegmentId){ const meta = segmentMeta.get(currentSegmentId); if(meta) meta.failed = true; }
    if(!decoded.trimStart().startsWith('#') && decoded.includes('#')){
      const hashIdx = decoded.indexOf('#');
      if(hashIdx > 0){
        const before = decoded.slice(0, hashIdx).replace(/\s+$/,'');
        const comment = decoded.slice(hashIdx);
  const container = document.createElement('span'); if(cls) container.className = cls; container.classList.add('log-line');
        const beforeNode = document.createTextNode(before + ' ');
        const commentSpan = document.createElement('span'); commentSpan.className = 'log-comment'; commentSpan.textContent = comment;
        container.appendChild(beforeNode); container.appendChild(commentSpan); container.appendChild(document.createTextNode('\n'));
        if(isFilteredSpan(container)) container.classList.add('hidden-log');
        if(currentSegmentId) container.dataset.segment = currentSegmentId;
        logBuffer.push(container);
      } else {
        const el = makeSpanLine(decoded, cls); if(currentSegmentId) el.dataset.segment = currentSegmentId; else el.dataset.segment = '0'; logBuffer.push(el);
      }
    } else {
      const el = makeSpanLine(decoded, cls); if(currentSegmentId) el.dataset.segment = currentSegmentId; else el.dataset.segment = '0'; logBuffer.push(el);
    }
    if(logBuffer.length >= MAX_BATCH) flushLogs(); else scheduleFlush();
  }
  // Append logs where an item may contain multiple lines; accepts string or array
  function appendLogs(logs){
    if(logs == null) return;
    if(Array.isArray(logs)){
      for(const l of logs) appendLogs(l);
      return;
    }
    const s = String(logs);
    // Split on CRLF/ LF and preserve blank lines
    const parts = s.split(/\r?\n/);
    for(const part of parts){ append(part); }
  }
  function appendUserInputLine(raw){
    const outEl = currentOutEl(); if(!outEl) return;
    if(!raw || raw.trimStart().startsWith('#')){ append(raw); return; }
  const container = document.createElement('span');
  container.classList.add('log-line');
    const idx = raw.indexOf('#');
    if(idx >= 0){
      const before = raw.slice(0, idx).replace(/\s+$/,'');
      const comment = raw.slice(idx);
      container.textContent = '> ' + before + ' ';
      const commentSpan = document.createElement('span'); commentSpan.className='log-comment'; commentSpan.textContent = comment; container.appendChild(commentSpan); container.appendChild(document.createTextNode('\n'));
    } else { container.textContent = '> ' + raw + '\n'; }
    logBuffer.push(container);
    if(logBuffer.length >= MAX_BATCH) flushLogs(); else scheduleFlush();
  }

  // Scroll nav
  const scrollTopBtn = document.getElementById('scrollTopBtn');
  const scrollBottomBtn = document.getElementById('scrollBottomBtn');
  function scrollToTop(){ window.scrollTo({top:0, behavior:'smooth'}); }
  function scrollToBottom(){ window.scrollTo({top:document.documentElement.scrollHeight, behavior:'smooth'}); }
  scrollTopBtn.addEventListener('click', scrollToTop);
  scrollBottomBtn.addEventListener('click', scrollToBottom);
  function updateScrollNav(){
    const y = window.scrollY || document.documentElement.scrollTop;
    const max = document.documentElement.scrollHeight - window.innerHeight;
    scrollTopBtn.style.display = y > 40 ? 'inline-block' : 'none';
    scrollBottomBtn.style.display = (max - y) > 40 ? 'inline-block' : 'none';
  }
  window.addEventListener('scroll', updateScrollNav, {passive:true});
  window.addEventListener('resize', updateScrollNav); updateScrollNav();

  // Streaming (SSE / WS)
  let es=null, ws=null; let streaming=false, useWS=false;
  function handleProgress(pct){ fileProgress.style.display='block'; fileProgressBar.style.width=pct+'%'; if(pct>=100) setTimeout(()=>{ fileProgress.style.display='none'; fileProgressBar.style.width='0%'; },1200); }
  function ensureStream(){
    if(!streaming) return;
    if(useWS){
      if(ws && ws.readyState===WebSocket.OPEN) return;
      if(ws) { try{ ws.close(); }catch{} ws=null; }
  ws = new WebSocket(__cacheBust((location.protocol==='https:'?'wss':'ws')+'://'+location.host+'/ws?rid='+encodeURIComponent(__guid())));
  ws.onmessage = ev => { const d = ev.data; if(typeof d==='string'){ if(d.startsWith('PROGRESS::')) handleProgress(parseInt(d.split('::')[1]||'0',10)); else appendLogs(d);} };
      ws.onclose = ()=>{ if(streaming && useWS) setTimeout(ensureStream, 1200); };
      ws.onerror = ()=>{ try{ ws.close(); }catch{} if(streaming && useWS) setTimeout(ensureStream,1200); };
    } else {
      if(es) return;
  es = new EventSource(__cacheBust('/api/stream?rid='+encodeURIComponent(__guid())));
  es.onmessage = e => { if(e.data && e.data!=='.'){ if(e.data.startsWith('PROGRESS::')) handleProgress(parseInt(e.data.split('::')[1]||'0',10)); else appendLogs(e.data); } };
      es.onerror = ()=>{ es.close(); es=null; if(streaming && !useWS) setTimeout(ensureStream,1500); };
    }
  }
  streamToggle.addEventListener('click',()=>{
    streaming = !streaming;
    streamToggle.dataset.on = streaming ? '1':'0';
    streamToggle.textContent = 'Streaming: ' + (streaming?'On':'Off');
    if(streaming){ ensureStream(); append('[streaming enabled via '+(useWS?'WebSocket':'SSE')+']'); }
    else { if(es){ es.close(); es=null; } if(ws){ ws.close(); ws=null; } append('[streaming disabled]'); }
  });
  wsToggle.addEventListener('click',()=>{
    useWS = !useWS; wsToggle.dataset.on = useWS ? '1':'0'; wsToggle.textContent = 'WS: ' + (useWS?'On':'Off');
    if(streaming){ if(es){ es.close(); es=null; } if(ws){ try{ ws.close(); }catch{} ws=null; } ensureStream(); append('[switched to '+(useWS?'WebSocket':'SSE')+']'); }
  });

  // Evaluation

  async function doEval(){
    const rawLines = exprEl.value.split(/\r?\n/);
    const merged = []; let buffer = '';
    for(let i=0;i<rawLines.length;i++){
      let line = rawLines[i];
      const continued = /(^|[^\\])\\$/.test(line);
      if(continued){ line = line.replace(/\\$/,''); buffer += line; continue; }
      else { buffer += line; const finalLine = buffer.trim(); if(finalLine.length) merged.push(finalLine); buffer=''; }
    }
    if(buffer.trim().length){ merged.push(buffer.trim()); }
    const lines = merged.filter(l=>l.length>0);
    if(lines.length === 0) return;
    runBtn.disabled = true;
    try {
      for(const raw of lines) {
        // Inline spinner line (placeholder) inserted before evaluation logs
        const outEl = currentOutEl();
        let spinEl = null;
        if(outEl){
          spinEl = document.createElement('span');
          spinEl.className='log-processing';
          spinEl.innerHTML = '<span class="inline-spinner" aria-hidden="true"></span>Evaluating: '+ raw + '\n';
          outEl.appendChild(spinEl); outEl.scrollTop = outEl.scrollHeight;
        }
        performance.mark('eval-start');
        const res = await evalExpr(raw);
        performance.mark('eval-end');
  try { performance.measure('eval','eval-start','eval-end'); performance.clearMarks('eval-start'); performance.clearMarks('eval-end'); } catch{}
  if(spinEl){ spinEl.remove(); }
  appendUserInputLine(raw);
        if(!streaming){
          if(Array.isArray(res.logs) && res.logs.length) appendLogs(res.logs);
          const primary = res.output || res.normalized || '(no output)';
          const logsArray = Array.isArray(res.logs) ? res.logs : [];
            const hasResultLine = logsArray.some(l=> typeof l==='string' && (l.startsWith('-> ') || l.includes('\n-> ')));
          if(!hasResultLine && (!logsArray.includes(primary))) append(primary);
          let appendedNorm = false;
          if(showNormalized && res.normalized && res.normalized !== primary){
            const alreadyLogged = logsArray.some(l=> l.trim() === res.normalized.trim() || l.trim() === ('Norm: '+res.normalized).trim());
            if(!alreadyLogged){ append('Norm: ' + res.normalized); appendedNorm = true; }
          }
          if(appendedNorm) append('');
        } else {
          const primary = res.output || res.normalized || '(no output)';
          append(primary); let appendedNorm=false;
          if(showNormalized && res.normalized && res.normalized !== primary){ append(res.normalized); appendedNorm=true; }
          if(appendedNorm) append('');
        }
      }
    } catch(e){ append(`ERR: ${e.message}`); }
    finally { runBtn.disabled=false; }
    try { lines.forEach(l=> pushHistory(l)); } catch {}
  }

  function updateContinuationHint(){
    const v = exprEl.value; const lines = v.split(/\r?\n/); const last = lines[lines.length-1];
    const active = /(^|[^\\])\\$/.test(last || '');
    document.getElementById('contHint').classList.toggle('active', active);
  }
  updateContinuationHint();
  runBtn.addEventListener('click', doEval);
  // Quick examples insert
  document.querySelectorAll('.examples-list .example').forEach(li=>{
    li.addEventListener('click', ()=>{
      if(!exprEl) return;
      const v = (li.textContent||'').trim();
      exprEl.value = exprEl.value ? (exprEl.value.replace(/\s*$/,'') + '\n' + v) : v;
      exprEl.focus();
      updateContinuationHint();
    });
  });
  // Copy/Download current log
  copyLogBtn?.addEventListener('click', async ()=>{
  try { await navigator.clipboard.writeText(getCurrentLogText()); showToast('Log copied to clipboard','good'); } catch { showToast('Copy failed','bad'); }
  });
  downloadLogBtn?.addEventListener('click', ()=>{
    const blob = new Blob([getCurrentLogText()], {type:'text/plain'});
    const a = document.createElement('a'); a.href = URL.createObjectURL(blob); a.download = 'lambda-log.txt'; a.click(); setTimeout(()=> URL.revokeObjectURL(a.href), 3000);
  showToast('Log downloaded','good');
  });

  // Search
  function clearSearch(){ searchHits.forEach(el=> el.classList.remove('search-hit','search-current')); searchHits=[]; searchIndex=-1; updateSearchFloat(); }
  function performSearch(){
    clearSearch();
    const term = searchInput.value.trim(); if(!term){ updateSearchFloat(); restoreSearchFilter(); return; }
    const regexMode = !!document.getElementById('searchRegex')?.checked;
    let rx; try { rx = regexMode ? new RegExp(term,'i') : new RegExp(term.replace(/[.*+?^${}()|[\]\\]/g,'\\$&'),'i'); } catch { return; }
    const outEl = currentOutEl();
    if(outEl) outEl.querySelectorAll('span').forEach(s=>{ if(rx.test(s.textContent)){ s.classList.add('search-hit'); searchHits.push(s); } });
    if(document.getElementById('searchFilter')?.checked){ applySearchFilter(); }
    if(searchHits.length){ searchIndex=0; focusSearch(); }
    updateSearchFloat();
  }
  function applySearchFilter(){
    const outEl = currentOutEl(); if(!outEl) return;
    const keep = new Set(searchHits);
    outEl.querySelectorAll('span').forEach(s=>{ if(!keep.has(s) && !s.classList.contains('search-current')) s.classList.add('search-filter-hidden'); });
  }
  function restoreSearchFilter(){
    document.querySelectorAll('.search-filter-hidden').forEach(el=> el.classList.remove('search-filter-hidden'));
  }
  function focusSearch(){ searchHits.forEach(el=> el.classList.remove('search-current')); if(searchIndex>=0 && searchIndex<searchHits.length){ const el = searchHits[searchIndex]; el.classList.add('search-current'); el.scrollIntoView({block:'center'}); } updateSearchFloat(); }
  function updateSearchFloat(){ if(!searchFloat) return; const termActive = !!(searchInput && searchInput.value.trim().length); if(termActive){ searchFloat.style.display='flex'; if(searchHits.length){ searchCount.textContent = (searchIndex>=0? (searchIndex+1):0) + '/' + searchHits.length; } else { searchCount.textContent = '0/0'; } searchFloat.setAttribute('aria-hidden','false'); } else { searchFloat.style.display='none'; searchFloat.setAttribute('aria-hidden','true'); } }
  searchInput?.addEventListener('keydown', e=>{ if(e.key==='Enter'){ performSearch(); } });
  function nextHit(){ if(!searchHits.length) performSearch(); else { searchIndex=(searchIndex+1)%searchHits.length; focusSearch(); } }
  function prevHit(){ if(!searchHits.length) performSearch(); else { searchIndex=(searchIndex-1+searchHits.length)%searchHits.length; focusSearch(); } }
  searchNext?.addEventListener('click', nextHit);
  searchPrev?.addEventListener('click', prevHit);
  searchClear?.addEventListener('click',()=>{ searchInput.value=''; clearSearch(); });
  document.getElementById('searchRegex')?.addEventListener('change', ()=> performSearch());
  document.getElementById('searchFilter')?.addEventListener('change', ()=>{ if(!searchInput.value.trim()){ restoreSearchFilter(); return; } if(document.getElementById('searchFilter').checked){ performSearch(); } else { restoreSearchFilter(); } });
  sfNext?.addEventListener('click', nextHit);
  sfPrev?.addEventListener('click', prevHit);
  sfClear?.addEventListener('click',()=>{ searchInput.value=''; clearSearch(); });
  document.addEventListener('keydown', e=>{
    const active = document.activeElement; const inEditable = active && (active.tagName==='INPUT' || active.tagName==='TEXTAREA');
    if(e.key==='/' && !inEditable){ e.preventDefault(); searchInput.focus(); }
    else if(e.key==='F3'){ e.preventDefault(); if(e.shiftKey) prevHit(); else nextHit(); }
    else if(e.key==='Enter' && active===searchInput){ e.preventDefault(); nextHit(); }
    else if((e.key==='n' || e.key==='N') && !inEditable){ e.preventDefault(); e.key==='n'? nextHit(): prevHit(); }
    else if(e.key==='Escape'){ if(searchHits.length){ clearSearch(); } }
    else if((e.ctrlKey||e.metaKey) && !inEditable && (e.key==='l' || e.key==='L')){ e.preventDefault(); clearActiveOutput(); showToast('Output cleared'); }
    else if(e.key==='?' && !inEditable){ e.preventDefault(); if(helpOverlay){ helpOverlay.classList.remove('hidden'); } }
  });
  helpClose?.addEventListener('click', ()=> helpOverlay?.classList.add('hidden'));
  helpOverlay?.addEventListener('click', (e)=>{ if(e.target===helpOverlay) helpOverlay.classList.add('hidden'); });
  if(searchInput) searchInput.placeholder = 'Search (/ F3 n/N)';
  // Search highlight styles are defined in app.css; no JS style injection to comply with CSP.

  // Initialize settings after DOM controls exist
  initSettingsUI();

  // ---- Drag-and-drop for output tabs ----
  function attachOutputTabDnD(tabEl, id){
    tabEl.setAttribute('draggable','true');
    tabEl.addEventListener('dragstart', e=>{ tabEl.classList.add('dragging'); e.dataTransfer.effectAllowed='move'; e.dataTransfer.setData('text/plain', id); e.dataTransfer.setData('text/out-tab','1'); });
    tabEl.addEventListener('dragend', ()=> tabEl.classList.remove('dragging'));
    tabEl.addEventListener('dragover', e=>{ if(e.dataTransfer?.getData('text/out-tab')==='1'){ e.preventDefault(); tabEl.classList.add('drag-over'); } });
    tabEl.addEventListener('dragleave', ()=> tabEl.classList.remove('drag-over'));
    tabEl.addEventListener('drop', e=>{ const srcId = e.dataTransfer?.getData('text/plain'); if(!srcId || srcId===id) return; e.preventDefault(); tabEl.classList.remove('drag-over'); reorderOutputTabs(srcId, id); });
  }

  // createTab already attaches DnD; wrapper removed

  // Reorder helpers
  function reorderArrayById(arr, srcId, dstId){
    const sIdx = arr.findIndex(x=> x.id===srcId); const dIdx = arr.findIndex(x=> x.id===dstId); if(sIdx<0||dIdx<0||sIdx===dIdx) return arr;
    const [item] = arr.splice(sIdx,1); arr.splice(dIdx,0,item); return arr;
  }
  function reorderInputTabs(srcId, dstId){
    const src = inputTabs.find(t=> t.id===srcId); const dst = inputTabs.find(t=> t.id===dstId); if(!src||!dst) return;
    // DOM order for tabs
    inputTabsHost.insertBefore(src.el, dst.el);
    // DOM order for editors
    inputEditorsHost.insertBefore(src.taEl, dst.taEl);
    // Array order
    reorderArrayById(inputTabs, srcId, dstId);
    persistTabs();
  }
  function reorderOutputTabs(srcId, dstId){
    const src = tabs.find(t=> t.id===srcId); const dst = tabs.find(t=> t.id===dstId); if(!src||!dst) return;
    outputTabsEl.insertBefore(src.el, dst.el);
    outputPanesEl.insertBefore(src.preEl, dst.preEl);
    reorderArrayById(tabs, srcId, dstId);
  persistOutputTabs();
  }

  // ---- Editor helpers (indentation, bracket auto-close) ----
  function handleEditorKeys(e, ta){
    // Tab / Shift+Tab
    if(e.key==='Tab'){
      e.preventDefault();
      const start = ta.selectionStart, end = ta.selectionEnd; const v = ta.value; const sel = v.slice(start,end);
      const indent = '  ';
      if(start!==end && sel.includes('\n')){
        // Indent multiple lines
        const before = v.slice(0,start), mid = v.slice(start,end), after = v.slice(end);
        const indented = mid.replace(/(^|\n)/g, '$1'+indent);
        ta.value = before + indented + after;
        ta.selectionStart = start + indent.length; ta.selectionEnd = end + indented.length - mid.length; // expanded selection
      } else if(e.shiftKey){
        // Unindent current line
        const lineStart = v.lastIndexOf('\n', start-1)+1;
        if(v.slice(lineStart).startsWith(indent)){
          ta.value = v.slice(0,lineStart) + v.slice(lineStart+indent.length);
          ta.selectionStart = ta.selectionEnd = start - indent.length;
        }
      } else {
        // Insert indent
        ta.value = v.slice(0,start) + indent + v.slice(end);
        const pos = start + indent.length; ta.selectionStart = ta.selectionEnd = pos;
      }
      persistTabs(); updateContinuationHint(); return;
    }
    // Enter auto-indent
    if(e.key==='Enter' && !e.shiftKey && !e.ctrlKey && !e.metaKey){
      const pos = ta.selectionStart; const v = ta.value; const lineStart = v.lastIndexOf('\n', pos-1)+1; const m = /^\s*/.exec(v.slice(lineStart,pos)); const pad = m? m[0]:'';
      setTimeout(()=>{ const p = ta.selectionStart; ta.value = ta.value.slice(0,p) + pad + ta.value.slice(p); ta.selectionStart = ta.selectionEnd = p + pad.length; persistTabs(); updateContinuationHint(); }, 0);
      return;
    }
    // Bracket auto-close for simple cases
    const pairs = { '(':')', '[':']', '{':'}', '"':'"', "'":"'" };
    if(pairs[e.key] && !e.ctrlKey && !e.metaKey){
      e.preventDefault();
      const start = ta.selectionStart, end = ta.selectionEnd; const v = ta.value; const close = pairs[e.key];
      ta.value = v.slice(0,start) + e.key + close + v.slice(end);
      ta.selectionStart = ta.selectionEnd = start + 1;
      persistTabs(); updateContinuationHint(); return;
    }
  }

  // Filters
  const FILTER_CONFIG = [
    {label:'Macro', classes:['log-macro']},
    {label:'Test', classes:['log-alpha','log-alpha-pass','log-alpha-fail']},
    {label:'Time', classes:['log-time']},
    {label:'Step', classes:['log-step']},
    {label:'Eval', classes:['log-eval']},
    {label:'Print', classes:['log-print']},
    {label:'Norm', classes:['log-norm']},
    {label:'Name', classes:['log-name']},
    {label:'Processing', classes:['log-processing']}
  ];
  let disabledGroups = new Set();
  let showOnlyFailedSegments = false; // 'Failed' filter toggle
  function isFilteredSpan(el){
    for(const g of FILTER_CONFIG){ if(disabledGroups.has(g.label)){ for(const cls of g.classes){ if(el.classList.contains(cls)) return true; } } }
    return false;
  }
  function renderFilters(){
    const host=document.getElementById('filters'); if(!host) return; host.innerHTML='';
    FILTER_CONFIG.forEach(g=>{ const id='f_'+g.label.toLowerCase(); const lab=document.createElement('label'); const cb=document.createElement('input'); cb.type='checkbox'; cb.id=id; cb.checked=!disabledGroups.has(g.label); cb.addEventListener('change',()=>{ if(cb.checked) disabledGroups.delete(g.label); else disabledGroups.add(g.label); applyLineFilters(); }); lab.appendChild(cb); lab.appendChild(document.createTextNode(g.label)); host.appendChild(lab); });
    // Append 'Failed' filter
    const failedLab = document.createElement('label');
    const failedCb = document.createElement('input'); failedCb.type='checkbox'; failedCb.id='f_failed'; failedCb.checked=false;
    failedCb.addEventListener('change',()=>{ showOnlyFailedSegments = failedCb.checked; applyLineFilters(); });
    failedLab.appendChild(failedCb); failedLab.appendChild(document.createTextNode('Failed'));
    host.appendChild(failedLab);
  }
  function applyLineFilters(){
    const outEl = currentOutEl(); if(!outEl) return;
    outEl.querySelectorAll('span').forEach(s=>{
      let hide = isFilteredSpan(s);
      if(showOnlyFailedSegments){
        const segId = s.dataset.segment ? parseInt(s.dataset.segment,10) : null;
        const meta = segId ? segmentMeta.get(segId) : null;
        // Only show spans that belong to a segment with a failure
        if(!meta || !meta.failed){ hide = true; }
      }
      if(hide) s.classList.add('hidden-log'); else s.classList.remove('hidden-log');
    });
  }
  renderFilters();

  // History
  const HISTORY_KEY_PREFIX = 'lambdaHistoryTab_';
  function loadHistory(tab){ try { const k = HISTORY_KEY_PREFIX+tab.id; const arr = JSON.parse(localStorage.getItem(k)||'[]'); if(Array.isArray(arr)) { tab.history = arr; tab.histIndex = arr.length; } } catch { } }
  function saveHistory(tab){ try { localStorage.setItem(HISTORY_KEY_PREFIX+tab.id, JSON.stringify(tab.history.slice(-400))); } catch { } }
  function pushHistory(entry){ if(!activeInput) return; if(!entry || !entry.trim()) return; if(activeInput.history[activeInput.history.length-1] === entry) return; activeInput.history.push(entry); activeInput.histIndex = activeInput.history.length; saveHistory(activeInput); }
  function handleHistoryNav(e){ if(!activeInput) return; const ta = activeInput.taEl; if(e.key === 'ArrowUp' && ta.selectionStart===0 && ta.selectionEnd===0){ if(activeInput.histIndex > 0){ activeInput.histIndex--; ta.value = activeInput.history[activeInput.histIndex] || ''; updateContinuationHint(); e.preventDefault(); } } else if(e.key === 'ArrowDown' && ta.selectionStart===ta.value.length && ta.selectionEnd===ta.value.length){ if(activeInput.histIndex < activeInput.history.length-1){ activeInput.histIndex++; ta.value = activeInput.history[activeInput.histIndex] || ''; updateContinuationHint(); e.preventDefault(); } else if(activeInput.histIndex === activeInput.history.length-1){ activeInput.histIndex = activeInput.history.length; ta.value=''; updateContinuationHint(); e.preventDefault(); } } }

  clearBtn.addEventListener('click', ()=>{ clearActiveOutput(); });
  clearInputBtn.addEventListener('click', ()=>{ if(exprEl){ exprEl.value=''; updateContinuationHint(); exprEl.focus(); } });
  loadBtn.addEventListener('click', async ()=>{
    const p = filePathEl.value.trim(); if(!p) return;
    // Kick off: visible log breadcrumb and ensure streaming connection if enabled
    append(`[load start: ${p}]`);
    fileStatus.textContent='Loading...'; if(streaming){ fileProgress.style.display='block'; fileProgressBar.style.width='0%'; ensureStream(); }
    try {
      const r = await loadFile(p);
      fileStatus.textContent = 'Loaded: ' + r.message;
      // Always surface the load message in the output
      append(r.message || 'Load complete');
      if(!streaming && r.logs && Array.isArray(r.logs) && r.logs.length){ appendLogs(r.logs); }
      append('[load done]');
    } catch(e){ fileStatus.textContent = 'Error: ' + e.message; append(`Error: ${e.message}`); }
    finally { if(streaming){ setTimeout(()=>{ fileProgress.style.display='none'; fileProgressBar.style.width='0%'; }, 600); } }
  });

  localLoadBtn?.addEventListener('click', ()=> localFileInput?.click());
  localFileInput?.addEventListener('change', ()=>{ const f = localFileInput.files?.[0]; if(!f) return; loadLocalLambdaFile(f); localFileInput.value=''; });
  async function loadLocalLambdaFile(file){
    fileStatus.textContent = `Local: ${file.name} (reading...)`;
    try {
      const text = await file.text();
      const rawLines = text.split(/\r?\n/);
      const toEval = rawLines.map(l=>l.trim()).filter(l=> l.length && !l.startsWith('#'));
      if(!toEval.length){ fileStatus.textContent = `Local: ${file.name} (no expressions found)`; return; }
      append(`[local load start: ${file.name} expressions=${toEval.length}]`);
      fileProgress.style.display='block'; fileProgressBar.style.width='0%';
      for(let i=0;i<toEval.length;i++){
        const expr = toEval[i];
        try {
          performance.mark('eval-start');
          const res = await evalExpr(expr);
            performance.mark('eval-end');
            try { performance.measure('eval','eval-start','eval-end'); performance.clearMarks('eval-start'); performance.clearMarks('eval-end'); } catch{}
          appendUserInputLine(expr);
          if(!streaming){
            if(Array.isArray(res.logs) && res.logs.length) appendLogs(res.logs);
            const primary = res.output || res.normalized || '(no output)';
            const logsArray = Array.isArray(res.logs)? res.logs:[];
            const hasResultLine = logsArray.some(l=> typeof l==='string' && (l.startsWith('-> ') || l.includes('\n-> ')));
            if(!hasResultLine && (!logsArray.includes(primary))) append(primary);
            let appendedNorm = false;
            if(showNormalized && res.normalized && res.normalized !== primary){
              const alreadyLogged = logsArray.some(l=> l.trim() === res.normalized.trim() || l.trim() === ('Norm: '+res.normalized).trim());
              if(!alreadyLogged){ append('Norm: ' + res.normalized); appendedNorm = true; }
            }
            if(appendedNorm) append('');
          } else {
            const primary = res.output || res.normalized || '(no output)';
            append(primary); let appendedNorm=false; if(showNormalized && res.normalized && res.normalized !== primary){ append(res.normalized); appendedNorm=true; } if(appendedNorm) append('');
          }
        } catch(err){ append(`ERR: ${err.message}`); }
        const pct = Math.round(((i+1)/toEval.length)*100); fileProgressBar.style.width = pct + '%';
      }
      setTimeout(()=>{ fileProgress.style.display='none'; fileProgressBar.style.width='0%'; }, 900);
      fileStatus.textContent = `Local: ${file.name} loaded (${toEval.length} expressions)`; append(`[local load done: ${file.name}]`);
    } catch(e){ fileStatus.textContent = `Local load error: ${e.message}`; }
  }

  fetchJSON(`/api/health?rid=${encodeURIComponent(__guid())}`, { timeoutMs: 4000, headers:{'Cache-Control':'no-cache','Pragma':'no-cache','Expires':'0','X-Request-ID':__guid()} }).then(()=>{
    const h = document.getElementById('health'); if(h) h.textContent='ready';
  }).catch(()=>{
    const h = document.getElementById('health'); if(h) h.textContent='unreachable';
  });
  if(location.hash.includes('stream')) streamToggle.click();

  // Theme (moved)
  initTheme();
});

// (End of modularized app core)
