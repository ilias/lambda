// Main application script extracted from previous inline <script> in index.html
// Organized in sections; future refactors can split into multiple modules.

(function(){
  // ---- Cache busting helper ----
  window.__cacheBust = function(u){ try { const t = Date.now().toString(36)+Math.random().toString(36).slice(2); return u + (u.includes('?')?'&':'?') + '_='+ t; } catch { return u; } };

  // ---- Optional Service Worker registration (static asset caching only) ----
  const SW_ENABLED = true; // flip to false for debugging if needed
  if(SW_ENABLED && 'serviceWorker' in navigator){
    // Defer registration a tick so first paint isn't delayed
    window.addEventListener('load', ()=>{
      navigator.serviceWorker.getRegistration('sw.js').then(reg=>{
        if(!reg){ navigator.serviceWorker.register('sw.js').catch(()=>{}); }
      }).catch(()=>{});
    });
  }
})();

// ---- Central fetch helper with timeout & unified error handling ----
const DEFAULT_TIMEOUT_MS = 15000; // 15s default; adjust if needed
async function fetchJSON(url, { method='GET', headers={}, body=null, timeoutMs=DEFAULT_TIMEOUT_MS, retry=0 } = {}) {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  let attempt = 0; let lastErr;
  while (attempt <= retry) {
    try {
      const res = await fetch(__cacheBust(url), {
        method,
        headers,
        body,
        cache: 'no-store',
        signal: controller.signal
      });
      clearTimeout(timer);
      if (!res.ok) {
        const text = await res.text().catch(()=> '');
        throw new Error(`HTTP ${res.status}${text?`: ${text.slice(0,180)}`:''}`);
      }
      // Try JSON parse; surface friendly error if invalid
      try { return await res.json(); } catch(parseErr){ throw new Error('Invalid JSON response'); }
    } catch (err) {
      lastErr = err;
      if (controller.signal.aborted && err.name === 'AbortError') {
        // No retry on timeout unless explicitly requested by retry param
        if (attempt >= retry) break;
      } else if (attempt >= retry) {
        break;
      }
      attempt++;
      // Simple linear backoff (could be exponential if needed)
      await new Promise(r => setTimeout(r, 150 * attempt));
    }
  }
  throw lastErr || new Error('Request failed');
}

async function evalExpr(expr){
  return fetchJSON(`/api/eval?expr=${encodeURIComponent(expr)}`);
}
async function loadFile(p){
  return fetchJSON('/api/load', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', 'Cache-Control': 'no-cache', 'Pragma': 'no-cache' },
    body: JSON.stringify({ path: p })
  });
}

// Utility: debounce
function debounce(fn, ms){ let t; return (...a)=>{ clearTimeout(t); t=setTimeout(()=>fn(...a), ms); }; }

// Root initialization after DOM ready
window.addEventListener('DOMContentLoaded', () => {
  // Elements
  const inputTabsHost = document.getElementById('inputTabs');
  const inputEditorsHost = document.getElementById('inputEditors');
  let inputTabs = []; // {id,name,el,taEl,history:[],histIndex:number}
  let activeInput = null; // tab object
  let exprEl = null; // alias to active textarea for legacy code

  function createInputTab(name){
    const id = 'in_'+Date.now().toString(36)+Math.random().toString(36).slice(2,7);
    const tabEl = document.createElement('div');
    tabEl.className='tab';
    tabEl.textContent = name;
    tabEl.dataset.tabId = id;
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
    ta.rows=8;
    ta.placeholder=':help or succ 41 or let add = x,y -> x + y in add 2 3';
    ta.dataset.tabId = id;
    ta.addEventListener('input', updateContinuationHint);
    ta.addEventListener('keyup', updateContinuationHint);
    ta.addEventListener('keydown', e=>{ if(e.key==='Enter' && (e.ctrlKey||e.metaKey)){ e.preventDefault(); doEval(); }});
    ta.addEventListener('keydown', handleHistoryNav);
    inputEditorsHost.appendChild(ta);
    const tabObj = {id,name,el:tabEl,taEl:ta,history:[],histIndex:0};
    inputTabs.push(tabObj);
    loadHistory(tabObj);
    if(inputTabs.length===1){ activateInputTab(id); }
    return tabObj;
  }
  function activateInputTab(id){
    const tab = inputTabs.find(t=> t.id===id) || null;
    if(!tab) return;
    inputTabs.forEach(t=>{
      const isActive = t===tab;
      t.el.classList.toggle('active', isActive);
      t.taEl.classList.toggle('active', isActive);
    });
    activeInput = tab; exprEl = tab.taEl; exprEl.focus();
    updateContinuationHint();
  }
  function closeInputTab(id){
    if(inputTabs.length===1) return;
    const idx = inputTabs.findIndex(t=> t.id===id);
    if(idx<0) return;
    const tab = inputTabs[idx];
    tab.el.remove(); tab.taEl.remove();
    inputTabs.splice(idx,1);
    if(activeInput && activeInput.id===id){ const newIdx = Math.max(0, idx-1); activateInputTab(inputTabs[newIdx].id); }
  }
  const addInputBtnRef = document.createElement('button');
  addInputBtnRef.type='button'; addInputBtnRef.textContent='+'; addInputBtnRef.className='secondary'; addInputBtnRef.style.fontSize='.65rem'; addInputBtnRef.title='New input tab';
  addInputBtnRef.addEventListener('click', ()=>{ createInputTab('Input '+(inputTabs.length+1)); });
  inputTabsHost.appendChild(addInputBtnRef);
  (function initFirstInput(){
    const orig = document.getElementById('expr');
    const first = createInputTab('Input 1');
    if(orig && orig.value){ first.taEl.value = orig.value; }
    if(orig) orig.remove();
  })();

  // Output tabs
  const outputTabsEl = document.getElementById('outputTabs');
  const outputPanesEl = document.getElementById('outputPanes');
  let tabs = []; // {id,name,preEl}
  let activeTab = null;
  let searchHits = []; let searchIndex = -1;
  let searchInput, searchPrev, searchNext, searchClear, searchFloat, sfPrev, sfNext, sfClear, searchCount;

  function createTab(name){
    const id = 'tab_'+Date.now().toString(36)+Math.random().toString(36).slice(2,7);
    const pre = document.createElement('pre');
    pre.className='output-pane'; pre.setAttribute('aria-live','polite'); pre.dataset.tabId = id;
    outputPanesEl.appendChild(pre);
    const tabEl = document.createElement('div'); tabEl.className='tab'; tabEl.textContent = name; tabEl.dataset.tabId = id;
    if(tabs.length>0){
      const close = document.createElement('span'); close.textContent='×'; close.className='close'; close.title='Close tab';
      close.addEventListener('click', (e)=>{ e.stopPropagation(); closeTab(id); }); tabEl.appendChild(close);
    }
    tabEl.addEventListener('click', ()=> activateTab(id));
    if(typeof addTabBtnRef === 'undefined' || !addTabBtnRef || !addTabBtnRef.parentNode){
      outputTabsEl.appendChild(tabEl);
    } else {
      outputTabsEl.insertBefore(tabEl, addTabBtnRef);
    }
    const tabObj = {id,name,el:tabEl,preEl:pre};
    tabs.push(tabObj);
    activateTab(id);
    return tabObj;
  }
  function activateTab(id){
    tabs.forEach(t=>{ t.el.classList.toggle('active', t.id===id); t.preEl.classList.toggle('active', t.id===id); });
    activeTab = tabs.find(t=>t.id===id) || null;
    clearSearch();
    applyLineFilters();
  }
  function closeTab(id){
    if(tabs.length===1) return;
    const idx = tabs.findIndex(t=> t.id===id); if(idx<0) return;
    const tab = tabs[idx]; tab.el.remove(); tab.preEl.remove(); tabs.splice(idx,1);
    if(activeTab && activeTab.id===id){ const newIdx = Math.max(0, idx-1); activateTab(tabs[newIdx].id); }
  }
  const addTabBtnRef = document.createElement('button');
  addTabBtnRef.id='addTabBtn'; addTabBtnRef.type='button'; addTabBtnRef.className='secondary'; addTabBtnRef.textContent='+'; addTabBtnRef.title='New output tab';
  addTabBtnRef.addEventListener('click', ()=>{ createTab('Session '+(tabs.length+1)); });
  outputTabsEl.appendChild(addTabBtnRef);
  createTab('Session 1');
  function getOutEl(){ return activeTab? activeTab.preEl : null; }
  function clearActiveOutput(){ const o=getOutEl(); if(o) o.textContent=''; resetTestStats(); }
  function currentOutEl(){ return getOutEl(); }

  // Test indicator
  const testIndicatorEl = document.getElementById('testIndicator');
  const testStats = { passed:0, total:0 };
  function updateTestIndicator(){
    if(!testIndicatorEl) return;
    const pct = testStats.total ? ((testStats.passed / testStats.total) * 100).toFixed(2) : '0';
    testIndicatorEl.textContent = `Tests: ${testStats.passed}/${testStats.total} (${pct}%)`;
    if(testStats.total === 0){ testIndicatorEl.style.color=''; }
    else if(testStats.passed === testStats.total){ testIndicatorEl.style.color='var(--good)'; }
    else { testIndicatorEl.style.color='#e3b341'; }
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
  }
  function scheduleFlush(){
    if(!flushScheduled){ flushScheduled = true; requestAnimationFrame(flushLogs); }
  }
  function makeSpanLine(decoded, cls){
    const span = document.createElement('span');
    if(cls) span.className = cls;
    span.textContent = decoded + '\n';
    if(isFilteredSpan(span)) span.classList.add('hidden-log');
    return span;
  }
  function append(text){
    const outEl = currentOutEl(); if(!outEl) return;
    const decoded = decodeUnicodeEscapes(text);
    const cls = classify(decoded);
    if(decoded.startsWith('Test:') && (/(?:\bpassed\b|\bfailed\b)/.test(decoded))){
      testStats.total++;
      if(/\bpassed\b/.test(decoded)) testStats.passed++;
      updateTestIndicator();
    }
    if(!decoded.trimStart().startsWith('#') && decoded.includes('#')){
      const hashIdx = decoded.indexOf('#');
      if(hashIdx > 0){
        const before = decoded.slice(0, hashIdx).replace(/\s+$/,'');
        const comment = decoded.slice(hashIdx);
        const container = document.createElement('span'); if(cls) container.className = cls;
        const beforeNode = document.createTextNode(before + ' ');
        const commentSpan = document.createElement('span'); commentSpan.className = 'log-comment'; commentSpan.textContent = comment;
        container.appendChild(beforeNode); container.appendChild(commentSpan); container.appendChild(document.createTextNode('\n'));
        if(isFilteredSpan(container)) container.classList.add('hidden-log');
        logBuffer.push(container);
      } else {
        logBuffer.push(makeSpanLine(decoded, cls));
      }
    } else {
      logBuffer.push(makeSpanLine(decoded, cls));
    }
    if(logBuffer.length >= MAX_BATCH) flushLogs(); else scheduleFlush();
  }
  function appendUserInputLine(raw){
    const outEl = currentOutEl(); if(!outEl) return;
    if(!raw || raw.trimStart().startsWith('#')){ append(raw); return; }
    const container = document.createElement('span');
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
      ws = new WebSocket(__cacheBust((location.protocol==='https:'?'wss':'ws')+'://'+location.host+'/ws'));
      ws.onmessage = ev => { const d = ev.data; if(typeof d==='string'){ if(d.startsWith('PROGRESS::')) handleProgress(parseInt(d.split('::')[1]||'0',10)); else append(d);} };
      ws.onclose = ()=>{ if(streaming && useWS) setTimeout(ensureStream, 1200); };
      ws.onerror = ()=>{ try{ ws.close(); }catch{} if(streaming && useWS) setTimeout(ensureStream,1200); };
    } else {
      if(es) return;
      es = new EventSource(__cacheBust('/api/stream'));
      es.onmessage = e => { if(e.data && e.data!=='.'){ if(e.data.startsWith('PROGRESS::')) handleProgress(parseInt(e.data.split('::')[1]||'0',10)); else append(e.data); } };
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
  // ---- Performance metrics (latency) ----
  const perfStatsEl = document.getElementById('perfStats');
  let evalDurations = []; // recent durations
  function recordEvalDuration(ms){
    if(!isFinite(ms)) return;
    evalDurations.push(ms);
    if(evalDurations.length > 200) evalDurations.splice(0, evalDurations.length-200);
    const sorted = [...evalDurations].sort((a,b)=>a-b);
    const median = sorted[Math.floor(sorted.length/2)] || 0;
    const last = evalDurations[evalDurations.length-1] || 0;
    if(perfStatsEl){ perfStatsEl.textContent = `Latency ms (last/med): ${last.toFixed(1)} / ${median.toFixed(1)}`; }
  }

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
        performance.mark('eval-start');
        const res = await evalExpr(raw);
        performance.mark('eval-end');
        try { performance.measure('eval','eval-start','eval-end'); const entries = performance.getEntriesByName('eval'); const recent = entries[entries.length-1]; if(recent) recordEvalDuration(recent.duration); performance.clearMarks('eval-start'); performance.clearMarks('eval-end'); if(entries.length>50) performance.clearMeasures('eval'); } catch{}
        appendUserInputLine(raw);
        if(!streaming){
          if(Array.isArray(res.logs) && res.logs.length) res.logs.forEach(l=>append(l));
          const primary = res.output || res.normalized || '(no output)';
          const logsArray = Array.isArray(res.logs) ? res.logs : [];
            const hasResultLine = logsArray.some(l=> l.startsWith('-> '));
          if(!hasResultLine && (!logsArray.includes(primary))) append(primary);
          let appendedNorm = false;
          if(res.normalized && res.normalized !== primary){
            const alreadyLogged = logsArray.some(l=> l.trim() === res.normalized.trim() || l.trim() === ('Norm: '+res.normalized).trim());
            if(!alreadyLogged){ append('Norm: ' + res.normalized); appendedNorm = true; }
          }
          if(appendedNorm) append('');
        } else {
          const primary = res.output || res.normalized || '(no output)';
          append(primary); let appendedNorm=false;
          if(res.normalized && res.normalized !== primary){ append(res.normalized); appendedNorm=true; }
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

  // Search
  function clearSearch(){ searchHits.forEach(el=> el.classList.remove('search-hit','search-current')); searchHits=[]; searchIndex=-1; updateSearchFloat(); }
  function performSearch(){
    clearSearch();
    const term = searchInput.value.trim(); if(!term){ updateSearchFloat(); return; }
    let rx; try { rx = new RegExp(term.replace(/[.*+?^${}()|[\]\\]/g,'\\$&'),'i'); } catch { return; }
    const outEl = currentOutEl(); if(outEl) outEl.querySelectorAll('span').forEach(s=>{ if(rx.test(s.textContent)){ s.classList.add('search-hit'); searchHits.push(s); } });
    if(searchHits.length){ searchIndex=0; focusSearch(); }
    updateSearchFloat();
  }
  function focusSearch(){ searchHits.forEach(el=> el.classList.remove('search-current')); if(searchIndex>=0 && searchIndex<searchHits.length){ const el = searchHits[searchIndex]; el.classList.add('search-current'); el.scrollIntoView({block:'center'}); } updateSearchFloat(); }
  function updateSearchFloat(){ if(!searchFloat) return; const termActive = !!(searchInput && searchInput.value.trim().length); if(termActive){ searchFloat.style.display='flex'; if(searchHits.length){ searchCount.textContent = (searchIndex>=0? (searchIndex+1):0) + '/' + searchHits.length; } else { searchCount.textContent = '0/0'; } searchFloat.setAttribute('aria-hidden','false'); } else { searchFloat.style.display='none'; searchFloat.setAttribute('aria-hidden','true'); } }
  searchInput?.addEventListener('keydown', e=>{ if(e.key==='Enter'){ performSearch(); } });
  function nextHit(){ if(!searchHits.length) performSearch(); else { searchIndex=(searchIndex+1)%searchHits.length; focusSearch(); } }
  function prevHit(){ if(!searchHits.length) performSearch(); else { searchIndex=(searchIndex-1+searchHits.length)%searchHits.length; focusSearch(); } }
  searchNext?.addEventListener('click', nextHit);
  searchPrev?.addEventListener('click', prevHit);
  searchClear?.addEventListener('click',()=>{ searchInput.value=''; clearSearch(); });
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
  });
  if(searchInput) searchInput.placeholder = 'Search (/ F3 n/N)';
  // Search highlight styles now defined in app.css for theming; keep fallback minimal if CSS failed to load.
  (function(){
    if(!document.querySelector('style[data-search-style-fallback]')){
      const style=document.createElement('style');
      style.dataset.searchStyleFallback='1';
      style.textContent='.search-hit{background:rgba(88,166,255,.18);border-radius:3px;} .search-current{background:var(--accent)!important;color:#fff!important;outline:2px solid var(--accent);outline-offset:0;}';
      document.head.appendChild(style);
    }
  })();

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
  function isFilteredSpan(el){
    for(const g of FILTER_CONFIG){ if(disabledGroups.has(g.label)){ for(const cls of g.classes){ if(el.classList.contains(cls)) return true; } } }
    return false;
  }
  function renderFilters(){ const host=document.getElementById('filters'); if(!host) return; host.innerHTML=''; FILTER_CONFIG.forEach(g=>{ const id='f_'+g.label.toLowerCase(); const lab=document.createElement('label'); const cb=document.createElement('input'); cb.type='checkbox'; cb.id=id; cb.checked=!disabledGroups.has(g.label); cb.addEventListener('change',()=>{ if(cb.checked) disabledGroups.delete(g.label); else disabledGroups.add(g.label); applyLineFilters(); }); lab.appendChild(cb); lab.appendChild(document.createTextNode(g.label)); host.appendChild(lab); }); }
  function applyLineFilters(){ const outEl = currentOutEl(); if(!outEl) return; outEl.querySelectorAll('span').forEach(s=>{ if(isFilteredSpan(s)) s.classList.add('hidden-log'); else s.classList.remove('hidden-log'); }); }
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
    fileStatus.textContent='Loading...'; if(streaming){ fileProgress.style.display='block'; fileProgressBar.style.width='0%'; }
    try { const r = await loadFile(p); fileStatus.textContent = 'Loaded: ' + r.message; if(!streaming && r.logs && Array.isArray(r.logs) && r.logs.length){ r.logs.forEach(l=>append(l)); append(''); } } catch(e){ fileStatus.textContent = 'Error: ' + e.message; }
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
            try { performance.measure('eval','eval-start','eval-end'); const entries = performance.getEntriesByName('eval'); const recent = entries[entries.length-1]; if(recent) recordEvalDuration(recent.duration); performance.clearMarks('eval-start'); performance.clearMarks('eval-end'); if(entries.length>50) performance.clearMeasures('eval'); } catch{}
          appendUserInputLine(expr);
          if(!streaming){
            if(Array.isArray(res.logs) && res.logs.length) res.logs.forEach(l=>append(l));
            const primary = res.output || res.normalized || '(no output)';
            const logsArray = Array.isArray(res.logs)? res.logs:[];
            const hasResultLine = logsArray.some(l=> l.startsWith('-> '));
            if(!hasResultLine && (!logsArray.includes(primary))) append(primary);
            let appendedNorm = false;
            if(res.normalized && res.normalized !== primary){
              const alreadyLogged = logsArray.some(l=> l.trim() === res.normalized.trim() || l.trim() === ('Norm: '+res.normalized).trim());
              if(!alreadyLogged){ append('Norm: ' + res.normalized); appendedNorm = true; }
            }
            if(appendedNorm) append('');
          } else {
            const primary = res.output || res.normalized || '(no output)';
            append(primary); let appendedNorm=false; if(res.normalized && res.normalized !== primary){ append(res.normalized); appendedNorm=true; } if(appendedNorm) append('');
          }
        } catch(err){ append(`ERR: ${err.message}`); }
        const pct = Math.round(((i+1)/toEval.length)*100); fileProgressBar.style.width = pct + '%';
      }
      setTimeout(()=>{ fileProgress.style.display='none'; fileProgressBar.style.width='0%'; }, 900);
      fileStatus.textContent = `Local: ${file.name} loaded (${toEval.length} expressions)`; append(`[local load done: ${file.name}]`);
    } catch(e){ fileStatus.textContent = `Local load error: ${e.message}`; }
  }

  fetchJSON('/api/health', { timeoutMs: 4000 }).then(()=>{
    const h = document.getElementById('health'); if(h) h.textContent='ready';
  }).catch(()=>{
    const h = document.getElementById('health'); if(h) h.textContent='unreachable';
  });
  if(location.hash.includes('stream')) streamToggle.click();

  // Theme
  (function(){
    const sel = document.getElementById('themeSel'); const key='lambdaTheme';
    function apply(t){ document.body.setAttribute('data-theme', t); try{ localStorage.setItem(key,t);}catch{} }
    const saved = (function(){ try{return localStorage.getItem(key);}catch{return null;} })();
    if(saved && sel.querySelector(`option[value="${saved}"]`)){ sel.value=saved; apply(saved); }
    sel.addEventListener('change', ()=> apply(sel.value));
  })();
});
