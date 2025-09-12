// Monaco editor bootstrapping and custom language wiring
import { debounce } from './util.js';
import { fetchJSON } from './net.js';

function shouldUseMonaco(){
  try{
    const s = JSON.parse(localStorage.getItem('lambdaUISettings_v1')||'{}');
    return s.useMonaco !== false; // default true
  }catch{ return true; }
}
let monacoEditor = null;
let monacoApi = null;
let loaderConfigured = false;
let loaderReady = false;

function getActiveTextArea(){
  const active = document.querySelector('#inputEditors textarea.input-pane.active');
  return active || document.querySelector('#inputEditors textarea.input-pane');
}

function syncFromTextarea(){
  const ta = getActiveTextArea();
  if(!monacoEditor || !ta) return;
  monacoEditor.setValue(ta.value);
}

function syncToTextarea(){
  const ta = getActiveTextArea();
  if(!monacoEditor || !ta) return;
  ta.value = monacoEditor.getValue();
  ta.dispatchEvent(new Event('input'));
}

// examples removed

function enableShortcuts(){
  // Ctrl+Enter evaluate (delegate to app.js listener tied to textarea)
  if(!monacoEditor) return;
  monacoEditor.addCommand(monacoApi.KeyMod.CtrlCmd | monacoApi.KeyCode.Enter, ()=>{
    syncToTextarea();
    const evt = new KeyboardEvent('keydown', { key:'Enter', ctrlKey:true });
    const ta = getActiveTextArea(); if(ta) ta.dispatchEvent(evt);
  });
  // Shift+Alt+F format (basic: trim lines)
  monacoEditor.addAction({ id:'lambda-format', label:'Format', keybindings:[monacoApi.KeyMod.Shift | monacoApi.KeyMod.Alt | monacoApi.KeyCode.KeyF], run:()=>{
    const text = monacoEditor.getValue().split(/\r?\n/).map(l=> l.trimEnd()).join('\n');
    monacoEditor.setValue(text);
    syncToTextarea();
  }});
}

function registerLanguage(m){
  m.languages.register({ id:'lambda' });
  m.languages.setLanguageConfiguration('lambda', {
    brackets: [ ['(',')'], ['[',']'] ],
    autoClosingPairs: [ {open:'(', close:')'}, {open:'[', close:']'} ]
  });
  m.languages.setMonarchTokensProvider('lambda', {
    tokenizer: {
      root: [
        [/\s+/, 'white'],
        [/#[^\n]*/, 'comment'],
        [/λ|\\/, 'lambda'],
        [/\b(let|in|rec|def|Y)\b/, 'keyword'],
        [/\b(true|false|nil)\b/, 'constant'],
        [/\d+/, 'number'],
        [/\-\>|∘|\.|\|\>|\$|\,|\[|\]|\(|\)|\=|\;|\.\.|\.\./, 'operator'],
        [/:[a-zA-Z][\w-]*/, 'meta'],
        [/[_A-Za-z][\w]*/, 'identifier']
      ]
    }
  });
  // Use built-in 'vs' and 'vs-dark' themes to avoid runtime errors
}

// --- Dynamic Theme Support -------------------------------------------------
function buildDynamicThemeSpec(){
  const cs = getComputedStyle(document.body);
  const val = (n, fb) => { const v = cs.getPropertyValue(n).trim(); return v || fb; };
  const themeAttr = document.body.getAttribute('data-theme') || 'auto';
  const base = (themeAttr === 'light') ? 'vs' : (themeAttr === 'auto' ? ((()=>{ try { return window.matchMedia('(prefers-color-scheme: light)').matches ? 'vs' : 'vs-dark'; } catch { return 'vs-dark'; } })()) : 'vs-dark');
  const colorful = themeAttr === 'colorful';
  const bg = val('--color-surface-1', base==='vs' ? '#ffffff' : '#1e1e1e');
  const bgAlt = val('--color-surface-2', base==='vs' ? '#f3f3f3' : '#252526');
  const editorBg = val('--color-bg', bg);
  const fg = val('--color-fg', base==='vs' ? '#333333' : '#d4d4d4');
  const subtle = val('--color-fg-subtle', base==='vs' ? '#6a6a6a' : '#858585');
  const accent = val('--color-accent', base==='vs' ? '#0066bf' : '#569cd6');
  const border = val('--color-border', base==='vs' ? '#d4d4d4' : '#3c3c3c');
  const warn = val('--color-warn', '#dcdcaa');
  const good = val('--color-good', '#6a9955');
  const bad = val('--color-bad', '#f48771');
  // Current line highlight (more subtle & neutral, user-overridable via --editor-current-line-bg)
  let lineHL = val('--editor-current-line-bg','');
  if(!lineHL){
    if(colorful) lineHL = 'rgba(255,138,61,0.05)'; // softer tint
    else if(base==='vs') lineHL = 'rgba(0,0,0,0.06)'; // light theme subtle shadow
    else lineHL = 'rgba(255,255,255,0.06)'; // dark theme soft glow
  }
  const lineHLBorder = val('--editor-current-line-border','');
  const selBG = val('--editor-selection-bg', colorful ? 'rgba(255,138,61,0.22)' : (base==='vs' ? 'rgba(0,120,215,0.25)' : 'rgba(88,166,255,0.25)'));
  const toHex = c => c.startsWith('#') ? c.slice(1) : c;
  return {
    base,
    inherit: true,
    rules: [
      { token:'lambda', foreground: toHex(val('--editor-token-keyword', accent)) },
      { token:'keyword', foreground: toHex(val('--editor-token-keyword', accent)) },
      { token:'number', foreground: toHex(val('--editor-token-number', warn)) },
      { token:'constant', foreground: toHex(val('--editor-token-constant', good)) },
      { token:'meta', foreground: toHex(val('--editor-token-meta', bad)) }
    ],
    colors: {
      'editor.background': editorBg,
      'editor.foreground': fg,
      'editorLineNumber.foreground': subtle,
      'editorLineNumber.activeForeground': accent,
      'editorCursor.foreground': accent,
      'editor.selectionBackground': selBG,
      'editor.inactiveSelectionBackground': selBG,
  // Force removal of current line highlight & any residual line border
  'editor.lineHighlightBackground': '#00000000',
  'editor.lineHighlightBorder': '#00000000',
  // Also remove other secondary highlight backgrounds that can appear reddish
  'editor.selectionHighlightBackground': '#00000000',
  'editor.wordHighlightBackground': '#00000000',
  'editor.wordHighlightStrongBackground': '#00000000',
  'editor.rangeHighlightBackground': '#00000000',
      'editorBracketMatch.border': accent,
      'editorIndentGuide.background': border,
      'editorIndentGuide.activeBackground': accent,
      'editorWhitespace.foreground': subtle,
      'editorGutter.background': bg,
      'editorWidget.background': bgAlt,
      'editorHoverWidget.background': bgAlt,
      'editorSuggestWidget.background': bgAlt,
      'editorSuggestWidget.foreground': fg,
      'editorSuggestWidget.border': border,
      'editorSuggestWidget.selectedBackground': selBG,
      'editorError.foreground': bad,
      'editorWarning.foreground': warn,
      'editorInfo.foreground': accent
    }
  };
}

const DYNAMIC_THEME_NAME = 'lambda-dynamic';
function applyDynamicMonacoTheme(){
  if(!monacoApi) return;
  try {
    const spec = buildDynamicThemeSpec();
    monacoApi.editor.defineTheme(DYNAMIC_THEME_NAME, spec);
    monacoApi.editor.setTheme(DYNAMIC_THEME_NAME);
  } catch {
    // Fallback to built-ins if anything goes wrong
    try { monacoApi.editor.setTheme('vs-dark'); } catch {}
  }
}

async function lintNow(){
  const model = monacoEditor?.getModel(); if(!model) return;
  const text = model.getValue();
  if(!text || !text.trim()){
    try { monacoApi?.editor?.setModelMarkers(model, 'lambda-lint', []); } catch {}
    return;
  }
  try{
    const res = await fetchJSON(`/api/lint?expr=${encodeURIComponent(text)}`);
    const markers = (res?.diagnostics||[]).map(d=>({
      severity: monacoApi.MarkerSeverity.Error,
      message: d.message || 'Parse error',
      startLineNumber: d.range?.startLineNumber || 1,
      startColumn: d.range?.startColumn || 1,
      endLineNumber: d.range?.endLineNumber || 1,
      endColumn: d.range?.endColumn || 1
    }));
    monacoApi.editor.setModelMarkers(model, 'lambda-lint', markers);
  }catch{
    monacoApi.editor.setModelMarkers(model, 'lambda-lint', []);
  }
}

function initMonaco(m){
  monacoApi = m;
  registerLanguage(m);
  const container = document.getElementById('monacoEditor');
  if(!container) return;
  monacoEditor = m.editor.create(container, {
    value: getActiveTextArea()?.value || '',
    language:'lambda',
    theme: 'vs-dark', // temporary; replaced by dynamic apply below
    automaticLayout: true,
  readOnly: false,
    wordWrap: 'on',
  scrollBeyondLastLine: false,
    minimap: { enabled:false },
    rulers: [100]
  });
  applyDynamicMonacoTheme();
  enableShortcuts();
  monacoEditor.onDidChangeModelContent(debounce(()=>{ syncToTextarea(); lintNow(); }, 250));
  lintNow();
}

function showMonaco(){
  const host = document.getElementById('monacoHost'); if(host) host.classList.remove('hidden');
  // Ensure model gets current textarea content
  syncFromTextarea();
}

function hideMonaco(){
  const host = document.getElementById('monacoHost'); if(host) host.classList.add('hidden');
}

function attachIntegration(){
  // If host is hidden, skip optional UI wiring until enabled
  const host = document.getElementById('monacoHost');
  if(host && host.classList.contains('hidden')) return;
  // Toggle textarea/monaco visibility: we show both, but textarea remains source of truth for existing logic.
  showMonaco(); syncFromTextarea();
  // Re-sync when user switches input tabs
  const tabs = document.getElementById('inputTabs');
  if(tabs){
    tabs.addEventListener('click', ()=>{
      setTimeout(()=>{
        syncFromTextarea();
        try { monacoEditor?.layout(); } catch {}
      }, 0);
    });
  }
  // Theme change
  const themeSel = document.getElementById('themeSel');
    let themeUpdateScheduled = false;
    function scheduleThemeUpdate(){
      if(themeUpdateScheduled) return; themeUpdateScheduled = true;
      requestAnimationFrame(()=>{ themeUpdateScheduled = false; applyDynamicMonacoTheme(); });
    }
    themeSel?.addEventListener('change', scheduleThemeUpdate);
  // If user is in Auto theme, follow system changes
  try {
    const mql = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)');
    if(mql && mql.addEventListener){
      mql.addEventListener('change', ()=>{
      if((localStorage.getItem('lambdaTheme')||'auto')==='auto') scheduleThemeUpdate();
      });
    }
  } catch {}
    // Respond to potential custom events if theme module emits them later
    document.addEventListener('lambda-theme-changed', scheduleThemeUpdate);
    // In case CSS variables mutate after load (rare), run once more
    setTimeout(scheduleThemeUpdate, 50);
  // Click-to-focus safety
  host?.addEventListener('click', ()=>{ try{ monacoEditor?.focus(); }catch{} });
  // no examples
}

function bootLoaderAndCreate(){
  let amdRequire = (typeof window !== 'undefined') ? window.require : undefined;
  if(!amdRequire){
    // Try to dynamically load the AMD loader once
    const existing = document.getElementById('monaco-loader');
    if(!existing){
      const s = document.createElement('script');
      s.id = 'monaco-loader';
      s.src = 'https://cdn.jsdelivr.net/npm/monaco-editor@0.49.0/min/vs/loader.min.js';
      s.async = true;
      s.onload = ()=> setTimeout(bootLoaderAndCreate, 0);
      s.onerror = ()=> setTimeout(bootLoaderAndCreate, 200);
      document.head.appendChild(s);
    } else {
      setTimeout(bootLoaderAndCreate, 50);
    }
    return;
  }
  if(!loaderConfigured){
    try { amdRequire.config({ paths: { 'vs': 'https://cdn.jsdelivr.net/npm/monaco-editor@0.49.0/min/vs' } }); loaderConfigured = true; }
    catch{}
  }
  amdRequire(['vs/editor/editor.main'], ()=>{
    loaderReady = true;
    initMonaco(window.monaco);
    attachIntegration();
    try { monacoEditor?.layout(); monacoEditor?.focus(); } catch {}
  });
}

// Global toggle callable from settings UI
window.__monacoToggle = function(enable){
  if(enable){
    showMonaco();
    if(!monacoEditor) {
      bootLoaderAndCreate();
    } else {
      try { monacoEditor.layout(); monacoEditor.focus(); } catch {}
    }
  try { document.body.classList.add('use-monaco'); } catch {}
  } else {
    if(monacoEditor){ try{ monacoEditor.dispose(); }catch{} monacoEditor = null; }
    const host = document.getElementById('monacoHost'); if(host) host.classList.add('hidden');
  // Focus textarea for immediate editing
  const ta = getActiveTextArea(); if(ta) ta.focus();
  try { document.body.classList.remove('use-monaco'); } catch {}
  }
};

// Initial load based on saved setting
if(shouldUseMonaco()){
  bootLoaderAndCreate();
  // Relayout on resize if visible
  window.addEventListener('resize', ()=>{ if(monacoEditor && !document.getElementById('monacoHost')?.classList.contains('hidden')){ try{ monacoEditor.layout(); }catch{} } });
}
