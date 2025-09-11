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
  m.editor.defineTheme('lambda-dark', {
    base:'vs-dark', inherit:true, rules:[
      { token:'lambda', foreground:'d19a66' },
      { token:'keyword', foreground:'61afef' },
      { token:'number', foreground:'d19a66' },
      { token:'constant', foreground:'98c379' },
      { token:'meta', foreground:'c678dd' }
    ]
  });
}

async function lintNow(){
  const model = monacoEditor?.getModel(); if(!model) return;
  const text = model.getValue();
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
    theme: (document.body.getAttribute('data-theme')==='dark'? 'lambda-dark':'vs'),
    automaticLayout: true,
    wordWrap: 'on',
    minimap: { enabled:false },
    rulers: [100]
  });
  enableShortcuts();
  monacoEditor.onDidChangeModelContent(debounce(()=>{ syncToTextarea(); lintNow(); }, 250));
  lintNow();
}

function showMonaco(){
  const host = document.getElementById('monacoHost'); if(host) host.classList.remove('hidden');
}

function hideMonaco(){
  const host = document.getElementById('monacoHost'); if(host) host.classList.add('hidden');
}

function attachIntegration(){
  if(!useMonaco) return;
  // Toggle textarea/monaco visibility: we show both, but textarea remains source of truth for existing logic.
  showMonaco(); syncFromTextarea();
  // Re-sync when user switches input tabs
  const tabs = document.getElementById('inputTabs');
  if(tabs){ tabs.addEventListener('click', ()=> setTimeout(syncFromTextarea, 0)); }
  // Theme change
  const themeSel = document.getElementById('themeSel');
  themeSel?.addEventListener('change', ()=>{
    const t = document.body.getAttribute('data-theme');
    monacoApi?.editor?.setTheme(t==='dark'? 'lambda-dark':'vs');
  });
  // no examples
}

if(shouldUseMonaco()){
  // AMD loader present via loader.min.js
  // eslint-disable-next-line no-undef
  require.config({ paths: { 'vs': 'https://cdn.jsdelivr.net/npm/monaco-editor@0.49.0/min/vs' } });
  // eslint-disable-next-line no-undef
  require(['vs/editor/editor.main'], ()=>{ initMonaco(window.monaco); attachIntegration(); });
}
