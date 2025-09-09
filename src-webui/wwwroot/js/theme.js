// Theme handling extracted from legacy app.js
export function initTheme(){
  const sel = document.getElementById('themeSel');
  if(!sel) return;
  const key='lambdaTheme';
  const mql = window.matchMedia && window.matchMedia('(prefers-color-scheme: light)');
  function apply(t){
    document.body.setAttribute('data-theme', t);
    try{ localStorage.setItem(key,t);}catch{}
  }
  const saved = (function(){ try{return localStorage.getItem(key);}catch{return null;} })();
  if(saved && sel.querySelector(`option[value="${saved}"]`)){
    sel.value = saved; apply(saved);
  }
  sel.addEventListener('change', ()=> apply(sel.value));
  // Live update when system theme changes in Auto mode
  if(mql && mql.addEventListener){
    mql.addEventListener('change', ()=>{ if((localStorage.getItem(key)||'auto')==='auto'){ apply('auto'); } });
  }
}
