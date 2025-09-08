// Utility helpers extracted from legacy app.js
// Provides: __cacheBust, __guid, debounce

export function __cacheBust(u){
  try {
    const t = Date.now().toString(36) + Math.random().toString(36).slice(2);
    return u + (u.includes('?') ? '&' : '?') + '_=' + t;
  } catch {
    return u;
  }
}

// RFC4122-ish v4 GUID (prefers crypto.randomUUID)
export function __guid(){
  try { if(window.crypto && crypto.randomUUID) return crypto.randomUUID(); } catch {}
  const t = Date.now().toString(16);
  const r = Math.random().toString(16).slice(2,10);
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
    .replace(/[xy]/g, c => {
      const n = (Math.random()*16)|0;
      const v = c === 'x' ? n : (n & 0x3 | 0x8);
      return v.toString(16);
    })
    .replace(/^(.{8})(.{4})(.{4})(.{4})(.{12}).*$/, '$1-$2-$3-$4-$5') + '-' + t + r;
}

export function debounce(fn, ms){
  let t; return (...a)=>{ clearTimeout(t); t=setTimeout(()=>fn(...a), ms); };
}

// Backwards compatibility for any legacy code expecting globals
window.__cacheBust = __cacheBust;
window.__guid = __guid;
