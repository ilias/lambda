// Networking helpers extracted from legacy app.js
import { __cacheBust, __guid } from './util.js';

const DEFAULT_TIMEOUT_MS = 15000;

export async function fetchJSON(url, { method='GET', headers={}, body=null, timeoutMs=DEFAULT_TIMEOUT_MS, retry=0 } = {}) {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  let attempt = 0; let lastErr;
  while (attempt <= retry) {
    try {
      const res = await fetch(__cacheBust(url), { method, headers, body, cache:'no-store', signal: controller.signal });
      clearTimeout(timer);
      if(!res.ok){
        const text = await res.text().catch(()=> '');
        throw new Error(`HTTP ${res.status}${text?`: ${text.slice(0,180)}`:''}`);
      }
      try { return await res.json(); } catch { throw new Error('Invalid JSON response'); }
    } catch(err){
      lastErr = err;
      if(controller.signal.aborted && err.name==='AbortError'){
        if(attempt >= retry) break;
      } else if(attempt >= retry){ break; }
      attempt++;
      await new Promise(r=> setTimeout(r, 150 * attempt));
    }
  }
  throw lastErr || new Error('Request failed');
}

export async function evalExpr(expr){
  const rid = __guid();
  return fetchJSON(`/api/eval?expr=${encodeURIComponent(expr)}&rid=${encodeURIComponent(rid)}`, {
    headers:{
      'Cache-Control':'no-cache, no-store, must-revalidate',
      'Pragma':'no-cache',
      'Expires':'0',
      'X-Request-ID': rid,
      'Accept':'application/json'
    }
  });
}

export async function loadFile(path){
  const rid = __guid();
  return fetchJSON(`/api/load?rid=${encodeURIComponent(rid)}`, {
    method:'POST',
    headers:{
      'Content-Type':'application/json',
      'Cache-Control':'no-cache, no-store, must-revalidate',
      'Pragma':'no-cache',
      'Expires':'0',
      'X-Request-ID': rid,
      'Accept':'application/json'
    },
    body: JSON.stringify({ path })
  });
}

// Backwards compatibility (existing code references)
window.fetchJSON = fetchJSON;
window.evalExpr = evalExpr;
window.loadFile = loadFile;
