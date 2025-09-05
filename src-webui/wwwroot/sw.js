// Simple service worker for static asset caching.
// Excludes any /api/ requests, WebSocket, SSE (EventSource) endpoints.
// Cache version bump when updating: change VERSION.
const VERSION = 'v1';
const STATIC_CACHE = 'lambda-static-' + VERSION;
const ASSETS = [
  './',
  'index.html',
  'app.css',
  'js/app.js'
];

self.addEventListener('install', evt => {
  evt.waitUntil(caches.open(STATIC_CACHE).then(c=> c.addAll(ASSETS).catch(()=>{})));
});
self.addEventListener('activate', evt => {
  evt.waitUntil(caches.keys().then(keys=> Promise.all(keys.filter(k=> k.startsWith('lambda-static-') && k!==STATIC_CACHE).map(k=> caches.delete(k)) )));
});

self.addEventListener('fetch', evt => {
  const url = new URL(evt.request.url);
  if(url.pathname.startsWith('/api/') || url.pathname === '/ws') return; // bypass dynamic endpoints
  if(evt.request.method !== 'GET') return;
  evt.respondWith(
    caches.match(evt.request).then(cached => {
      const fetchPromise = fetch(evt.request).then(res => {
        try {
          const copy = res.clone();
          if(copy.ok && copy.type === 'basic'){
            caches.open(STATIC_CACHE).then(c=> c.put(evt.request, copy).catch(()=>{}));
          }
        } catch {}
        return res;
      }).catch(()=> cached || Response.error());
      return cached || fetchPromise;
    })
  );
});
