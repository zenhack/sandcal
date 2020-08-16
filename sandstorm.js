document.addEventListener('DOMContentLoaded', function() {
  window.parent.postMessage({
    setPath: location.pathname + location.search + location.hash,
  }, '*')
  window.parent.postMessage({
    setTitle: document.title,
  }, '*')
})
