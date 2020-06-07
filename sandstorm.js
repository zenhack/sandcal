document.addEventListener('DOMContentLoaded', function() {
  window.parent.postMessage({
    setPath: location.pathname + location.hash,
  }, '*')
  window.parent.postMessage({
    setTitle: document.title,
  }, '*')
})
