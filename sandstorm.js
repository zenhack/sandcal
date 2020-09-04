(function() {
  if(document.cookie === "") {
    // If the timezone isn't set, set it and then refresh the page to get
    // data in the correct zone.
    var browserTz = new Intl.DateTimeFormat().resolvedOptions().timeZone;
    document.cookie = 'timezone=' + browserTz
    window.location = window.location;
  }
})();
document.addEventListener('DOMContentLoaded', function() {
  window.parent.postMessage({
    setPath: location.pathname + location.search + location.hash,
  }, '*')
  window.parent.postMessage({
    setTitle: document.title,
  }, '*')
})
