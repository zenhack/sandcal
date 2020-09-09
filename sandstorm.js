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

  window.parent.postMessage({
    renderTemplate: {
      rpcId: '0',
      template: location.protocol + "//$API_HOST/.sandstorm-token/$API_TOKEN/export.ics",
      clipboardButton: 'left',
      roleAssignment: {roleId: 0}, // viewer
      forSharing: true,
    },
  }, '*');
})
window.addEventListener('message', function(event) {
  if(event.source != window.parent) {
    return;
  }

  console.log("Event", event)

  const exportIframe = document.getElementById('export-offer-iframe');
  if(exportIframe !== null) {
    exportIframe.src = event.data.uri;
  }
})
