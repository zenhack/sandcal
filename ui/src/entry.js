document.addEventListener('DOMContentLoaded', function() {
  var browserTz = new Intl.DateTimeFormat().resolvedOptions().timeZone;
  var elem = document.getElementById('bs-form');
  var template = elem.attributes['data-sandcal-template'].nodeValue;
  var now = new Date();
  var app = Elm.Main.init({
    node: elem,
    flags: {
      tpl: JSON.parse(template),
      browserTz: browserTz,
      now: {
        year: now.getFullYear(),
        month: now.getMonth(),
        day: now.getDate(),
      },
    },
  });
  app.ports.setLocation.subscribe(function(loc) {
    console.log("port setLocation: ", loc);
    window.location = loc;
  });
})
