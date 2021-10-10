document.addEventListener('DOMContentLoaded', function() {
  var browserTz = new Intl.DateTimeFormat().resolvedOptions().timeZone;
  var elem = document.getElementById('bs-form');
  var template = elem.attributes['data-sandcal-template'].nodeValue;
  Elm.Main.init({
    node: elem,
    flags: {
      tpl: template,
      browserTz: browserTz,
    },
  })
})
