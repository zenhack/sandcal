import { main } from './new_event.bs.js';

document.addEventListener('DOMContentLoaded', function() {
  var browserTz = new Intl.DateTimeFormat().resolvedOptions().timeZone;
  var elem = document.getElementById('bs-form');
  var template = elem.attributes['data-sandcal-template'].nodeValue;
  console.log("template:");
  console.log(JSON.parse(template));
  console.log("browserTz = ", browserTz);
  main(template, browserTz)(elem);
})
