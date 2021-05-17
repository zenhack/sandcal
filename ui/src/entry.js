import { main } from './new_event.bs.js';

window.postJsonWithCSRF = function(path, csrf, jsonAsString) {
  return fetch(path, {
    method: "POST",
    headers: {
      "X-CSRF-Token": csrf,
      "Content-Type": "application/json",
    },
    body: jsonAsString,
  })
}

document.addEventListener('DOMContentLoaded', function() {
  var browserTz = new Intl.DateTimeFormat().resolvedOptions().timeZone;
  var elem = document.getElementById('bs-form');
  var template = elem.attributes['data-sandcal-template'].nodeValue;
  console.log("template:");
  console.log(JSON.parse(template));
  console.log("browserTz = ", browserTz);
  main(template, browserTz)(elem);
})
