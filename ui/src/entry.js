import { main } from './new_event.bs.js';

document.addEventListener('DOMContentLoaded', function() {
  var elem = document.getElementById('bs-form');
  var template = elem.attributes['data-sandcal-template'].nodeValue;
  console.log("template:");
  console.log(JSON.parse(template));
  main(template)(elem);
})
