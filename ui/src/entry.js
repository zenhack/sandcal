import { main } from './new_event.bs.js';

document.addEventListener('DOMContentLoaded', function() {
  var elem = document.getElementById('bs-form');
  var template = elem.attributes['data-sandcal-template'].nodeValue;
  main(template)(elem);
})
