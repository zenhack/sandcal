import { main } from './new_event.bs.js';

document.addEventListener('DOMContentLoaded', function() {
  var elem = document.getElementById('bs-form');
  var userTz = elem.attributes['data-sandcal-user-tz'].nodeValue;
  var action = elem.attributes['data-sandcal-action'].nodeValue;
  main(userTz, action)(elem);
})
