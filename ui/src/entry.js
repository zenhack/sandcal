import { main } from './new_event.bs.js';

document.addEventListener('DOMContentLoaded', function() {
  var elem = document.getElementById('bs-form');
  //var formId = elem.attributes['data-sandcal-form-id'].nodeValue;
  var userTz = elem.attributes['data-sandcal-user-tz'].nodeValue;
  main(userTz)(elem);
})
