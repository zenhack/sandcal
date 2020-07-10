import { main } from './new_event.bs.js';

document.addEventListener('DOMContentLoaded', function() {
  var elem = document.getElementById('bs-form');
  var dataAttr = function(attrName) {
    return elem.attributes['data-sandcal-' + attrName].nodeValue;
  }
  main(
    dataAttr('user-tz'),
    dataAttr('action'),
    dataAttr('submit-text')
  )(elem);
})
