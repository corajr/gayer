// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Caml_array from "bs-platform/lib/es6/caml_array.js";

var defaultState = /* record */[/* notesOn */Caml_array.caml_make_vect(128, 0.0)];

function update(midiState, $$event) {
  if ($$event.tag) {
    return Caml_array.caml_array_set(midiState[/* notesOn */0], $$event[0][0], 0.0);
  } else {
    var match = $$event[0];
    return Caml_array.caml_array_set(midiState[/* notesOn */0], match[0], match[1]);
  }
}

export {
  defaultState ,
  update ,
  
}
/* No side effect */