// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as $$Set from "bs-platform/lib/es6/set.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Caml_obj from "bs-platform/lib/es6/caml_obj.js";

var compare = Caml_obj.caml_compare;

var PitchSet = $$Set.Make(/* module */[/* compare */compare]);

function filterByPitchSet(pitchClasses, filterValues) {
  return $$Array.mapi((function (i, v) {
                var match = Curry._2(PitchSet[/* mem */2], i % 12, pitchClasses);
                if (match) {
                  return v;
                } else {
                  return 0.0;
                }
              }), filterValues);
}

export {
  PitchSet ,
  filterByPitchSet ,
  
}
/* PitchSet Not a pure module */
