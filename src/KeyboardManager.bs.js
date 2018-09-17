// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as DocumentRe from "bs-webapi/src/dom/nodes/DocumentRe.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as Option$Rationale from "rationale/src/Option.js";

function addKeyDownListenerToBody(keyboardManagerState, keyboardEventCallback) {
  var maybeBody = Curry._2(Option$Rationale.Infix[/* >>= */0], DocumentRe.asHtmlDocument(document), (function (prim) {
          return Js_primitive.nullable_to_opt(prim.body);
        }));
  if (maybeBody !== undefined) {
    Js_primitive.valFromOption(maybeBody).addEventListener("keydown", keyboardEventCallback);
    keyboardManagerState[/* listener */0][0] = keyboardEventCallback;
    return /* () */0;
  } else {
    return /* () */0;
  }
}

function removeKeyDownListenerFromBody(keyboardManagerState) {
  var maybeBody = Curry._2(Option$Rationale.Infix[/* >>= */0], DocumentRe.asHtmlDocument(document), (function (prim) {
          return Js_primitive.nullable_to_opt(prim.body);
        }));
  var match = keyboardManagerState[/* listener */0][0];
  if (maybeBody !== undefined && match !== undefined) {
    Js_primitive.valFromOption(maybeBody).removeEventListener("keydown", match);
    return /* () */0;
  } else {
    return /* () */0;
  }
}

export {
  addKeyDownListenerToBody ,
  removeKeyDownListenerFromBody ,
  
}
/* DocumentRe Not a pure module */
