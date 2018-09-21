// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Char from "bs-platform/lib/es6/char.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as $$String from "bs-platform/lib/es6/string.js";
import * as RotJs from "rot-js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";

var component = ReasonReact.reducerComponent("RotDisplayCanvas-Gayer");

function make(layerKey, _, setRef, saveTick, currentFilterValues, $staropt$star, $staropt$star$1, $staropt$star$2, _$1) {
  var width = $staropt$star !== undefined ? $staropt$star : 240;
  var height = $staropt$star$1 !== undefined ? $staropt$star$1 : 240;
  var fontSize = $staropt$star$2 !== undefined ? $staropt$star$2 : 8;
  var setUpRot = function (theRef, param) {
    var state = param[/* state */1];
    var maybeParent = (theRef == null) ? undefined : Js_primitive.some(theRef);
    state[/* parentElRef */1][0] = maybeParent;
    var match = state[/* canvasRef */0][0];
    if (match !== undefined && !(theRef == null)) {
      var canvas = Js_primitive.valFromOption(match);
      canvas.appendChild(theRef);
      return Curry._1(setRef, canvas);
    } else {
      return /* () */0;
    }
  };
  var textHeight = Caml_int32.div(height, fontSize);
  var textWidth = Caml_int32.div(width, fontSize);
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              var display = new RotJs.Display({
                    width: textWidth,
                    height: textHeight,
                    fontSize: 8,
                    forceSquareRatio: true
                  });
              self[/* state */1][/* rotDisplayRef */2][0] = Js_primitive.some(display);
              self[/* state */1][/* canvasRef */0][0] = Js_primitive.some(display.getContainer());
              Curry._3(saveTick, self[/* onUnmount */4], layerKey, (function () {
                      var match = currentFilterValues[0];
                      var match$1 = self[/* state */1][/* rotDisplayRef */2][0];
                      if (match.tag || match$1 === undefined) {
                        return /* () */0;
                      } else {
                        var display = Js_primitive.valFromOption(match$1);
                        var values = match[0];
                        var n = values.length;
                        var scale = textHeight / n;
                        for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
                          var v = Caml_array.caml_array_get(values, i);
                          if (v > 0.0) {
                            display.drawText(textWidth - 1 | 0, scale * i | 0, $$String.make(1, Char.chr(i)));
                          }
                          
                        }
                        return /* () */0;
                      }
                    }));
              return Curry._1(self[/* onUnmount */4], (function () {
                            var match = self[/* state */1][/* canvasRef */0][0];
                            var match$1 = self[/* state */1][/* parentElRef */1][0];
                            if (match !== undefined && match$1 !== undefined) {
                              try {
                                Js_primitive.valFromOption(match).removeChild(Js_primitive.valFromOption(match$1));
                                return /* () */0;
                              }
                              catch (exn){
                                return /* () */0;
                              }
                            } else {
                              return /* () */0;
                            }
                          }));
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              return React.createElement("div", {
                          ref: Curry._1(self[/* handle */0], setUpRot)
                        });
            }),
          /* initialState */(function () {
              return /* record */[
                      /* canvasRef : record */[/* contents */undefined],
                      /* parentElRef : record */[/* contents */undefined],
                      /* rotDisplayRef : record */[/* contents */undefined]
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (_, _$1) {
              return /* NoUpdate */0;
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  component ,
  make ,
  
}
/* component Not a pure module */
