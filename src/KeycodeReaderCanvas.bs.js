// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Char from "bs-platform/lib/es6/char.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as $$String from "bs-platform/lib/es6/string.js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as KeycodeUtil$Gayer from "./KeycodeUtil.bs.js";

var component = ReasonReact.reducerComponent("KeycodeReaderCanvas-Gayer");

function make(layerKey, _, _$1, setRef, saveTick, currentFilterValues, writePos, $staropt$star, $staropt$star$1, $staropt$star$2, _$2) {
  var width = $staropt$star !== undefined ? $staropt$star : 240;
  var height = $staropt$star$1 !== undefined ? $staropt$star$1 : 240;
  var fontSize = $staropt$star$2 !== undefined ? $staropt$star$2 : 12;
  var setCanvasRef = function (theRef, param) {
    var maybeCanvas = (theRef == null) ? undefined : Js_primitive.some(theRef);
    param[/* state */1][/* canvasRef */0][0] = maybeCanvas;
    return Curry._1(setRef, theRef);
  };
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              return Curry._3(saveTick, self[/* onUnmount */4], layerKey, (function () {
                            var match = currentFilterValues[0];
                            var match$1 = self[/* state */1][/* canvasRef */0][0];
                            if (match !== undefined && match$1 !== undefined) {
                              var values = match[0];
                              var ctx = Js_primitive.valFromOption(match$1).getContext("2d");
                              ctx.fillStyle = Canvas$Gayer.rgba(0, 0, 0, 0.008);
                              ctx.fillRect(0, 0, width, height);
                              ctx.font = fontSize.toString() + "px monospace";
                              var n = values.length;
                              for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
                                var v = Caml_array.caml_array_get(values, i);
                                if (v > 0.1) {
                                  var keyCodeN = KeycodeUtil$Gayer.yToKeyCode(height, i);
                                  var s = $$String.make(1, Char.chr(keyCodeN));
                                  ctx.fillStyle = Canvas$Gayer.rgba(255, 255, 255, v);
                                  ctx.fillText(s, writePos[0], i);
                                }
                                
                              }
                              return /* () */0;
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
              return React.createElement("canvas", {
                          ref: Curry._1(self[/* handle */0], setCanvasRef),
                          height: height.toString(),
                          width: width.toString()
                        });
            }),
          /* initialState */(function () {
              return /* record */[/* canvasRef : record */[/* contents */undefined]];
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
