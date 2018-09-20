// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as Color$Gayer from "./Color.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as Belt_MapString from "bs-platform/lib/es6/belt_MapString.js";
import * as ImageDataUtil$Gayer from "./ImageDataUtil.bs.js";

var component = ReasonReact.reducerComponent("HistogramCanvas");

function make(setRef, layerKey, layerRefs, saveTick, rootHeight, getReadAndWritePos, width, height, _) {
  var saveRef = function (aRef, param) {
    Curry._1(setRef, aRef);
    var maybeRef = (aRef == null) ? undefined : Js_primitive.some(aRef);
    param[/* state */1][/* canvasRef */0][0] = maybeRef;
    return /* () */0;
  };
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              return Curry._3(saveTick, self[/* onUnmount */4], layerKey, (function (param) {
                            var param$1 = self;
                            var match = Belt_MapString.get(layerRefs[0], "root");
                            var match$1 = param$1[/* state */1][/* canvasRef */0][0];
                            if (match !== undefined && match$1 !== undefined) {
                              var rootCtx = Js_primitive.valFromOption(match).getContext("2d");
                              var ctx = Js_primitive.valFromOption(match$1).getContext("2d");
                              var xToRead = /* record */[/* contents */0];
                              Curry._1(getReadAndWritePos, (function (toRead, _) {
                                      xToRead[0] = toRead;
                                      return /* () */0;
                                    }));
                              var slice = rootCtx.getImageData(xToRead[0], 0, 1, rootHeight);
                              var binFn = function (param) {
                                var match = Color$Gayer.rgbToHslFloat(param[/* r */0], param[/* g */1], param[/* b */2]);
                                var octave = match[2] * 9.0 | 0;
                                var offsetInOctave = match[0] * 11.0 | 0;
                                var bin = Caml_int32.imul(octave, 12) + offsetInOctave | 0;
                                return /* tuple */[
                                        bin,
                                        match[1]
                                      ];
                              };
                              var histogram = ImageDataUtil$Gayer.imageDataToHistogram(120, binFn, 10.0, slice);
                              var n = height / 120 | 0;
                              for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
                                var offset = Caml_int32.imul((n - i | 0) - 1 | 0, 120);
                                for(var j = 0; j <= 119; ++j){
                                  var h = j % 12 / 12.0;
                                  var s = Caml_array.caml_array_get(histogram, j);
                                  var l = (j / 10 | 0) / 10.0;
                                  var yPos = offset + ((120 - j | 0) - 1 | 0) | 0;
                                  var match$2 = s > 0.05;
                                  var color = Color$Gayer.hsl(undefined, h, s, match$2 ? l : 0.0);
                                  ctx.fillStyle = color;
                                  ctx.fillRect(0, yPos, 1, 1);
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
                          ref: Curry._1(self[/* handle */0], saveRef),
                          style: {
                            opacity: "0.0"
                          },
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
