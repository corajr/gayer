// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as Belt_MapString from "bs-platform/lib/es6/belt_MapString.js";

var component = ReasonReact.reducerComponent("SlitscanCanvas-Gayer");

function make(setRef, layerKey, layerRefs, sourceKey, width, height, saveTick, _) {
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
                            var match = param$1[/* state */1][/* canvasRef */0][0];
                            var match$1 = Belt_MapString.get(layerRefs[0], sourceKey);
                            if (match !== undefined && match$1 !== undefined) {
                              var canvas = Js_primitive.valFromOption(match);
                              var ctx = canvas.getContext("2d");
                              ctx.drawImage(Js_primitive.valFromOption(match$1), 320, 0, 1, 480, width - 1 | 0, 0, 1, height);
                              ctx.drawImage(canvas, -1, 0);
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
