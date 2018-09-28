// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Belt_Option from "bs-platform/lib/es6/belt_Option.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as Belt_MapString from "bs-platform/lib/es6/belt_MapString.js";
import * as DrawCommand$Gayer from "./DrawCommand.bs.js";

var component = ReasonReact.reducerComponent("DrawCommandCanvas-Gayer");

function make(cmds, layerKey, layerRefs, setRef, saveTick, width, height, _) {
  var setCanvasRef = function (theRef, param) {
    var maybeCanvas = (theRef == null) ? undefined : Js_primitive.some(theRef);
    param[/* state */1][/* drawContext */0][/* maybeCtxRef */0][0] = Belt_Option.map(maybeCanvas, (function (x) {
            return x.getContext("2d");
          }));
    return Curry._1(setRef, theRef);
  };
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              return Curry._3(saveTick, self[/* onUnmount */4], layerKey, (function () {
                            return DrawCommand$Gayer.drawCommands(self[/* state */1][/* drawContext */0], cmds);
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
              return /* record */[/* drawContext : record */[
                        /* maybeCtxRef : record */[/* contents */undefined],
                        /* layerRefs */layerRefs,
                        /* width */width,
                        /* height */height,
                        /* variables */Belt_MapString.empty
                      ]];
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
