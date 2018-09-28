// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as Belt_MapString from "bs-platform/lib/es6/belt_MapString.js";

function onTick(opts, globalDrawContext, layerRefs, _, _$1, param) {
  var state = param[/* state */1];
  var sourceDrawContext_000 = /* maybeCtxRef : record */[/* contents */undefined];
  var sourceDrawContext = /* record */[
    sourceDrawContext_000,
    /* width */640,
    /* height */480,
    /* variables */Belt_MapString.empty
  ];
  var sourceRect_000 = /* x */Canvas$Gayer.DrawCommand[/* getLength */3](sourceDrawContext, opts[/* sourceRect */1][/* x */0]);
  var sourceRect_001 = /* y */Canvas$Gayer.DrawCommand[/* getLength */3](sourceDrawContext, opts[/* sourceRect */1][/* y */1]);
  var sourceRect_002 = /* w */Canvas$Gayer.DrawCommand[/* getLength */3](sourceDrawContext, opts[/* sourceRect */1][/* w */2]);
  var sourceRect_003 = /* h */Canvas$Gayer.DrawCommand[/* getLength */3](sourceDrawContext, opts[/* sourceRect */1][/* h */3]);
  var sourceRect = /* record */[
    sourceRect_000,
    sourceRect_001,
    sourceRect_002,
    sourceRect_003
  ];
  var sourceWidth = Canvas$Gayer.DrawCommand[/* getLength */3](sourceDrawContext, /* Width */0);
  var sourceHeight = Canvas$Gayer.DrawCommand[/* getLength */3](sourceDrawContext, /* Height */1);
  var destRect_000 = /* x */Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, opts[/* destRect */2][/* x */0]);
  var destRect_001 = /* y */Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, opts[/* destRect */2][/* y */1]);
  var destRect_002 = /* w */Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, opts[/* destRect */2][/* w */2]);
  var destRect_003 = /* h */Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, opts[/* destRect */2][/* h */3]);
  var destRect = /* record */[
    destRect_000,
    destRect_001,
    destRect_002,
    destRect_003
  ];
  var destWidth = Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, /* Width */0);
  var destHeight = Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, /* Height */1);
  state[/* sourceRectReal */1][0] = sourceRect;
  state[/* sourceXDelta */3][0] = Canvas$Gayer.DrawCommand[/* getLength */3](sourceDrawContext, opts[/* sourceXDelta */3]);
  state[/* sourceYDelta */4][0] = Canvas$Gayer.DrawCommand[/* getLength */3](sourceDrawContext, opts[/* sourceYDelta */4]);
  state[/* destRectReal */2][0] = destRect;
  state[/* destXDelta */5][0] = Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, opts[/* destXDelta */5]);
  state[/* destYDelta */6][0] = Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, opts[/* destYDelta */6]);
  return (function () {
      var match = state[/* canvasRef */0][0];
      var match$1 = Belt_MapString.get(layerRefs[0], opts[/* sourceLayerKey */0]);
      if (match !== undefined && match$1 !== undefined) {
        var ctx = Js_primitive.valFromOption(match).getContext("2d");
        var currentSourceRect = state[/* sourceRectReal */1][0];
        state[/* sourceRectReal */1][0] = /* record */[
          /* x */Canvas$Gayer.wrapCoord(currentSourceRect[/* x */0], state[/* sourceXDelta */3][0], sourceWidth),
          /* y */Canvas$Gayer.wrapCoord(currentSourceRect[/* y */1], state[/* sourceYDelta */4][0], sourceHeight),
          /* w */currentSourceRect[/* w */2],
          /* h */currentSourceRect[/* h */3]
        ];
        var currentDestRect = state[/* destRectReal */2][0];
        state[/* destRectReal */2][0] = /* record */[
          /* x */Canvas$Gayer.wrapCoord(currentDestRect[/* x */0], state[/* destXDelta */5][0], destWidth),
          /* y */Canvas$Gayer.wrapCoord(currentDestRect[/* y */1], state[/* destYDelta */6][0], destHeight),
          /* w */currentDestRect[/* w */2],
          /* h */currentDestRect[/* h */3]
        ];
        ctx.drawImage(Js_primitive.valFromOption(match$1), currentSourceRect[/* x */0], currentSourceRect[/* y */1], currentSourceRect[/* w */2], currentSourceRect[/* h */3], currentDestRect[/* x */0], currentDestRect[/* y */1], currentDestRect[/* w */2], currentDestRect[/* h */3]);
        return /* () */0;
      } else {
        return /* () */0;
      }
    });
}

var component = ReasonReact.reducerComponent("SlitscanCanvas-Gayer");

function make(setRef, layerKey, layerRefs, width, height, saveTick, globalDrawContext, opts, _) {
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
              return Curry._3(saveTick, self[/* onUnmount */4], layerKey, onTick(opts, globalDrawContext, layerRefs, width, height, self));
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */(function (param) {
              return Curry._3(saveTick, (function () {
                            return /* () */0;
                          }), layerKey, onTick(opts, globalDrawContext, layerRefs, width, height, param[/* newSelf */1]));
            }),
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
              return /* record */[
                      /* canvasRef : record */[/* contents */undefined],
                      /* sourceRectReal : record */[/* contents : record */[
                          /* x */0,
                          /* y */0,
                          /* w */0,
                          /* h */0
                        ]],
                      /* destRectReal : record */[/* contents : record */[
                          /* x */0,
                          /* y */0,
                          /* w */0,
                          /* h */0
                        ]],
                      /* sourceXDelta : record */[/* contents */0],
                      /* sourceYDelta : record */[/* contents */0],
                      /* destXDelta : record */[/* contents */0],
                      /* destYDelta : record */[/* contents */0]
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
  onTick ,
  component ,
  make ,
  
}
/* component Not a pure module */
