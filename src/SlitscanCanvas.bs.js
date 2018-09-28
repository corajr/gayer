// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as Belt_MapString from "bs-platform/lib/es6/belt_MapString.js";
import * as DrawCommand$Gayer from "./DrawCommand.bs.js";

function onTick(opts, globalDrawContext, layerRefs, writePos, _, _$1, param) {
  var state = param[/* state */1];
  var match = Belt_MapString.get(layerRefs[0], opts[/* sourceLayerKey */0]);
  var sourceDrawContext;
  if (match !== undefined) {
    var el = Js_primitive.valFromOption(match);
    var exit = 0;
    var ctx;
    try {
      ctx = el.getContext("2d");
      exit = 1;
    }
    catch (exn){
      sourceDrawContext = /* record */[
        /* maybeCtxRef : record */[/* contents */undefined],
        /* layerRefs */layerRefs,
        /* width */640,
        /* height */480,
        /* variables */Belt_MapString.empty
      ];
    }
    if (exit === 1) {
      sourceDrawContext = /* record */[
        /* maybeCtxRef : record */[/* contents */Js_primitive.some(ctx)],
        /* layerRefs */layerRefs,
        /* width */el.width,
        /* height */el.height,
        /* variables */Belt_MapString.empty
      ];
    }
    
  } else {
    sourceDrawContext = /* record */[
      /* maybeCtxRef : record */[/* contents */undefined],
      /* layerRefs */layerRefs,
      /* width */640,
      /* height */480,
      /* variables */Belt_MapString.empty
    ];
  }
  var sourceRect_000 = /* x */DrawCommand$Gayer.getLength(sourceDrawContext, opts[/* sourceRect */1][/* x */0]);
  var sourceRect_001 = /* y */DrawCommand$Gayer.getLength(sourceDrawContext, opts[/* sourceRect */1][/* y */1]);
  var sourceRect_002 = /* w */DrawCommand$Gayer.getLength(sourceDrawContext, opts[/* sourceRect */1][/* w */2]);
  var sourceRect_003 = /* h */DrawCommand$Gayer.getLength(sourceDrawContext, opts[/* sourceRect */1][/* h */3]);
  var sourceRect = /* record */[
    sourceRect_000,
    sourceRect_001,
    sourceRect_002,
    sourceRect_003
  ];
  var sourceWidth = DrawCommand$Gayer.getLength(sourceDrawContext, /* Width */0);
  var sourceHeight = DrawCommand$Gayer.getLength(sourceDrawContext, /* Height */1);
  var match$1 = opts[/* destRect */2];
  var destRect;
  if (match$1 !== undefined) {
    var destRect$1 = match$1;
    destRect = /* record */[
      /* x */DrawCommand$Gayer.getLength(globalDrawContext, destRect$1[/* x */0]),
      /* y */DrawCommand$Gayer.getLength(globalDrawContext, destRect$1[/* y */1]),
      /* w */DrawCommand$Gayer.getLength(globalDrawContext, destRect$1[/* w */2]),
      /* h */DrawCommand$Gayer.getLength(globalDrawContext, destRect$1[/* h */3])
    ];
  } else {
    destRect = /* record */[
      /* x */0,
      /* y */0,
      /* w */1,
      /* h */DrawCommand$Gayer.getLength(globalDrawContext, /* Height */1)
    ];
  }
  DrawCommand$Gayer.getLength(globalDrawContext, /* Width */0);
  var destHeight = DrawCommand$Gayer.getLength(globalDrawContext, /* Height */1);
  state[/* sourceRectReal */1][0] = sourceRect;
  state[/* sourceXDelta */3][0] = DrawCommand$Gayer.getLength(sourceDrawContext, opts[/* sourceXDelta */3]);
  state[/* sourceYDelta */4][0] = DrawCommand$Gayer.getLength(sourceDrawContext, opts[/* sourceYDelta */4]);
  state[/* destRectReal */2][0] = destRect;
  state[/* destXDelta */5][0] = DrawCommand$Gayer.getLength(globalDrawContext, opts[/* destXDelta */5]);
  state[/* destYDelta */6][0] = DrawCommand$Gayer.getLength(globalDrawContext, opts[/* destYDelta */6]);
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
          /* x */writePos[0],
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

function make(setRef, layerKey, layerRefs, width, height, writePos, saveTick, globalDrawContext, opts, _) {
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
              return Curry._3(saveTick, self[/* onUnmount */4], layerKey, onTick(opts, globalDrawContext, layerRefs, writePos, width, height, self));
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */(function (param) {
              return Curry._3(saveTick, (function () {
                            return /* () */0;
                          }), layerKey, onTick(opts, globalDrawContext, layerRefs, writePos, width, height, param[/* newSelf */1]));
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
