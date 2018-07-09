// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as Audio$Gayer from "./Audio.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Video$Gayer from "./Video.bs.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as UserMedia$Gayer from "./UserMedia.bs.js";

var defaultState_012 = /* allowedPitchClasses */Curry._1(Music$Gayer.PitchSet[/* of_list */25], /* :: */[
      0,
      /* :: */[
        2,
        /* :: */[
          5,
          /* :: */[
            7,
            /* :: */[
              9,
              /* [] */0
            ]
          ]
        ]
      ]
    ]);

var defaultState_014 = /* canvasRef */[/* None */0];

var defaultState_015 = /* windowHeight */Curry._1(Canvas$Gayer.getWindowHeight, /* () */0);

var defaultState_016 = /* timerId */[/* None */0];

var defaultState = /* record */[
  /* xIndex */0,
  /* xDelta */1,
  /* inputGain */1.0,
  /* outputGain */0.1,
  /* filterInput */Audio$Gayer.defaultNoise,
  /* visualInput : None */0,
  /* micInput : None */0,
  /* cameraInput : None */0,
  /* shouldClear */true,
  /* channelToRead : R */0,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  defaultState_012,
  /* filterBank : None */0,
  defaultState_014,
  defaultState_015,
  defaultState_016
];

function setCanvasRef(theRef, param) {
  param[/* state */1][/* canvasRef */14][0] = (theRef == null) ? /* None */0 : [theRef];
  return /* () */0;
}

var component = ReasonReact.reducerComponent("App");

function maybeUpdateCanvas(maybeEl, f) {
  var match = maybeEl[0];
  if (match) {
    return Curry._1(f, match[0]);
  } else {
    return /* () */0;
  }
}

function maybeMapFilterBank(f, maybeFilterBank) {
  if (maybeFilterBank) {
    return Curry._1(f, maybeFilterBank[0]);
  } else {
    return /* () */0;
  }
}

function clearCanvas(canvasElement, width, height) {
  var ctx = canvasElement.getContext("2d");
  ctx.clearRect(0, 0, width, height);
  return /* () */0;
}

function drawCanvas(canvasElement, width, height, state) {
  if (state[/* shouldClear */8]) {
    clearCanvas(canvasElement, width, height);
  }
  var ctx = canvasElement.getContext("2d");
  ctx.globalAlpha = state[/* alpha */10];
  Canvas$Gayer.Ctx[/* setGlobalCompositeOperation */0](ctx, state[/* compositeOperation */11]);
  var match = state[/* visualInput */5];
  if (match) {
    ctx.drawImage(match[0], 0, 0, width, height);
  }
  var slice = ctx.getImageData(state[/* xIndex */0], 0, 1, height);
  var values = Canvas$Gayer.imageDataToFloatArray(slice, state[/* channelToRead */9]);
  ctx.globalAlpha = 1.0;
  Canvas$Gayer.Ctx[/* setGlobalCompositeOperation */0](ctx, /* SourceOver */0);
  ctx.fillStyle = "white";
  ctx.fillRect(state[/* xIndex */0], 0, 1, height);
  return values;
}

function make($staropt$star, $staropt$star$1, _) {
  var width = $staropt$star ? $staropt$star[0] : 120;
  var height = $staropt$star$1 ? $staropt$star$1[0] : 120;
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              var filterBank = Audio$Gayer.defaultFilterBank(/* Some */[Audio$Gayer.defaultAudioCtx], /* Some */[height], /* Some */[Audio$Gayer.defaultQ]);
              Audio$Gayer.connectFilterBank(self[/* state */1][/* filterInput */4], filterBank);
              var match = UserMedia$Gayer.getAudioVisualStream(/* () */0);
              if (match) {
                match[0].then((function (stream) {
                        var audio = Audio$Gayer.defaultAudioCtx.createMediaStreamSource(stream);
                        var video = Video$Gayer.attachVideoStream(stream);
                        Curry._1(self[/* send */3], /* SetMicInput */Block.__(2, [audio]));
                        Curry._1(self[/* send */3], /* SetCameraInput */Block.__(3, [/* Some */[video]]));
                        Curry._1(self[/* send */3], /* SetFilterInput */Block.__(0, [audio]));
                        Curry._1(self[/* send */3], /* SetVisualInput */Block.__(1, [/* Some */[video]]));
                        return Promise.resolve(/* () */0);
                      }));
              }
              Curry._1(self[/* send */3], /* SetFilterBank */Block.__(4, [filterBank]));
              Curry._1(self[/* send */3], /* Clear */0);
              self[/* state */1][/* timerId */16][0] = /* Some */[setInterval((function () {
                        return Curry._1(self[/* send */3], /* Tick */1);
                      }), 33)];
              return /* () */0;
            }),
          /* didUpdate */(function (param) {
              var newSelf = param[/* newSelf */1];
              var oldSelf = param[/* oldSelf */0];
              if (oldSelf[/* state */1][/* filterInput */4] !== newSelf[/* state */1][/* filterInput */4]) {
                var partial_arg = oldSelf[/* state */1][/* filterInput */4];
                maybeMapFilterBank((function (param) {
                        return Audio$Gayer.disconnectFilterBank(partial_arg, param);
                      }), oldSelf[/* state */1][/* filterBank */13]);
              }
              if (oldSelf[/* state */1][/* filterBank */13] !== newSelf[/* state */1][/* filterBank */13]) {
                var partial_arg$1 = oldSelf[/* state */1][/* filterInput */4];
                return maybeMapFilterBank((function (param) {
                              return Audio$Gayer.disconnectFilterBank(partial_arg$1, param);
                            }), oldSelf[/* state */1][/* filterBank */13]);
              } else {
                return 0;
              }
            }),
          /* willUnmount */(function (self) {
              var partial_arg = self[/* state */1][/* filterInput */4];
              return maybeMapFilterBank((function (param) {
                            return Audio$Gayer.disconnectFilterBank(partial_arg, param);
                          }), self[/* state */1][/* filterBank */13]);
            }),
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              return React.createElement("div", {
                          onClick: (function () {
                              return Curry._1(self[/* send */3], /* Tick */1);
                            })
                        }, React.createElement("h1", undefined, "GAYER"), React.createElement("canvas", {
                              ref: Curry._1(self[/* handle */0], setCanvasRef),
                              height: height.toString(),
                              width: width.toString()
                            }));
            }),
          /* initialState */(function () {
              return defaultState;
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              if (typeof action === "number") {
                if (action === 0) {
                  return /* SideEffects */Block.__(1, [(function (self) {
                                return maybeUpdateCanvas(self[/* state */1][/* canvasRef */14], (function (canvas) {
                                              return clearCanvas(canvas, width, height);
                                            }));
                              })]);
                } else {
                  return /* UpdateWithSideEffects */Block.__(2, [
                            /* record */[
                              /* xIndex */Caml_int32.mod_(state[/* xIndex */0] + state[/* xDelta */1] | 0, width),
                              /* xDelta */state[/* xDelta */1],
                              /* inputGain */state[/* inputGain */2],
                              /* outputGain */state[/* outputGain */3],
                              /* filterInput */state[/* filterInput */4],
                              /* visualInput */state[/* visualInput */5],
                              /* micInput */state[/* micInput */6],
                              /* cameraInput */state[/* cameraInput */7],
                              /* shouldClear */state[/* shouldClear */8],
                              /* channelToRead */state[/* channelToRead */9],
                              /* alpha */state[/* alpha */10],
                              /* compositeOperation */state[/* compositeOperation */11],
                              /* allowedPitchClasses */state[/* allowedPitchClasses */12],
                              /* filterBank */state[/* filterBank */13],
                              /* canvasRef */state[/* canvasRef */14],
                              /* windowHeight */state[/* windowHeight */15],
                              /* timerId */state[/* timerId */16]
                            ],
                            (function (self) {
                                return maybeUpdateCanvas(self[/* state */1][/* canvasRef */14], (function (canvas) {
                                              var rawFilterValues = drawCanvas(canvas, width, height, self[/* state */1]);
                                              var filterValues = Music$Gayer.filterByPitchSet(self[/* state */1][/* allowedPitchClasses */12], rawFilterValues);
                                              return maybeMapFilterBank((function (filterBank) {
                                                            return Audio$Gayer.updateFilterBank(filterBank, filterValues, self[/* state */1][/* inputGain */2], self[/* state */1][/* outputGain */3]);
                                                          }), self[/* state */1][/* filterBank */13]);
                                            }));
                              })
                          ]);
                }
              } else {
                switch (action.tag | 0) {
                  case 0 : 
                      return /* UpdateWithSideEffects */Block.__(2, [
                                /* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* xDelta */state[/* xDelta */1],
                                  /* inputGain */state[/* inputGain */2],
                                  /* outputGain */state[/* outputGain */3],
                                  /* filterInput */action[0],
                                  /* visualInput */state[/* visualInput */5],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* shouldClear */state[/* shouldClear */8],
                                  /* channelToRead */state[/* channelToRead */9],
                                  /* alpha */state[/* alpha */10],
                                  /* compositeOperation */state[/* compositeOperation */11],
                                  /* allowedPitchClasses */state[/* allowedPitchClasses */12],
                                  /* filterBank */state[/* filterBank */13],
                                  /* canvasRef */state[/* canvasRef */14],
                                  /* windowHeight */state[/* windowHeight */15],
                                  /* timerId */state[/* timerId */16]
                                ],
                                (function (self) {
                                    var partial_arg = self[/* state */1][/* filterInput */4];
                                    return maybeMapFilterBank((function (param) {
                                                  return Audio$Gayer.connectFilterBank(partial_arg, param);
                                                }), self[/* state */1][/* filterBank */13]);
                                  })
                              ]);
                  case 1 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* xDelta */state[/* xDelta */1],
                                  /* inputGain */state[/* inputGain */2],
                                  /* outputGain */state[/* outputGain */3],
                                  /* filterInput */state[/* filterInput */4],
                                  /* visualInput */action[0],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* shouldClear */state[/* shouldClear */8],
                                  /* channelToRead */state[/* channelToRead */9],
                                  /* alpha */state[/* alpha */10],
                                  /* compositeOperation */state[/* compositeOperation */11],
                                  /* allowedPitchClasses */state[/* allowedPitchClasses */12],
                                  /* filterBank */state[/* filterBank */13],
                                  /* canvasRef */state[/* canvasRef */14],
                                  /* windowHeight */state[/* windowHeight */15],
                                  /* timerId */state[/* timerId */16]
                                ]]);
                  case 2 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* xDelta */state[/* xDelta */1],
                                  /* inputGain */state[/* inputGain */2],
                                  /* outputGain */state[/* outputGain */3],
                                  /* filterInput */state[/* filterInput */4],
                                  /* visualInput */state[/* visualInput */5],
                                  /* micInput : Some */[action[0]],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* shouldClear */state[/* shouldClear */8],
                                  /* channelToRead */state[/* channelToRead */9],
                                  /* alpha */state[/* alpha */10],
                                  /* compositeOperation */state[/* compositeOperation */11],
                                  /* allowedPitchClasses */state[/* allowedPitchClasses */12],
                                  /* filterBank */state[/* filterBank */13],
                                  /* canvasRef */state[/* canvasRef */14],
                                  /* windowHeight */state[/* windowHeight */15],
                                  /* timerId */state[/* timerId */16]
                                ]]);
                  case 3 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* xDelta */state[/* xDelta */1],
                                  /* inputGain */state[/* inputGain */2],
                                  /* outputGain */state[/* outputGain */3],
                                  /* filterInput */state[/* filterInput */4],
                                  /* visualInput */state[/* visualInput */5],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */action[0],
                                  /* shouldClear */state[/* shouldClear */8],
                                  /* channelToRead */state[/* channelToRead */9],
                                  /* alpha */state[/* alpha */10],
                                  /* compositeOperation */state[/* compositeOperation */11],
                                  /* allowedPitchClasses */state[/* allowedPitchClasses */12],
                                  /* filterBank */state[/* filterBank */13],
                                  /* canvasRef */state[/* canvasRef */14],
                                  /* windowHeight */state[/* windowHeight */15],
                                  /* timerId */state[/* timerId */16]
                                ]]);
                  case 4 : 
                      return /* UpdateWithSideEffects */Block.__(2, [
                                /* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* xDelta */state[/* xDelta */1],
                                  /* inputGain */state[/* inputGain */2],
                                  /* outputGain */state[/* outputGain */3],
                                  /* filterInput */state[/* filterInput */4],
                                  /* visualInput */state[/* visualInput */5],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* shouldClear */state[/* shouldClear */8],
                                  /* channelToRead */state[/* channelToRead */9],
                                  /* alpha */state[/* alpha */10],
                                  /* compositeOperation */state[/* compositeOperation */11],
                                  /* allowedPitchClasses */state[/* allowedPitchClasses */12],
                                  /* filterBank : Some */[action[0]],
                                  /* canvasRef */state[/* canvasRef */14],
                                  /* windowHeight */state[/* windowHeight */15],
                                  /* timerId */state[/* timerId */16]
                                ],
                                (function (self) {
                                    var partial_arg = self[/* state */1][/* filterInput */4];
                                    return maybeMapFilterBank((function (param) {
                                                  return Audio$Gayer.connectFilterBank(partial_arg, param);
                                                }), self[/* state */1][/* filterBank */13]);
                                  })
                              ]);
                  case 5 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* xIndex */Caml_int32.mod_(action[0], width),
                                  /* xDelta */state[/* xDelta */1],
                                  /* inputGain */state[/* inputGain */2],
                                  /* outputGain */state[/* outputGain */3],
                                  /* filterInput */state[/* filterInput */4],
                                  /* visualInput */state[/* visualInput */5],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* shouldClear */state[/* shouldClear */8],
                                  /* channelToRead */state[/* channelToRead */9],
                                  /* alpha */state[/* alpha */10],
                                  /* compositeOperation */state[/* compositeOperation */11],
                                  /* allowedPitchClasses */state[/* allowedPitchClasses */12],
                                  /* filterBank */state[/* filterBank */13],
                                  /* canvasRef */state[/* canvasRef */14],
                                  /* windowHeight */state[/* windowHeight */15],
                                  /* timerId */state[/* timerId */16]
                                ]]);
                  case 6 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* xDelta */action[0],
                                  /* inputGain */state[/* inputGain */2],
                                  /* outputGain */state[/* outputGain */3],
                                  /* filterInput */state[/* filterInput */4],
                                  /* visualInput */state[/* visualInput */5],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* shouldClear */state[/* shouldClear */8],
                                  /* channelToRead */state[/* channelToRead */9],
                                  /* alpha */state[/* alpha */10],
                                  /* compositeOperation */state[/* compositeOperation */11],
                                  /* allowedPitchClasses */state[/* allowedPitchClasses */12],
                                  /* filterBank */state[/* filterBank */13],
                                  /* canvasRef */state[/* canvasRef */14],
                                  /* windowHeight */state[/* windowHeight */15],
                                  /* timerId */state[/* timerId */16]
                                ]]);
                  
                }
              }
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  defaultState ,
  setCanvasRef ,
  component ,
  maybeUpdateCanvas ,
  maybeMapFilterBank ,
  clearCanvas ,
  drawCanvas ,
  make ,
  
}
/* defaultState Not a pure module */
