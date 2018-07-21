// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Caml_obj from "bs-platform/lib/es6/caml_obj.js";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as Audio$Gayer from "./Audio.bs.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Video$Gayer from "./Video.bs.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as UserMedia$Gayer from "./UserMedia.bs.js";
import * as AnalysisCanvas$Gayer from "./AnalysisCanvas.bs.js";

var defaultParams = /* record */[
  /* xDelta */1,
  /* inputGain */1.0,
  /* outputGain */0.2,
  /* q */Audio$Gayer.defaultQ,
  /* transpose */0,
  /* shouldClear */false,
  /* channelToRead : R */0,
  /* alpha */0.25,
  /* compositeOperation : Overlay */13,
  /* allowedPitchClasses */Music$Gayer.cMajor
];

function params(json) {
  return /* record */[
          /* xDelta */Json_decode.field("xDelta", Json_decode.$$int, json),
          /* inputGain */Json_decode.field("inputGain", Json_decode.$$float, json),
          /* outputGain */Json_decode.field("outputGain", Json_decode.$$float, json),
          /* q */Json_decode.field("q", Json_decode.$$float, json),
          /* transpose */Json_decode.field("transpose", Json_decode.$$int, json),
          /* shouldClear */Json_decode.field("shouldClear", Json_decode.bool, json),
          /* channelToRead */Json_decode.map(Canvas$Gayer.channel_of_int, (function (param) {
                  return Json_decode.field("channelToRead", Json_decode.$$int, param);
                }), json),
          /* alpha */Json_decode.field("alpha", Json_decode.$$float, json),
          /* compositeOperation */Json_decode.map(Canvas$Gayer.compositeOperation_of_string, (function (param) {
                  return Json_decode.field("compositeOperation", Json_decode.string, param);
                }), json),
          /* allowedPitchClasses */Json_decode.map(Music$Gayer.PitchSet[/* of_list */25], (function (param) {
                  return Json_decode.field("allowedPitchClasses", (function (param) {
                                return Json_decode.list(Json_decode.$$int, param);
                              }), param);
                }), json)
        ];
}

var DecodeParams = /* module */[/* params */params];

function params$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "xDelta",
                r[/* xDelta */0]
              ],
              /* :: */[
                /* tuple */[
                  "inputGain",
                  r[/* inputGain */1]
                ],
                /* :: */[
                  /* tuple */[
                    "outputGain",
                    r[/* outputGain */2]
                  ],
                  /* :: */[
                    /* tuple */[
                      "q",
                      r[/* q */3]
                    ],
                    /* :: */[
                      /* tuple */[
                        "transpose",
                        r[/* transpose */4]
                      ],
                      /* :: */[
                        /* tuple */[
                          "shouldClear",
                          r[/* shouldClear */5]
                        ],
                        /* :: */[
                          /* tuple */[
                            "alpha",
                            r[/* alpha */7]
                          ],
                          /* :: */[
                            /* tuple */[
                              "compositeOperation",
                              Canvas$Gayer.string_of_compositeOperation(r[/* compositeOperation */8])
                            ],
                            /* :: */[
                              /* tuple */[
                                "channelToRead",
                                Canvas$Gayer.int_of_channel(r[/* channelToRead */6])
                              ],
                              /* :: */[
                                /* tuple */[
                                  "allowedPitchClasses",
                                  Json_encode.list((function (prim) {
                                          return prim;
                                        }), Curry._1(Music$Gayer.PitchSet[/* elements */19], r[/* allowedPitchClasses */9]))
                                ],
                                /* [] */0
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var EncodeParams = /* module */[/* params */params$1];

var defaultState_007 = /* analysisCanvasRef */[/* None */0];

var defaultState_008 = /* canvasRef */[/* None */0];

var defaultState_009 = /* timerId */[/* None */0];

var defaultState = /* record */[
  /* xIndex */0,
  /* filterInput */Audio$Gayer.defaultNoise,
  /* visualInput : None */0,
  /* params */defaultParams,
  /* micInput : None */0,
  /* cameraInput : None */0,
  /* filterBank : None */0,
  defaultState_007,
  defaultState_008,
  defaultState_009
];

function setCanvasRef(theRef, param) {
  param[/* state */1][/* canvasRef */8][0] = (theRef == null) ? /* None */0 : [theRef];
  return /* () */0;
}

function setAnalysisCanvasRef(theRef, param) {
  param[/* state */1][/* analysisCanvasRef */7][0] = (theRef == null) ? /* None */0 : [theRef];
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

function connectInputs(state) {
  var partial_arg = state[/* filterInput */1];
  return maybeMapFilterBank((function (param) {
                return Audio$Gayer.connectFilterBank(partial_arg, param);
              }), state[/* filterBank */6]);
}

function disconnectInputs(state) {
  var partial_arg = state[/* filterInput */1];
  return maybeMapFilterBank((function (param) {
                return Audio$Gayer.disconnectFilterBank(partial_arg, param);
              }), state[/* filterBank */6]);
}

function clearCanvas(canvasElement, width, height) {
  var ctx = canvasElement.getContext("2d");
  ctx.clearRect(0, 0, width, height);
  return /* () */0;
}

function drawCanvas(canvasElement, width, height, state) {
  if (state[/* params */3][/* shouldClear */5]) {
    clearCanvas(canvasElement, width, height);
  }
  var ctx = canvasElement.getContext("2d");
  var match = state[/* analysisCanvasRef */7][0];
  if (match) {
    ctx.globalAlpha = 1.0;
    Canvas$Gayer.Ctx[/* setGlobalCompositeOperation */0](ctx, /* SourceOver */0);
    ctx.drawImage(match[0], state[/* xIndex */0], 0);
  }
  ctx.globalAlpha = state[/* params */3][/* alpha */7];
  Canvas$Gayer.Ctx[/* setGlobalCompositeOperation */0](ctx, state[/* params */3][/* compositeOperation */8]);
  var match$1 = state[/* visualInput */2];
  if (match$1) {
    ctx.drawImage(match$1[0], 0, 0, width, height);
  }
  var slice = ctx.getImageData(state[/* xIndex */0], 0, 1, height);
  return Canvas$Gayer.imageDataToFloatArray(slice, state[/* params */3][/* channelToRead */6]);
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
              var filterBank = Audio$Gayer.makeFilterBank(Audio$Gayer.defaultAudioCtx, height, Audio$Gayer.defaultQ, (function (param) {
                      return Audio$Gayer.frequencyFromNoteNumber(16, param);
                    }));
              Curry._1(self[/* send */3], /* SetFilterBank */Block.__(4, [filterBank]));
              var match = UserMedia$Gayer.getAudioVisualStream(/* () */0);
              if (match) {
                match[0].then((function (stream) {
                        var audio = Audio$Gayer.defaultAudioCtx.createMediaStreamSource(stream);
                        var video = Video$Gayer.attachVideoStream(stream);
                        Curry._1(self[/* send */3], /* SetMicInput */Block.__(2, [audio]));
                        Curry._1(self[/* send */3], /* SetCameraInput */Block.__(3, [/* Some */[video]]));
                        Curry._1(self[/* send */3], /* SetVisualInput */Block.__(1, [/* Some */[video]]));
                        return Promise.resolve(/* () */0);
                      }));
              }
              Curry._1(self[/* send */3], /* Clear */0);
              self[/* state */1][/* timerId */9][0] = /* Some */[setInterval((function () {
                        return Curry._1(self[/* send */3], /* Tick */1);
                      }), 20)];
              return /* () */0;
            }),
          /* didUpdate */(function (param) {
              var newSelf = param[/* newSelf */1];
              var oldSelf = param[/* oldSelf */0];
              if (Caml_obj.caml_notequal(oldSelf[/* state */1][/* filterInput */1], newSelf[/* state */1][/* filterInput */1]) || Caml_obj.caml_notequal(oldSelf[/* state */1][/* filterBank */6], newSelf[/* state */1][/* filterBank */6])) {
                return disconnectInputs(oldSelf[/* state */1]);
              } else {
                return 0;
              }
            }),
          /* willUnmount */(function (self) {
              return disconnectInputs(self[/* state */1]);
            }),
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              return React.createElement("div", {
                          style: {
                            display: "flex",
                            minHeight: (height << 2).toString(),
                            flexDirection: "row",
                            justifyContent: "space-between"
                          },
                          onClick: (function () {
                              return Curry._1(self[/* send */3], /* Tick */1);
                            })
                        }, React.createElement("div", {
                              style: {
                                margin: "10px",
                                width: "50%"
                              }
                            }, React.createElement("h1", undefined, "GAYER"), React.createElement("div", undefined, "UI forthcoming; for now, please download and edit defaultParams in App.re"), React.createElement("a", {
                                  href: "https://github.com/corajr/gayer"
                                }, "source"), React.createElement("br", undefined), "params:", React.createElement("br", undefined), React.createElement("div", undefined, JSON.stringify(params$1(self[/* state */1][/* params */3]), null, 2))), React.createElement("div", {
                              style: {
                                display: "flex",
                                margin: "0",
                                minHeight: (height << 2).toString(),
                                width: "50%",
                                alignItems: "flex-start",
                                justifyContent: "flex-end"
                              }
                            }, React.createElement("canvas", {
                                  ref: Curry._1(self[/* handle */0], setCanvasRef),
                                  style: {
                                    transform: "scale(4)",
                                    transformOrigin: "top right"
                                  },
                                  height: height.toString(),
                                  width: width.toString()
                                }), ReasonReact.element(/* None */0, /* None */0, AnalysisCanvas$Gayer.make(height, Audio$Gayer.defaultAudioCtx, self[/* state */1][/* micInput */4], Curry._1(self[/* handle */0], setAnalysisCanvasRef), /* array */[]))));
            }),
          /* initialState */(function () {
              return defaultState;
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              if (typeof action === "number") {
                if (action === 0) {
                  return /* SideEffects */Block.__(1, [(function (self) {
                                return maybeUpdateCanvas(self[/* state */1][/* canvasRef */8], (function (canvas) {
                                              return clearCanvas(canvas, width, height);
                                            }));
                              })]);
                } else {
                  return /* UpdateWithSideEffects */Block.__(2, [
                            /* record */[
                              /* xIndex */Caml_int32.mod_(state[/* xIndex */0] + state[/* params */3][/* xDelta */0] | 0, width),
                              /* filterInput */state[/* filterInput */1],
                              /* visualInput */state[/* visualInput */2],
                              /* params */state[/* params */3],
                              /* micInput */state[/* micInput */4],
                              /* cameraInput */state[/* cameraInput */5],
                              /* filterBank */state[/* filterBank */6],
                              /* analysisCanvasRef */state[/* analysisCanvasRef */7],
                              /* canvasRef */state[/* canvasRef */8],
                              /* timerId */state[/* timerId */9]
                            ],
                            (function (self) {
                                return maybeUpdateCanvas(self[/* state */1][/* canvasRef */8], (function (canvas) {
                                              var rawFilterValues = drawCanvas(canvas, width, height, self[/* state */1]);
                                              var filterValues = Music$Gayer.filterByPitchSet(self[/* state */1][/* params */3][/* allowedPitchClasses */9], rawFilterValues);
                                              return maybeMapFilterBank((function (filterBank) {
                                                            var partial_arg = 16 + self[/* state */1][/* params */3][/* transpose */4] | 0;
                                                            return Audio$Gayer.updateFilterBank(/* Some */[self[/* state */1][/* params */3][/* inputGain */1]], /* Some */[self[/* state */1][/* params */3][/* outputGain */2]], /* Some */[self[/* state */1][/* params */3][/* q */3]], /* Some */[(function (param) {
                                                                            return Audio$Gayer.frequencyFromNoteNumber(partial_arg, param);
                                                                          })], filterBank, filterValues);
                                                          }), self[/* state */1][/* filterBank */6]);
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
                                  /* filterInput */action[0],
                                  /* visualInput */state[/* visualInput */2],
                                  /* params */state[/* params */3],
                                  /* micInput */state[/* micInput */4],
                                  /* cameraInput */state[/* cameraInput */5],
                                  /* filterBank */state[/* filterBank */6],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */7],
                                  /* canvasRef */state[/* canvasRef */8],
                                  /* timerId */state[/* timerId */9]
                                ],
                                (function (self) {
                                    return connectInputs(self[/* state */1]);
                                  })
                              ]);
                  case 1 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* filterInput */state[/* filterInput */1],
                                  /* visualInput */action[0],
                                  /* params */state[/* params */3],
                                  /* micInput */state[/* micInput */4],
                                  /* cameraInput */state[/* cameraInput */5],
                                  /* filterBank */state[/* filterBank */6],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */7],
                                  /* canvasRef */state[/* canvasRef */8],
                                  /* timerId */state[/* timerId */9]
                                ]]);
                  case 2 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* filterInput */state[/* filterInput */1],
                                  /* visualInput */state[/* visualInput */2],
                                  /* params */state[/* params */3],
                                  /* micInput : Some */[action[0]],
                                  /* cameraInput */state[/* cameraInput */5],
                                  /* filterBank */state[/* filterBank */6],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */7],
                                  /* canvasRef */state[/* canvasRef */8],
                                  /* timerId */state[/* timerId */9]
                                ]]);
                  case 3 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* filterInput */state[/* filterInput */1],
                                  /* visualInput */state[/* visualInput */2],
                                  /* params */state[/* params */3],
                                  /* micInput */state[/* micInput */4],
                                  /* cameraInput */action[0],
                                  /* filterBank */state[/* filterBank */6],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */7],
                                  /* canvasRef */state[/* canvasRef */8],
                                  /* timerId */state[/* timerId */9]
                                ]]);
                  case 4 : 
                      return /* UpdateWithSideEffects */Block.__(2, [
                                /* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* filterInput */state[/* filterInput */1],
                                  /* visualInput */state[/* visualInput */2],
                                  /* params */state[/* params */3],
                                  /* micInput */state[/* micInput */4],
                                  /* cameraInput */state[/* cameraInput */5],
                                  /* filterBank : Some */[action[0]],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */7],
                                  /* canvasRef */state[/* canvasRef */8],
                                  /* timerId */state[/* timerId */9]
                                ],
                                (function (self) {
                                    return connectInputs(self[/* state */1]);
                                  })
                              ]);
                  case 5 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* xIndex */state[/* xIndex */0],
                                  /* filterInput */state[/* filterInput */1],
                                  /* visualInput */state[/* visualInput */2],
                                  /* params */action[0],
                                  /* micInput */state[/* micInput */4],
                                  /* cameraInput */state[/* cameraInput */5],
                                  /* filterBank */state[/* filterBank */6],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */7],
                                  /* canvasRef */state[/* canvasRef */8],
                                  /* timerId */state[/* timerId */9]
                                ]]);
                  
                }
              }
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  defaultParams ,
  DecodeParams ,
  EncodeParams ,
  defaultState ,
  setCanvasRef ,
  setAnalysisCanvasRef ,
  component ,
  maybeUpdateCanvas ,
  maybeMapFilterBank ,
  connectInputs ,
  disconnectInputs ,
  clearCanvas ,
  drawCanvas ,
  make ,
  
}
/* component Not a pure module */
