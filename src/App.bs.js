// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as Json from "@glennsl/bs-json/src/Json.bs.js";
import * as List from "bs-platform/lib/es6/list.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Caml_obj from "bs-platform/lib/es6/caml_obj.js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as Audio$Gayer from "./Audio.bs.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Video$Gayer from "./Video.bs.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as Params$Gayer from "./Params.bs.js";
import * as Belt_MapString from "bs-platform/lib/es6/belt_MapString.js";
import * as RList$Rationale from "rationale/src/RList.js";
import * as UserMedia$Gayer from "./UserMedia.bs.js";
import * as AnalysisCanvas$Gayer from "./AnalysisCanvas.bs.js";

var defaultState_007 = /* cameraInput */[/* None */0];

var defaultState_009 = /* analysisCanvasRef */[/* None */0];

var defaultState_010 = /* loadedImages */[Belt_MapString.empty];

var defaultState_011 = /* canvasRef */[/* None */0];

var defaultState_012 = /* timerId */[/* None */0];

var defaultState = /* record */[
  /* readPos */0,
  /* writePos */0,
  /* filterInput */Audio$Gayer.defaultNoise,
  /* visualInput : None */0,
  /* params */Params$Gayer.defaultParams,
  /* mediaStream : None */0,
  /* micInput : None */0,
  defaultState_007,
  /* filterBank : None */0,
  defaultState_009,
  defaultState_010,
  defaultState_011,
  defaultState_012
];

function setCanvasRef(theRef, param) {
  param[/* state */1][/* canvasRef */11][0] = (theRef == null) ? /* None */0 : [theRef];
  return /* () */0;
}

function setAnalysisCanvasRef(theRef, param) {
  param[/* state */1][/* analysisCanvasRef */9][0] = (theRef == null) ? /* None */0 : [theRef];
  return /* () */0;
}

function setLayerRef(param, param$1) {
  var state = param$1[/* state */1];
  var theRef = param[1];
  var match = param[0][/* content */0];
  if (typeof match === "number") {
    if (match === 0 && !(theRef == null)) {
      var match$1 = state[/* mediaStream */5];
      var match$2 = state[/* cameraInput */7][0];
      if (match$1 && !match$2) {
        var video = Video$Gayer.attachVideoStream(theRef, match$1[0]);
        state[/* cameraInput */7][0] = /* Some */[video];
        return /* () */0;
      } else {
        return /* () */0;
      }
    } else {
      return /* () */0;
    }
  } else if (!match.tag && !(theRef == null)) {
    state[/* loadedImages */10][0] = Belt_MapString.set(state[/* loadedImages */10][0], match[0], theRef);
    return /* () */0;
  } else {
    return /* () */0;
  }
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
  var partial_arg = state[/* filterInput */2];
  return maybeMapFilterBank((function (param) {
                return Audio$Gayer.connectFilterBank(partial_arg, param);
              }), state[/* filterBank */8]);
}

function disconnectInputs(state) {
  var partial_arg = state[/* filterInput */2];
  return maybeMapFilterBank((function (param) {
                return Audio$Gayer.disconnectFilterBank(partial_arg, param);
              }), state[/* filterBank */8]);
}

function clearCanvas(canvasElement, width, height) {
  var ctx = canvasElement.getContext("2d");
  ctx.clearRect(0, 0, width, height);
  return /* () */0;
}

function pushParamsState(newParams) {
  var newParamsJson = JSON.stringify(Params$Gayer.EncodeParams[/* params */0](newParams));
  return ReasonReact.Router[/* push */0]("#" + newParamsJson);
}

function drawLayer(ctx, width, height, state, layer) {
  ctx.globalAlpha = layer[/* alpha */1];
  Canvas$Gayer.Ctx[/* setGlobalCompositeOperation */0](ctx, layer[/* compositeOperation */2]);
  var match = layer[/* content */0];
  if (typeof match === "number") {
    if (match === 0) {
      var match$1 = state[/* cameraInput */7][0];
      if (match$1) {
        ctx.drawImage(match$1[0], 0, 0, width, height);
      }
      return /* None */0;
    } else {
      var match$2 = state[/* analysisCanvasRef */9][0];
      if (match$2) {
        var x = Canvas$Gayer.wrapCoord(state[/* writePos */1] + state[/* params */4][/* writePosOffset */2] | 0, 0, width);
        ctx.drawImage(match$2[0], x, 0);
      }
      return /* None */0;
    }
  } else {
    switch (match.tag | 0) {
      case 0 : 
          var match$3 = Belt_MapString.get(state[/* loadedImages */10][0], match[0]);
          if (match$3) {
            ctx.drawImage(match$3[0], 0, 0, width, height);
          }
          return /* None */0;
      case 1 : 
          var classList = Curry._1(Music$Gayer.PitchSet[/* elements */19], Curry._2(Music$Gayer.PitchSet[/* diff */8], Music$Gayer.allPitches, match[0]));
          ctx.fillStyle = "black";
          var binsPerSemitone = height / 120 | 0;
          for(var i = 0 ,i_finish = height / 10 | 0; i <= i_finish; ++i){
            List.iter((function(i){
                return function (j) {
                  var y = Caml_int32.imul(Caml_int32.imul(i, 12) + j | 0, binsPerSemitone);
                  ctx.fillRect(0, y, width, binsPerSemitone);
                  return /* () */0;
                }
                }(i)), classList);
          }
          return /* None */0;
      case 2 : 
          var channel = match[0];
          var slice = ctx.getImageData(state[/* readPos */0], 0, 1, height);
          var tmp;
          switch (channel) {
            case 0 : 
                tmp = "red";
                break;
            case 1 : 
                tmp = "green";
                break;
            case 2 : 
                tmp = "blue";
                break;
            case 3 : 
                tmp = "white";
                break;
            
          }
          ctx.fillStyle = tmp;
          ctx.fillRect(state[/* readPos */0], 0, 1, height);
          return /* Some */[Canvas$Gayer.imageDataToFloatArray(slice, channel)];
      
    }
  }
}

function drawCanvas(canvasElement, width, height, state) {
  if (state[/* params */4][/* shouldClear */7]) {
    clearCanvas(canvasElement, width, height);
  }
  var ctx = canvasElement.getContext("2d");
  return List.fold_left((function (values, layer) {
                var newMaybeValues = drawLayer(ctx, width, height, state, layer);
                if (newMaybeValues) {
                  return newMaybeValues[0];
                } else {
                  return values;
                }
              }), Caml_array.caml_make_vect(height, 0.0), state[/* params */4][/* layers */8]);
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
              var filterBank = Audio$Gayer.makeFilterBank(Audio$Gayer.defaultAudioCtx, height, Audio$Gayer.defaultQ, Audio$Gayer.yToFrequency(height / 120 | 0, 16 + self[/* state */1][/* params */4][/* transpose */6] | 0));
              Curry._1(self[/* send */3], /* SetFilterBank */Block.__(4, [filterBank]));
              var match = UserMedia$Gayer.getAudioVisualStream(/* () */0);
              if (match) {
                match[0].then((function (stream) {
                        Curry._1(self[/* send */3], /* SetMediaStream */Block.__(3, [stream]));
                        var audio = Audio$Gayer.defaultAudioCtx.createMediaStreamSource(stream);
                        Curry._1(self[/* send */3], /* SetMicInput */Block.__(2, [audio]));
                        return Promise.resolve(/* () */0);
                      }));
              }
              Curry._1(self[/* send */3], /* Clear */0);
              self[/* state */1][/* timerId */12][0] = /* Some */[setInterval((function () {
                        return Curry._1(self[/* send */3], /* Tick */1);
                      }), 20)];
              var watcherID = ReasonReact.Router[/* watchUrl */1]((function (url) {
                      var hash = decodeURIComponent(url[/* hash */1]);
                      var match = Json.parse(hash);
                      if (match) {
                        var match$1 = Json_decode.optional(Params$Gayer.DecodeParams[/* params */0], match[0]);
                        if (match$1) {
                          return Curry._1(self[/* send */3], /* SetParams */Block.__(5, [match$1[0]]));
                        } else {
                          return /* () */0;
                        }
                      } else {
                        return /* () */0;
                      }
                    }));
              Curry._1(self[/* onUnmount */4], (function () {
                      return ReasonReact.Router[/* unwatchUrl */2](watcherID);
                    }));
              var url = ReasonReact.Router[/* dangerouslyGetInitialUrl */3](/* () */0);
              if (url[/* hash */1] === "") {
                var startingParams = JSON.stringify(Params$Gayer.EncodeParams[/* params */0](self[/* state */1][/* params */4]));
                return ReasonReact.Router[/* push */0]("#" + startingParams);
              } else {
                return ReasonReact.Router[/* push */0]("#" + url[/* hash */1]);
              }
            }),
          /* didUpdate */(function (param) {
              var newSelf = param[/* newSelf */1];
              var oldSelf = param[/* oldSelf */0];
              if (Caml_obj.caml_notequal(oldSelf[/* state */1][/* filterInput */2], newSelf[/* state */1][/* filterInput */2]) || Caml_obj.caml_notequal(oldSelf[/* state */1][/* filterBank */8], newSelf[/* state */1][/* filterBank */8])) {
                disconnectInputs(oldSelf[/* state */1]);
              }
              if (Caml_obj.caml_notequal(oldSelf[/* state */1][/* params */4][/* layers */8], newSelf[/* state */1][/* params */4][/* layers */8])) {
                newSelf[/* state */1][/* cameraInput */7][0] = /* None */0;
                newSelf[/* state */1][/* loadedImages */10][0] = Belt_MapString.empty;
                return /* () */0;
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
                            }, React.createElement("h1", undefined, "GAYER"), React.createElement("a", {
                                  href: "https://github.com/corajr/gayer"
                                }, "source"), React.createElement("br", undefined), ReasonReact.element(/* None */0, /* None */0, Params$Gayer.make(self[/* state */1][/* params */4], (function (i, j) {
                                        return Curry._1(self[/* send */3], /* MoveLayer */Block.__(0, [
                                                      i,
                                                      j
                                                    ]));
                                      }), (function (layer, theRef) {
                                        return Curry._2(self[/* handle */0], setLayerRef, /* tuple */[
                                                    layer,
                                                    theRef
                                                  ]);
                                      }), /* array */[]))), React.createElement("div", {
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
                                }), ReasonReact.element(/* None */0, /* None */0, AnalysisCanvas$Gayer.make(height, Audio$Gayer.defaultAudioCtx, self[/* state */1][/* micInput */6], Curry._1(self[/* handle */0], setAnalysisCanvasRef), /* array */[]))));
            }),
          /* initialState */(function () {
              return defaultState;
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              if (typeof action === "number") {
                if (action === 0) {
                  return /* SideEffects */Block.__(1, [(function (self) {
                                return maybeUpdateCanvas(self[/* state */1][/* canvasRef */11], (function (canvas) {
                                              return clearCanvas(canvas, width, height);
                                            }));
                              })]);
                } else {
                  return /* UpdateWithSideEffects */Block.__(2, [
                            /* record */[
                              /* readPos */Canvas$Gayer.wrapCoord(state[/* readPos */0], state[/* params */4][/* readPosDelta */0], width),
                              /* writePos */Canvas$Gayer.wrapCoord(state[/* writePos */1], state[/* params */4][/* writePosDelta */1], width),
                              /* filterInput */state[/* filterInput */2],
                              /* visualInput */state[/* visualInput */3],
                              /* params */state[/* params */4],
                              /* mediaStream */state[/* mediaStream */5],
                              /* micInput */state[/* micInput */6],
                              /* cameraInput */state[/* cameraInput */7],
                              /* filterBank */state[/* filterBank */8],
                              /* analysisCanvasRef */state[/* analysisCanvasRef */9],
                              /* loadedImages */state[/* loadedImages */10],
                              /* canvasRef */state[/* canvasRef */11],
                              /* timerId */state[/* timerId */12]
                            ],
                            (function (self) {
                                return maybeUpdateCanvas(self[/* state */1][/* canvasRef */11], (function (canvas) {
                                              var filterValues = drawCanvas(canvas, width, height, self[/* state */1]);
                                              return maybeMapFilterBank((function (filterBank) {
                                                            return Audio$Gayer.updateFilterBank(/* Some */[self[/* state */1][/* params */4][/* inputGain */3]], /* Some */[self[/* state */1][/* params */4][/* outputGain */4]], /* Some */[self[/* state */1][/* params */4][/* q */5]], /* Some */[Audio$Gayer.yToFrequency(height / 120 | 0, 16 + self[/* state */1][/* params */4][/* transpose */6] | 0)], filterBank, filterValues);
                                                          }), self[/* state */1][/* filterBank */8]);
                                            }));
                              })
                          ]);
                }
              } else {
                switch (action.tag | 0) {
                  case 0 : 
                      var hoverIndex = action[1];
                      var dragIndex = action[0];
                      return /* SideEffects */Block.__(1, [(function () {
                                    var layers = state[/* params */4][/* layers */8];
                                    var layer = List.nth(layers, dragIndex);
                                    var updatedLayers = RList$Rationale.insert(hoverIndex, layer, RList$Rationale.remove(dragIndex, 1, layers));
                                    var init = state[/* params */4];
                                    return pushParamsState(/* record */[
                                                /* readPosDelta */init[/* readPosDelta */0],
                                                /* writePosDelta */init[/* writePosDelta */1],
                                                /* writePosOffset */init[/* writePosOffset */2],
                                                /* inputGain */init[/* inputGain */3],
                                                /* outputGain */init[/* outputGain */4],
                                                /* q */init[/* q */5],
                                                /* transpose */init[/* transpose */6],
                                                /* shouldClear */init[/* shouldClear */7],
                                                /* layers */updatedLayers
                                              ]);
                                  })]);
                  case 1 : 
                      return /* UpdateWithSideEffects */Block.__(2, [
                                /* record */[
                                  /* readPos */state[/* readPos */0],
                                  /* writePos */state[/* writePos */1],
                                  /* filterInput */action[0],
                                  /* visualInput */state[/* visualInput */3],
                                  /* params */state[/* params */4],
                                  /* mediaStream */state[/* mediaStream */5],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* filterBank */state[/* filterBank */8],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */9],
                                  /* loadedImages */state[/* loadedImages */10],
                                  /* canvasRef */state[/* canvasRef */11],
                                  /* timerId */state[/* timerId */12]
                                ],
                                (function (self) {
                                    return connectInputs(self[/* state */1]);
                                  })
                              ]);
                  case 2 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* readPos */state[/* readPos */0],
                                  /* writePos */state[/* writePos */1],
                                  /* filterInput */state[/* filterInput */2],
                                  /* visualInput */state[/* visualInput */3],
                                  /* params */state[/* params */4],
                                  /* mediaStream */state[/* mediaStream */5],
                                  /* micInput : Some */[action[0]],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* filterBank */state[/* filterBank */8],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */9],
                                  /* loadedImages */state[/* loadedImages */10],
                                  /* canvasRef */state[/* canvasRef */11],
                                  /* timerId */state[/* timerId */12]
                                ]]);
                  case 3 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* readPos */state[/* readPos */0],
                                  /* writePos */state[/* writePos */1],
                                  /* filterInput */state[/* filterInput */2],
                                  /* visualInput */state[/* visualInput */3],
                                  /* params */state[/* params */4],
                                  /* mediaStream : Some */[action[0]],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* filterBank */state[/* filterBank */8],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */9],
                                  /* loadedImages */state[/* loadedImages */10],
                                  /* canvasRef */state[/* canvasRef */11],
                                  /* timerId */state[/* timerId */12]
                                ]]);
                  case 4 : 
                      return /* UpdateWithSideEffects */Block.__(2, [
                                /* record */[
                                  /* readPos */state[/* readPos */0],
                                  /* writePos */state[/* writePos */1],
                                  /* filterInput */state[/* filterInput */2],
                                  /* visualInput */state[/* visualInput */3],
                                  /* params */state[/* params */4],
                                  /* mediaStream */state[/* mediaStream */5],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* filterBank : Some */[action[0]],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */9],
                                  /* loadedImages */state[/* loadedImages */10],
                                  /* canvasRef */state[/* canvasRef */11],
                                  /* timerId */state[/* timerId */12]
                                ],
                                (function (self) {
                                    return connectInputs(self[/* state */1]);
                                  })
                              ]);
                  case 5 : 
                      return /* Update */Block.__(0, [/* record */[
                                  /* readPos */state[/* readPos */0],
                                  /* writePos */state[/* writePos */1],
                                  /* filterInput */state[/* filterInput */2],
                                  /* visualInput */state[/* visualInput */3],
                                  /* params */action[0],
                                  /* mediaStream */state[/* mediaStream */5],
                                  /* micInput */state[/* micInput */6],
                                  /* cameraInput */state[/* cameraInput */7],
                                  /* filterBank */state[/* filterBank */8],
                                  /* analysisCanvasRef */state[/* analysisCanvasRef */9],
                                  /* loadedImages */state[/* loadedImages */10],
                                  /* canvasRef */state[/* canvasRef */11],
                                  /* timerId */state[/* timerId */12]
                                ]]);
                  
                }
              }
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var RList = 0;

export {
  RList ,
  defaultState ,
  setCanvasRef ,
  setAnalysisCanvasRef ,
  setLayerRef ,
  component ,
  maybeUpdateCanvas ,
  maybeMapFilterBank ,
  connectInputs ,
  disconnectInputs ,
  clearCanvas ,
  pushParamsState ,
  drawLayer ,
  drawCanvas ,
  make ,
  
}
/* component Not a pure module */
