// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as CQT$Gayer from "./CQT.bs.js";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as Audio$Gayer from "./Audio.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as Timing$Gayer from "./Timing.bs.js";
import * as Palette$Gayer from "./Palette.bs.js";
import * as AudioGraph$Gayer from "./AudioGraph.bs.js";
import * as ImageDataUtil$Gayer from "./ImageDataUtil.bs.js";

function drawCQTBar(ctx, state, options, writePos, width, _) {
  var audioDataL = state[/* cqt */4][0].get_input_array(0);
  var audioDataR = state[/* cqt */4][0].get_input_array(1);
  state[/* analyserL */0][0].getFloatTimeDomainData(audioDataL);
  state[/* analyserR */1][0].getFloatTimeDomainData(audioDataR);
  state[/* cqt */4][0].calc();
  state[/* cqt */4][0].render_line(1);
  var cqtLine = state[/* cqt */4][0].get_output_array();
  var match = options[/* analysisSize */2];
  var xToWrite;
  xToWrite = typeof match === "number" || match.tag ? width - 1 | 0 : Caml_int32.mod_(writePos[0], width);
  var match$1 = options[/* readerType */1];
  if (match$1) {
    var outputImageData = ImageDataUtil$Gayer.makeImageData(cqtLine);
    ctx.putImageData(outputImageData, xToWrite, 0);
    return /* () */0;
  } else {
    var outputImageData$1 = ImageDataUtil$Gayer.makeImageDataWithPalette(Palette$Gayer.saturationRainbow, cqtLine);
    ctx.putImageData(outputImageData$1, xToWrite, 0);
    return /* () */0;
  }
}

var component = ReasonReact.reducerComponent("AnalysisCanvas");

function make(width, height, layerKey, audioCtx, audioGraph, writePos, options, millisPerTick, saveRef, saveTick, _) {
  var setCanvasRef = function (theRef, param) {
    param[/* state */1][/* canvasRef */5][0] = (theRef == null) ? undefined : Js_primitive.some(theRef);
    return Curry._1(saveRef, theRef);
  };
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              audioGraph[0] = AudioGraph$Gayer.updateConnections(AudioGraph$Gayer.addEdge(/* tuple */[
                        layerKey + "input",
                        layerKey,
                        0,
                        0
                      ], AudioGraph$Gayer.addNode(/* tuple */[
                            layerKey,
                            self[/* state */1][/* stereoPanner */3][0]
                          ], audioGraph[0])));
              Curry._1(self[/* onUnmount */4], (function () {
                      audioGraph[0] = AudioGraph$Gayer.updateConnections(AudioGraph$Gayer.removeAllEdgesInvolvingNode(layerKey, AudioGraph$Gayer.removeNode(layerKey, audioGraph[0])));
                      return /* () */0;
                    }));
              var match = options[/* analysisSize */2];
              var exit = 0;
              if (typeof match === "number" || match.tag !== 1) {
                exit = 1;
              } else {
                Curry._3(saveTick, self[/* onUnmount */4], layerKey, (function () {
                        var match = self[/* state */1][/* canvasRef */5][0];
                        if (match !== undefined) {
                          var canvas = Js_primitive.valFromOption(match);
                          var ctx = canvas.getContext("2d");
                          ctx.drawImage(canvas, -1, 0);
                          return /* () */0;
                        } else {
                          return /* () */0;
                        }
                      }));
              }
              if (exit === 1) {
                Curry._3(saveTick, self[/* onUnmount */4], layerKey, (function () {
                        return /* () */0;
                      }));
              }
              Timing$Gayer.setTimer(self[/* state */1][/* timerId */6], (function () {
                      var match = self[/* state */1][/* canvasRef */5][0];
                      if (match !== undefined) {
                        var ctx = Js_primitive.valFromOption(match).getContext("2d");
                        return drawCQTBar(ctx, self[/* state */1], options, writePos, width, height);
                      } else {
                        return /* () */0;
                      }
                    }), millisPerTick);
              return Curry._1(self[/* onUnmount */4], (function () {
                            return Timing$Gayer.maybeClearTimer(self[/* state */1][/* timerId */6]);
                          }));
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */(function (param) {
              var newSelf = param[/* newSelf */1];
              Timing$Gayer.maybeClearTimer(param[/* oldSelf */0][/* state */1][/* timerId */6]);
              var match = options[/* analysisSize */2];
              var exit = 0;
              if (typeof match === "number" || match.tag !== 1) {
                exit = 1;
              } else {
                Curry._3(saveTick, (function () {
                        return /* () */0;
                      }), layerKey, (function () {
                        var match = newSelf[/* state */1][/* canvasRef */5][0];
                        if (match !== undefined) {
                          var canvas = Js_primitive.valFromOption(match);
                          var ctx = canvas.getContext("2d");
                          ctx.drawImage(canvas, -1, 0);
                          return /* () */0;
                        } else {
                          return /* () */0;
                        }
                      }));
              }
              if (exit === 1) {
                Curry._3(saveTick, (function () {
                        return /* () */0;
                      }), layerKey, (function () {
                        return /* () */0;
                      }));
              }
              return Timing$Gayer.setTimer(newSelf[/* state */1][/* timerId */6], (function () {
                            var match = newSelf[/* state */1][/* canvasRef */5][0];
                            if (match !== undefined) {
                              var ctx = Js_primitive.valFromOption(match).getContext("2d");
                              return drawCQTBar(ctx, newSelf[/* state */1], options, writePos, width, height);
                            } else {
                              return /* () */0;
                            }
                          }), millisPerTick);
            }),
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              return React.createElement("canvas", {
                          ref: Curry._1(self[/* handle */0], setCanvasRef),
                          style: {
                            position: "absolute",
                            visibility: "hidden"
                          },
                          height: height.toString(),
                          width: width.toString()
                        });
            }),
          /* initialState */(function () {
              var cqt = CQT$Gayer.createShowCQTBar(/* record */[
                    /* bits */CQT$Gayer.defaultCqtBarParams[/* bits */0],
                    /* rate */audioCtx.sampleRate,
                    /* width */height,
                    /* height */CQT$Gayer.defaultCqtBarParams[/* height */3],
                    /* barVolume */CQT$Gayer.defaultCqtBarParams[/* barVolume */4],
                    /* sonogramVolume */CQT$Gayer.defaultCqtBarParams[/* sonogramVolume */5],
                    /* supersampling */CQT$Gayer.defaultCqtBarParams[/* supersampling */6]
                  ]);
              var fftSize = cqt.fft_size;
              console.log("Constant-Q transform initialized. Using FFT of size " + fftSize.toString());
              var analyserL = Audio$Gayer.makeAnalyser(audioCtx, fftSize, undefined, undefined, undefined, /* () */0);
              var analyserR = Audio$Gayer.makeAnalyser(audioCtx, fftSize, undefined, undefined, undefined, /* () */0);
              var stereoPanner = audioCtx.createStereoPanner();
              var channelSplitter = audioCtx.createChannelSplitter();
              stereoPanner.connect(channelSplitter);
              channelSplitter.connect(analyserL, 0);
              channelSplitter.connect(analyserR, 1);
              return /* record */[
                      /* analyserL : record */[/* contents */analyserL],
                      /* analyserR : record */[/* contents */analyserR],
                      /* channelSplitter : record */[/* contents */channelSplitter],
                      /* stereoPanner : record */[/* contents */stereoPanner],
                      /* cqt : record */[/* contents */cqt],
                      /* canvasRef : record */[/* contents */undefined],
                      /* timerId : record */[/* contents */undefined]
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
  drawCQTBar ,
  component ,
  make ,
  
}
/* component Not a pure module */
