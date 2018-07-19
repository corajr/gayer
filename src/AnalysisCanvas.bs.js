// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as CQT$Gayer from "./CQT.bs.js";
import * as Audio$Gayer from "./Audio.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Canvas$Gayer from "./Canvas.bs.js";

function drawCQTBar(canvasRenderingContext2D, state) {
  var audioData = state[/* cqt */1].get_input_array(0);
  state[/* analyser */0].getFloatTimeDomainData(audioData);
  state[/* cqt */1].calc();
  state[/* cqt */1].render_line(1);
  var cqtLine = state[/* cqt */1].get_output_array();
  var outputImageData = Canvas$Gayer.makeImageData(cqtLine);
  canvasRenderingContext2D.putImageData(outputImageData, 0, 0);
  return /* () */0;
}

var component = ReasonReact.reducerComponent("AnalysisCanvas");

function make(size, audioCtx, input, saveRef, _) {
  var setCanvasRef = function (theRef, param) {
    param[/* state */1][/* canvasRef */2][0] = (theRef == null) ? /* None */0 : [theRef];
    return Curry._1(saveRef, theRef);
  };
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              self[/* state */1][/* timerId */3][0] = /* Some */[setInterval((function () {
                        return Curry._1(self[/* send */3], /* Draw */0);
                      }), 15)];
              return /* () */0;
            }),
          /* didUpdate */(function (param) {
              if (input) {
                input[0].connect(param[/* newSelf */1][/* state */1][/* analyser */0]);
                return /* () */0;
              } else {
                return /* () */0;
              }
            }),
          /* willUnmount */(function (self) {
              if (input) {
                input[0].disconnect(self[/* state */1][/* analyser */0]);
                return /* () */0;
              } else {
                return /* () */0;
              }
            }),
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              return React.createElement("canvas", {
                          ref: Curry._1(self[/* handle */0], setCanvasRef),
                          style: {
                            position: "absolute",
                            visibility: "hidden"
                          },
                          height: size.toString(),
                          width: "1"
                        });
            }),
          /* initialState */(function () {
              var cqt = CQT$Gayer.createShowCQTBar(/* record */[
                    /* rate */audioCtx.sampleRate,
                    /* width */size,
                    /* height */CQT$Gayer.defaultCqtBarParams[/* height */2],
                    /* barVolume */CQT$Gayer.defaultCqtBarParams[/* barVolume */3],
                    /* sonogramVolume */CQT$Gayer.defaultCqtBarParams[/* sonogramVolume */4],
                    /* supersampling */CQT$Gayer.defaultCqtBarParams[/* supersampling */5]
                  ]);
              var analyser = Audio$Gayer.makeAnalyser(/* Some */[audioCtx], /* Some */[cqt.fft_size], /* None */0, /* None */0, /* None */0, /* () */0);
              return /* record */[
                      /* analyser */analyser,
                      /* cqt */cqt,
                      /* canvasRef */[/* None */0],
                      /* timerId */[/* None */0]
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (_, state) {
              var match = state[/* canvasRef */2][0];
              if (match) {
                var canvas = match[0];
                return /* SideEffects */Block.__(1, [(function () {
                              var ctx = canvas.getContext("2d");
                              return drawCQTBar(ctx, state);
                            })]);
              } else {
                return /* NoUpdate */0;
              }
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
