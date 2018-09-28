// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as VideoFile$Gayer from "./VideoFile.bs.js";
import * as MIDICanvas$Gayer from "./MIDICanvas.bs.js";
import * as ReglCanvas$Gayer from "./ReglCanvas.bs.js";
import * as KeycodeCanvas$Gayer from "./KeycodeCanvas.bs.js";
import * as AnalysisCanvas$Gayer from "./AnalysisCanvas.bs.js";
import * as RawAudioCanvas$Gayer from "./RawAudioCanvas.bs.js";
import * as RawAudioReader$Gayer from "./RawAudioReader.bs.js";
import * as SlitscanCanvas$Gayer from "./SlitscanCanvas.bs.js";
import * as HandDrawnCanvas$Gayer from "./HandDrawnCanvas.bs.js";
import * as HistogramCanvas$Gayer from "./HistogramCanvas.bs.js";
import * as DrawCommandCanvas$Gayer from "./DrawCommandCanvas.bs.js";
import * as KeycodeReaderCanvas$Gayer from "./KeycodeReaderCanvas.bs.js";

var component = ReasonReact.statelessComponent("LayerContent");

function make(layerKey, audioCtx, audioGraph, layerRefs, setRef, saveTick, millisPerTick, width, height, readPos, writePos, globalDrawContext, currentFilterValues, layerContent, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function () {
              var tmp;
              if (typeof layerContent === "number") {
                switch (layerContent) {
                  case 0 : 
                      tmp = ReasonReact.element(undefined, undefined, HandDrawnCanvas$Gayer.make(setRef, width, height, /* array */[]));
                      break;
                  case 1 : 
                      tmp = React.createElement("video", {
                            ref: setRef,
                            autoPlay: true,
                            muted: true
                          });
                      break;
                  case 2 : 
                      tmp = ReasonReact.element(undefined, undefined, MIDICanvas$Gayer.make(height, setRef, /* array */[]));
                      break;
                  case 3 : 
                      tmp = ReasonReact.element(undefined, undefined, HistogramCanvas$Gayer.make(setRef, layerKey, layerRefs, saveTick, height, readPos, 1, 120, /* array */[]));
                      break;
                  
                }
              } else {
                switch (layerContent.tag | 0) {
                  case 1 : 
                      tmp = ReasonReact.element(undefined, undefined, DrawCommandCanvas$Gayer.make(layerContent[0], layerKey, layerRefs, setRef, saveTick, width, height, /* array */[]));
                      break;
                  case 4 : 
                      tmp = ReasonReact.element(undefined, undefined, SlitscanCanvas$Gayer.make(setRef, layerKey, layerRefs, width, height, writePos, saveTick, globalDrawContext, layerContent[0], /* array */[]));
                      break;
                  case 5 : 
                      tmp = React.createElement("img", {
                            ref: setRef,
                            height: "120",
                            src: layerContent[0],
                            width: "120"
                          });
                      break;
                  case 6 : 
                      tmp = ReasonReact.element(undefined, undefined, VideoFile$Gayer.make(audioCtx, audioGraph, layerKey, setRef, layerContent[0], /* array */[]));
                      break;
                  case 7 : 
                      var options = layerContent[0];
                      var match = options[/* analysisSize */2];
                      var match$1;
                      var exit = 0;
                      switch (match.tag | 0) {
                        case 0 : 
                        case 1 : 
                            exit = 1;
                            break;
                        case 2 : 
                            var match$2 = match[0];
                            match$1 = /* tuple */[
                              match$2[/* w */2],
                              match$2[/* h */3]
                            ];
                            break;
                        
                      }
                      if (exit === 1) {
                        var match$3 = match[0];
                        match$1 = /* tuple */[
                          match$3[/* w */0],
                          match$3[/* h */1]
                        ];
                      }
                      var analysisWidth = Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, match$1[0]);
                      var analysisHeight = Canvas$Gayer.DrawCommand[/* getLength */3](globalDrawContext, match$1[1]);
                      tmp = ReasonReact.element(undefined, undefined, AnalysisCanvas$Gayer.make(analysisWidth, analysisHeight, layerKey, audioCtx, audioGraph, writePos, options, millisPerTick, setRef, saveTick, /* array */[]));
                      break;
                  case 9 : 
                      tmp = ReasonReact.element(undefined, undefined, KeycodeReaderCanvas$Gayer.make(layerKey, layerRefs, layerContent[0], setRef, saveTick, currentFilterValues, writePos, undefined, undefined, undefined, /* array */[]));
                      break;
                  case 10 : 
                      tmp = ReasonReact.element(undefined, undefined, KeycodeCanvas$Gayer.make(layerKey, layerRefs, layerContent[0], setRef, undefined, undefined, /* array */[]));
                      break;
                  case 11 : 
                      var match$4 = layerContent[0];
                      var h = match$4[/* h */3];
                      var w = match$4[/* w */2];
                      tmp = ReasonReact.element(undefined, undefined, RawAudioCanvas$Gayer.make(Caml_int32.imul(w, h), w, h, saveTick, layerKey, layerRefs, audioCtx, match$4[/* encoding */4], audioGraph, setRef, match$4[/* x */0], match$4[/* y */1], /* array */[]));
                      break;
                  case 12 : 
                      tmp = ReasonReact.element(undefined, undefined, RawAudioReader$Gayer.make(layerKey, layerRefs, audioCtx, audioGraph, saveTick, layerContent[0], /* array */[]));
                      break;
                  case 13 : 
                      tmp = ReasonReact.element(undefined, undefined, ReglCanvas$Gayer.make(layerRefs, layerContent[0], setRef, saveTick, layerKey, width, height, /* array */[]));
                      break;
                  default:
                    tmp = null;
                }
              }
              return React.createElement("div", {
                          key: layerKey
                        }, tmp);
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  component ,
  make ,
  
}
/* component Not a pure module */
