// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as React from "react";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
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
import * as KeycodeReaderCanvas$Gayer from "./KeycodeReaderCanvas.bs.js";

var component = ReasonReact.statelessComponent("LayerContent");

function make(layerKey, audioCtx, audioGraph, layerRefs, setRef, saveTick, millisPerTick, width, height, getReadAndWritePos, layerContent, _) {
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
                      tmp = ReasonReact.element(undefined, undefined, KeycodeReaderCanvas$Gayer.make(layerKey, layerRefs, setRef, undefined, undefined, /* array */[]));
                      break;
                  case 4 : 
                      tmp = ReasonReact.element(undefined, undefined, KeycodeCanvas$Gayer.make(layerKey, layerRefs, setRef, undefined, undefined, /* array */[]));
                      break;
                  case 5 : 
                      tmp = ReasonReact.element(undefined, undefined, HistogramCanvas$Gayer.make(setRef, layerKey, layerRefs, saveTick, height, getReadAndWritePos, 1, 120, /* array */[]));
                      break;
                  case 6 : 
                      tmp = ReasonReact.element(undefined, undefined, ReglCanvas$Gayer.make(layerRefs, setRef, saveTick, layerKey, width, height, /* array */[]));
                      break;
                  
                }
              } else {
                switch (layerContent.tag | 0) {
                  case 2 : 
                      tmp = ReasonReact.element(undefined, undefined, SlitscanCanvas$Gayer.make(setRef, layerKey, layerRefs, layerContent[0][/* sourceLayerKey */0], width, height, saveTick, /* array */[]));
                      break;
                  case 3 : 
                      tmp = React.createElement("img", {
                            ref: setRef,
                            height: "120",
                            src: layerContent[0],
                            width: "120"
                          });
                      break;
                  case 4 : 
                      tmp = ReasonReact.element(undefined, undefined, VideoFile$Gayer.make(audioCtx, audioGraph, layerKey, setRef, layerContent[0], /* array */[]));
                      break;
                  case 5 : 
                      tmp = ReasonReact.element(undefined, undefined, AnalysisCanvas$Gayer.make(height, layerKey, audioCtx, audioGraph, layerContent[0], millisPerTick, setRef, saveTick, /* array */[]));
                      break;
                  case 7 : 
                      var match = layerContent[0];
                      var h = match[/* h */3];
                      var w = match[/* w */2];
                      tmp = ReasonReact.element(undefined, undefined, RawAudioCanvas$Gayer.make(Caml_int32.imul(w, h), w, h, saveTick, layerKey, layerRefs, audioCtx, match[/* encoding */4], audioGraph, setRef, match[/* x */0], match[/* y */1], /* array */[]));
                      break;
                  case 8 : 
                      tmp = ReasonReact.element(undefined, undefined, RawAudioReader$Gayer.make(layerKey, layerRefs, audioCtx, audioGraph, saveTick, layerContent[0], /* array */[]));
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
