// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Layer$Gayer from "./Layer.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as AudioFile$Gayer from "./AudioFile.bs.js";
import * as AudioGraph$Gayer from "./AudioGraph.bs.js";
import * as LayerContent$Gayer from "./LayerContent.bs.js";

var component = ReasonReact.statelessComponent("MediaProvider");

function make(layers, rootWidth, rootHeight, onSetRef, getAudio, audioGraph, audioCtx, layerRefs, currentFilterValues, saveTick, getReadAndWritePos, millisPerAudioTick, _) {
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
              return React.createElement("div", {
                          style: {
                            position: "absolute"
                          }
                        }, $$Array.of_list(List.map((function (layer) {
                                    var key = Layer$Gayer.getLayerKey(layer);
                                    var match = layer[/* content */0];
                                    var maybeAudio;
                                    if (typeof match === "number") {
                                      maybeAudio = null;
                                    } else {
                                      switch (match.tag | 0) {
                                        case 5 : 
                                            var source = match[0];
                                            var exit = 0;
                                            if (typeof source === "number") {
                                              exit = 1;
                                            } else {
                                              switch (source.tag | 0) {
                                                case 0 : 
                                                    maybeAudio = ReasonReact.element(undefined, undefined, AudioFile$Gayer.make(audioCtx, audioGraph, key + "input", source[0], /* array */[]));
                                                    break;
                                                case 1 : 
                                                    var input = audioCtx.createGain();
                                                    audioGraph[0] = AudioGraph$Gayer.updateConnections(AudioGraph$Gayer.addEdge(/* tuple */[
                                                              source[0],
                                                              key + "input",
                                                              0,
                                                              0
                                                            ], AudioGraph$Gayer.addNode(/* tuple */[
                                                                  key + "input",
                                                                  input
                                                                ], audioGraph[0])));
                                                    maybeAudio = null;
                                                    break;
                                                default:
                                                  exit = 1;
                                              }
                                            }
                                            if (exit === 1) {
                                              var match$1 = Curry._1(getAudio, source);
                                              var maybeInput = match$1[1];
                                              if (maybeInput !== undefined) {
                                                audioGraph[0] = AudioGraph$Gayer.updateConnections(AudioGraph$Gayer.addNode(/* tuple */[
                                                          key + "input",
                                                          maybeInput
                                                        ], audioGraph[0]));
                                                maybeAudio = null;
                                              } else {
                                                maybeAudio = null;
                                              }
                                            }
                                            break;
                                        case 7 : 
                                            var match$2 = Curry._1(getAudio, /* Mic */2);
                                            var maybeInput$1 = match$2[1];
                                            if (maybeInput$1 !== undefined) {
                                              audioGraph[0] = AudioGraph$Gayer.updateConnections(AudioGraph$Gayer.addNode(/* tuple */[
                                                        key + "input",
                                                        maybeInput$1
                                                      ], audioGraph[0]));
                                              maybeAudio = null;
                                            } else {
                                              maybeAudio = null;
                                            }
                                            break;
                                        default:
                                          maybeAudio = null;
                                      }
                                    }
                                    var match$3 = layer[/* content */0];
                                    var tmp;
                                    var exit$1 = 0;
                                    if (typeof match$3 === "number" && match$3 === 0) {
                                      tmp = {
                                        border: "1px solid black",
                                        position: "absolute",
                                        zIndex: "10"
                                      };
                                    } else {
                                      exit$1 = 1;
                                    }
                                    if (exit$1 === 1) {
                                      tmp = {
                                        position: "absolute",
                                        visibility: "hidden"
                                      };
                                    }
                                    return React.createElement("div", {
                                                key: key,
                                                style: tmp
                                              }, maybeAudio, ReasonReact.element(undefined, undefined, LayerContent$Gayer.make(key, audioCtx, audioGraph, layerRefs, Curry._1(onSetRef, layer), saveTick, millisPerAudioTick, rootWidth, rootHeight, getReadAndWritePos, currentFilterValues, layer[/* content */0], /* array */[])));
                                  }), layers)));
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
