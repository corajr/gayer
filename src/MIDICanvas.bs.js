// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Webmidi from "webmidi";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as MIDI$Gayer from "./MIDI.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as WebMIDI$Gayer from "./WebMIDI.bs.js";

function defaultState() {
  return /* record */[
          /* canvasRef : record */[/* contents */undefined],
          /* midiState : record */[/* contents */MIDI$Gayer.defaultState]
        ];
}

function drawMidiNotes(canvasRenderingContext2D, state) {
  var outputImageData = Canvas$Gayer.makeImageDataFromFloats(state[/* midiState */1][0][/* notesOn */0], 1, 128);
  canvasRenderingContext2D.putImageData(outputImageData, 0, 0);
  return /* () */0;
}

var component = ReasonReact.reducerComponent("MIDICanvas");

function make(saveRef, _) {
  var setCanvasRef = function (theRef, param) {
    param[/* state */1][/* canvasRef */0][0] = (theRef == null) ? undefined : Js_primitive.some(theRef);
    return Curry._1(saveRef, theRef);
  };
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */(function (self) {
              Webmidi.enable((function () {
                      console.log("MIDI Inputs: ");
                      $$Array.iter((function (prim) {
                              console.log(prim);
                              return /* () */0;
                            }), Webmidi.inputs);
                      console.log("MIDI Outputs: ");
                      $$Array.iter((function (prim) {
                              console.log(prim);
                              return /* () */0;
                            }), Webmidi.outputs);
                      var input = Caml_array.caml_array_get(Webmidi.inputs, 1);
                      WebMIDI$Gayer.addListener(input, /* WebMidiNoteOn */0, /* All */0, (function (e) {
                              return Curry._1(self[/* send */3], /* MIDIEventReceived */[e]);
                            }));
                      return WebMIDI$Gayer.addListener(input, /* WebMidiNoteOff */1, /* All */0, (function (e) {
                                    return Curry._1(self[/* send */3], /* MIDIEventReceived */[e]);
                                  }));
                    }));
              return /* () */0;
            }),
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              return React.createElement("canvas", {
                          ref: Curry._1(self[/* handle */0], setCanvasRef),
                          height: "128",
                          width: "1"
                        });
            }),
          /* initialState */defaultState,
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              var $$event = action[0];
              var match = state[/* canvasRef */0][0];
              if (match !== undefined) {
                var canvas = Js_primitive.valFromOption(match);
                return /* SideEffects */Block.__(1, [(function () {
                              MIDI$Gayer.update(state[/* midiState */1][0], $$event);
                              console.log(state[/* midiState */1][0][/* notesOn */0]);
                              var ctx = canvas.getContext("2d");
                              return drawMidiNotes(ctx, state);
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
  defaultState ,
  drawMidiNotes ,
  component ,
  make ,
  
}
/* component Not a pure module */
