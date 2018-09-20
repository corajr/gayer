// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Webmidi from "webmidi";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as MIDI$Gayer from "./MIDI.bs.js";
import * as Pervasives from "bs-platform/lib/es6/pervasives.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as WebMIDI$Gayer from "./WebMIDI.bs.js";
import * as ImageDataUtil$Gayer from "./ImageDataUtil.bs.js";

function defaultState() {
  return /* record */[
          /* canvasRef : record */[/* contents */undefined],
          /* midiState : record */[/* contents */MIDI$Gayer.defaultState]
        ];
}

function oneRainbow(noteNumber) {
  return "hsl(" + (Pervasives.string_of_float(noteNumber * (300.0 / 127.0)) + "deg,100%,50%)");
}

function makeNoteColors(getFillStyleForNumber) {
  var noteDrawCommands = /* [] */0;
  for(var i = 127; i >= 0; --i){
    noteDrawCommands = /* :: */[
      /* FillRect */Block.__(5, [/* record */[
            /* x : Pixels */Block.__(1, [0]),
            /* y : Pixels */Block.__(1, [i]),
            /* w : Pixels */Block.__(1, [1]),
            /* h : Pixels */Block.__(1, [1])
          ]]),
      /* :: */[
        /* SetFillStyle */Block.__(3, [Curry._1(getFillStyleForNumber, i)]),
        noteDrawCommands
      ]
    ];
  }
  return noteDrawCommands;
}

function drawMidiNotesImg(canvasRenderingContext2D, state) {
  var outputImageData = ImageDataUtil$Gayer.makeImageDataFromFloats(state[/* midiState */1][0][/* notesOn */0], 1, 128);
  canvasRenderingContext2D.putImageData(outputImageData, 0, 0);
  return /* () */0;
}

function drawMidiNotes(ctx, _, noteToY, state) {
  var notesOn = state[/* midiState */1][0][/* notesOn */0];
  for(var i = 127; i >= 0; --i){
    var v = Caml_array.caml_array_get(notesOn, i);
    if (v > 0.0) {
      ctx.fillStyle = "rgba(255,255,255," + (v.toString() + ")");
      ctx.fillRect(0, Curry._1(noteToY, i), 1, 1);
    }
    
  }
  return /* () */0;
}

var component = ReasonReact.reducerComponent("MIDICanvas");

function make(height, saveRef, _) {
  var setCanvasRef = function (theRef, param) {
    param[/* state */1][/* canvasRef */0][0] = (theRef == null) ? undefined : Js_primitive.some(theRef);
    return Curry._1(saveRef, theRef);
  };
  var noteToY = function (note) {
    var pixelsPerNote = height / 120 | 0;
    return Caml_int32.imul(124 - note | 0, pixelsPerNote);
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
                      WebMIDI$Gayer.addListener(input, /* NoteOn */0, /* All */0, (function (e) {
                              return Curry._1(self[/* send */3], /* MIDIEventReceived */[e]);
                            }));
                      return WebMIDI$Gayer.addListener(input, /* NoteOff */1, /* All */0, (function (e) {
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
                          height: height.toString(),
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
                              var ctx = canvas.getContext("2d");
                              ctx.clearRect(0, 0, 1, height);
                              return drawMidiNotes(ctx, height, noteToY, state);
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
  oneRainbow ,
  makeNoteColors ,
  drawMidiNotesImg ,
  drawMidiNotes ,
  component ,
  make ,
  
}
/* component Not a pure module */
