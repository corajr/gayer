// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";

function eventType_of_string(param) {
  switch (param) {
    case "controlchange" : 
        return /* ControlChange */3;
    case "noteoff" : 
        return /* NoteOff */1;
    case "noteon" : 
        return /* NoteOn */0;
    case "pitchbend" : 
        return /* PitchBend */2;
    default:
      return /* NoteOff */1;
  }
}

function string_of_eventType(param) {
  switch (param) {
    case 0 : 
        return "noteon";
    case 1 : 
        return "noteoff";
    case 2 : 
        return "pitchbend";
    case 3 : 
        return "controlchange";
    
  }
}

var WebMidiEventType = /* module */[
  /* eventType_of_string */eventType_of_string,
  /* string_of_eventType */string_of_eventType
];

function webMidiChannelToJs(param) {
  if (param) {
    return /* `Int */[
            3654863,
            param[0]
          ];
  } else {
    return /* `Str */[
            4153489,
            "all"
          ];
  }
}

function midiEvent_of_webMidiEvent(webMidiEvent) {
  var eventType = eventType_of_string(webMidiEvent.type);
  switch (eventType) {
    case 0 : 
        var noteNumber = webMidiEvent.note.number;
        var velocity = webMidiEvent.velocity;
        return /* NoteOn */Block.__(0, [/* tuple */[
                    noteNumber,
                    velocity
                  ]]);
    case 1 : 
        var noteNumber$1 = webMidiEvent.note.number;
        webMidiEvent.velocity;
        return /* NoteOff */Block.__(1, [/* tuple */[
                    noteNumber$1,
                    0.0
                  ]]);
    case 2 : 
        return /* NoteOff */Block.__(1, [/* tuple */[
                    0,
                    0.0
                  ]]);
    case 3 : 
        var controllerNumber = webMidiEvent.controller.number;
        var value = webMidiEvent.value;
        return /* ControlChange */Block.__(2, [/* tuple */[
                    controllerNumber,
                    value / 127.0
                  ]]);
    
  }
}

function addListener(inputOrOutput, eventType, channel, callback) {
  inputOrOutput.addListener(string_of_eventType(eventType), webMidiChannelToJs(channel)[1], (function (webMidiEvent) {
          return Curry._1(callback, midiEvent_of_webMidiEvent(webMidiEvent));
        }));
  return /* () */0;
}

function removeListener(inputOrOutput, eventType) {
  inputOrOutput.removeListener(string_of_eventType(eventType));
  return /* () */0;
}

var onWebMidiStart = function (webMIDIre){
var WebMIDI = webMIDIre;
WebMidi.enable(function () {

    // Viewing available inputs and outputs
    console.log(WebMidi.inputs);
    console.log(WebMidi.outputs);

    // Retrieve an input by name, id or index
    var input = WebMidi.getInputByName("My Awesome Keyboard");
    // OR...
    // input = WebMidi.getInputById("1809568182");
    input = WebMidi.inputs[0];

    // Listen for a 'note on' message on all channels
    input.addListener('noteon', 'all',
        function (e) {
            console.log("Received 'noteon' message (" + e.note.name + e.note.octave + ").");
        }
    );

    // Listen to pitch bend message on channel 3
    input.addListener('pitchbend', 3,
        function (e) {
            console.log("Received 'pitchbend' message.", e);
        }
    );

    // Listen to control change message on all channels
    input.addListener('controlchange', "all",
        function (e) {
            console.log("Received 'controlchange' message.", e);
        }
    );

    // Remove all listeners for 'noteoff' on all channels
    input.removeListener('noteoff');

    // Remove all listeners on the input
    input.removeListener();

});

};

export {
  WebMidiEventType ,
  webMidiChannelToJs ,
  midiEvent_of_webMidiEvent ,
  addListener ,
  removeListener ,
  onWebMidiStart ,
  
}
/* No side effect */
