// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as JsonifySelect$Gayer from "./JsonifySelect.bs.js";

function keycodeFormat(json) {
  return Json_decode.map((function (param) {
                switch (param) {
                  case "ascii" : 
                      return /* AsciiAsHeight */0;
                  case "pitch-filter" : 
                      return /* PitchFilter */1;
                  default:
                    return /* AsciiAsHeight */0;
                }
              }), Json_decode.string, json);
}

var DecodeKeycodeFormat = /* module */[/* keycodeFormat */keycodeFormat];

function keycodeFormat$1(param) {
  if (param) {
    return "pitch-filter";
  } else {
    return "ascii";
  }
}

var EncodeKeycodeFormat = /* module */[/* keycodeFormat */keycodeFormat$1];

var component = ReasonReact.statelessComponent("KeycodeFormatSelect");

function make(currentSetting, onChange, _) {
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
              return ReasonReact.element(undefined, undefined, JsonifySelect$Gayer.make(/* array */[
                              /* AsciiAsHeight */0,
                              /* PitchFilter */1
                            ], currentSetting, onChange, keycodeFormat$1, keycodeFormat, /* array */[]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

var KeycodeFormatSelect = /* module */[
  /* component */component,
  /* make */make
];

function keyCodeToY(height, keyCodeN) {
  return (height - ((keyCodeN - 8 | 0) << 1) | 0) - 1 | 0;
}

function yToKeyCode(height, keyCodeY) {
  return ((height - (keyCodeY + 1 | 0) | 0) / 2 | 0) + 8 | 0;
}

export {
  DecodeKeycodeFormat ,
  EncodeKeycodeFormat ,
  KeycodeFormatSelect ,
  keyCodeToY ,
  yToKeyCode ,
  
}
/* component Not a pure module */
