// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Hashtbl from "bs-platform/lib/es6/hashtbl.js";
import * as Audio$Gayer from "./Audio.bs.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";
import * as Layer$Gayer from "./Layer.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as Container$Gayer from "./DnD/Container.bs.js";
import * as MaterialUIIcons from "bs-material-ui-icons/src/MaterialUIIcons.js";
import * as MaterialUi_Switch from "@jsiebern/bs-material-ui/src/MaterialUi_Switch.bs.js";
import * as MaterialUi_FormGroup from "@jsiebern/bs-material-ui/src/MaterialUi_FormGroup.bs.js";
import * as MaterialUi_FormLabel from "@jsiebern/bs-material-ui/src/MaterialUi_FormLabel.bs.js";
import * as MaterialUi_Typography from "@jsiebern/bs-material-ui/src/MaterialUi_Typography.bs.js";
import * as MaterialUi_FormControl from "@jsiebern/bs-material-ui/src/MaterialUi_FormControl.bs.js";
import * as MaterialUi_ExpansionPanel from "@jsiebern/bs-material-ui/src/MaterialUi_ExpansionPanel.bs.js";
import * as MaterialUi_FormControlLabel from "@jsiebern/bs-material-ui/src/MaterialUi_FormControlLabel.bs.js";
import * as MaterialUi_ExpansionPanelDetails from "@jsiebern/bs-material-ui/src/MaterialUi_ExpansionPanelDetails.bs.js";
import * as MaterialUi_ExpansionPanelSummary from "@jsiebern/bs-material-ui/src/MaterialUi_ExpansionPanelSummary.bs.js";

function params(json) {
  var partial_arg = Layer$Gayer.DecodeLayer[/* layer */2];
  return /* record */[
          /* readPosDelta */Json_decode.field("readPosDelta", Json_decode.$$int, json),
          /* writePosDelta */Json_decode.field("writePosDelta", Json_decode.$$int, json),
          /* writePosOffset */Json_decode.field("writePosOffset", Json_decode.$$int, json),
          /* audioInputSetting */Json_decode.field("audioInputSetting", Audio$Gayer.AudioInput[/* DecodeAudioInput */1][/* audioInputSetting */0], json),
          /* inputGain */Json_decode.field("inputGain", Json_decode.$$float, json),
          /* outputGain */Json_decode.field("outputGain", Json_decode.$$float, json),
          /* q */Json_decode.field("q", Json_decode.$$float, json),
          /* transpose */Json_decode.field("transpose", Json_decode.$$int, json),
          /* shouldClear */Json_decode.field("shouldClear", Json_decode.bool, json),
          /* layers */Json_decode.field("layers", (function (param) {
                  return Json_decode.list(partial_arg, param);
                }), json)
        ];
}

var DecodeParams = /* module */[/* params */params];

function params$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "readPosDelta",
                r[/* readPosDelta */0]
              ],
              /* :: */[
                /* tuple */[
                  "writePosDelta",
                  r[/* writePosDelta */1]
                ],
                /* :: */[
                  /* tuple */[
                    "writePosOffset",
                    r[/* writePosOffset */2]
                  ],
                  /* :: */[
                    /* tuple */[
                      "audioInputSetting",
                      Curry._1(Audio$Gayer.AudioInput[/* EncodeAudioInput */0][/* audioInputSetting */0], r[/* audioInputSetting */3])
                    ],
                    /* :: */[
                      /* tuple */[
                        "inputGain",
                        r[/* inputGain */4]
                      ],
                      /* :: */[
                        /* tuple */[
                          "outputGain",
                          r[/* outputGain */5]
                        ],
                        /* :: */[
                          /* tuple */[
                            "q",
                            r[/* q */6]
                          ],
                          /* :: */[
                            /* tuple */[
                              "transpose",
                              r[/* transpose */7]
                            ],
                            /* :: */[
                              /* tuple */[
                                "shouldClear",
                                r[/* shouldClear */8]
                              ],
                              /* :: */[
                                /* tuple */[
                                  "layers",
                                  Json_encode.list(Layer$Gayer.EncodeLayer[/* layer */1], r[/* layers */9])
                                ],
                                /* [] */0
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var EncodeParams = /* module */[/* params */params$1];

var component = ReasonReact.statelessComponent("Params");

function make(params, onMoveCard, onSetRef, onChangeLayer, onSetParams, _) {
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
              return React.createElement("div", undefined, ReasonReact.element(undefined, undefined, MaterialUi_ExpansionPanel.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, {
                                  marginBottom: "12px"
                                }, /* array */[
                                  ReasonReact.element(undefined, undefined, MaterialUi_ExpansionPanelSummary.make(undefined, undefined, undefined, Js_primitive.some(ReasonReact.element(undefined, undefined, MaterialUIIcons.ExpandMore[/* make */0](/* array */[]))), undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[ReasonReact.element(undefined, undefined, MaterialUi_Typography.make(undefined, undefined, /* Inherit */-72987685, undefined, undefined, undefined, undefined, undefined, /* Subheading */148169314, undefined, undefined, /* array */["Global Settings"]))])),
                                  ReasonReact.element(undefined, undefined, MaterialUi_ExpansionPanelDetails.make(undefined, undefined, undefined, /* array */[
                                            ReasonReact.element(undefined, undefined, MaterialUi_FormControl.make(undefined, /* `String */[
                                                      -976970511,
                                                      "fieldset"
                                                    ], undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[
                                                      ReasonReact.element(undefined, undefined, MaterialUi_FormLabel.make(undefined, /* `String */[
                                                                -976970511,
                                                                "legend"
                                                              ], undefined, undefined, undefined, undefined, undefined, undefined, /* array */["Graphics"])),
                                                      ReasonReact.element(undefined, undefined, MaterialUi_FormGroup.make(undefined, true, undefined, undefined, /* array */[ReasonReact.element(undefined, undefined, MaterialUi_FormControlLabel.make(undefined, undefined, Js_primitive.some(ReasonReact.element(undefined, undefined, MaterialUi_Switch.make(/* `Bool */[
                                                                                      737456202,
                                                                                      params[/* shouldClear */8]
                                                                                    ], undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, (function (_, value) {
                                                                                        return Curry._1(onSetParams, /* record */[
                                                                                                    /* readPosDelta */params[/* readPosDelta */0],
                                                                                                    /* writePosDelta */params[/* writePosDelta */1],
                                                                                                    /* writePosOffset */params[/* writePosOffset */2],
                                                                                                    /* audioInputSetting */params[/* audioInputSetting */3],
                                                                                                    /* inputGain */params[/* inputGain */4],
                                                                                                    /* outputGain */params[/* outputGain */5],
                                                                                                    /* q */params[/* q */6],
                                                                                                    /* transpose */params[/* transpose */7],
                                                                                                    /* shouldClear */value,
                                                                                                    /* layers */params[/* layers */9]
                                                                                                  ]);
                                                                                      }), undefined, "shouldClear", undefined, undefined, /* array */[]))), undefined, undefined, "Clear between frames", undefined, undefined, undefined, undefined, undefined, /* array */[]))])),
                                                      ReasonReact.element(undefined, undefined, MaterialUi_FormGroup.make(undefined, true, undefined, undefined, /* array */[React.createElement("div", undefined, "readPosDelta: ", params[/* readPosDelta */0].toString())])),
                                                      React.createElement("div", undefined, "writePosDelta: ", params[/* writePosDelta */1].toString()),
                                                      React.createElement("div", undefined, "writePosOffset: ", params[/* writePosOffset */2].toString())
                                                    ])),
                                            ReasonReact.element(undefined, undefined, MaterialUi_FormControl.make(undefined, /* `String */[
                                                      -976970511,
                                                      "fieldset"
                                                    ], undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[
                                                      ReasonReact.element(undefined, undefined, MaterialUi_FormLabel.make(undefined, /* `String */[
                                                                -976970511,
                                                                "legend"
                                                              ], undefined, undefined, undefined, undefined, undefined, undefined, /* array */["Audio"])),
                                                      React.createElement("div", undefined, "audioInputSetting: ", JSON.stringify(Curry._1(Audio$Gayer.AudioInput[/* EncodeAudioInput */0][/* audioInputSetting */0], params[/* audioInputSetting */3]))),
                                                      React.createElement("div", undefined, "inputGain: ", params[/* inputGain */4].toString()),
                                                      React.createElement("div", undefined, "outputGain: ", params[/* outputGain */5].toString()),
                                                      React.createElement("div", undefined, "q: ", params[/* q */6].toString()),
                                                      React.createElement("div", undefined, "transpose: ", params[/* transpose */7].toString())
                                                    ]))
                                          ]))
                                ])), ReasonReact.element(undefined, undefined, Container$Gayer.make(List.map((function (layer) {
                                        var id = "card" + String(Hashtbl.hash(layer));
                                        return /* record */[
                                                /* id */id,
                                                /* layer */layer
                                              ];
                                      }), params[/* layers */9]), onMoveCard, onChangeLayer, onSetRef, /* array */[])));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  DecodeParams ,
  EncodeParams ,
  component ,
  make ,
  
}
/* component Not a pure module */
