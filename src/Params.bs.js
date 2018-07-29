// Generated by BUCKLESCRIPT VERSION 3.1.5, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as React from "react";
import * as Hashtbl from "bs-platform/lib/es6/hashtbl.js";
import * as Audio$Gayer from "./Audio.bs.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";
import * as Layer$Gayer from "./Layer.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Container$Gayer from "./DnD/Container.bs.js";

var defaultParams_008 = /* layers : :: */[
  /* record */[
    /* content : Analysis */1,
    /* alpha */1.0,
    /* compositeOperation : SourceOver */0
  ],
  /* :: */[
    /* record */[
      /* content : Webcam */0,
      /* alpha */0.25,
      /* compositeOperation : Overlay */13
    ],
    /* :: */[
      /* record */[
        /* content : Image */Block.__(0, ["media/DeadFishSwimming.gif"]),
        /* alpha */0.0,
        /* compositeOperation : Multiply */11
      ],
      /* :: */[
        /* record */[
          /* content : PitchClasses */Block.__(1, [Music$Gayer.cMajor]),
          /* alpha */1.0,
          /* compositeOperation : DestinationOut */6
        ],
        /* :: */[
          /* record */[
            /* content : Reader */Block.__(2, [/* R */0]),
            /* alpha */0.0,
            /* compositeOperation : SourceOver */0
          ],
          /* [] */0
        ]
      ]
    ]
  ]
];

var defaultParams = /* record */[
  /* readPosDelta */1,
  /* writePosDelta */1,
  /* writePosOffset */0,
  /* inputGain */1.0,
  /* outputGain */0.2,
  /* q */Audio$Gayer.defaultQ,
  /* transpose */0,
  /* shouldClear */false,
  defaultParams_008
];

function params(json) {
  var partial_arg = Layer$Gayer.DecodeLayer[/* layer */2];
  return /* record */[
          /* readPosDelta */Json_decode.field("readPosDelta", Json_decode.$$int, json),
          /* writePosDelta */Json_decode.field("writePosDelta", Json_decode.$$int, json),
          /* writePosOffset */Json_decode.field("writePosOffset", Json_decode.$$int, json),
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
                      "inputGain",
                      r[/* inputGain */3]
                    ],
                    /* :: */[
                      /* tuple */[
                        "outputGain",
                        r[/* outputGain */4]
                      ],
                      /* :: */[
                        /* tuple */[
                          "q",
                          r[/* q */5]
                        ],
                        /* :: */[
                          /* tuple */[
                            "transpose",
                            r[/* transpose */6]
                          ],
                          /* :: */[
                            /* tuple */[
                              "shouldClear",
                              r[/* shouldClear */7]
                            ],
                            /* :: */[
                              /* tuple */[
                                "layers",
                                Json_encode.list(Layer$Gayer.EncodeLayer[/* layer */1], r[/* layers */8])
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
            ]);
}

var EncodeParams = /* module */[/* params */params$1];

var component = ReasonReact.statelessComponent("Params");

function make(params, onMoveCard, _) {
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
              var match = params[/* shouldClear */7];
              return React.createElement("div", undefined, ReasonReact.element(/* None */0, /* None */0, Container$Gayer.make(List.map((function (layer) {
                                        var id = Hashtbl.hash(layer);
                                        return /* record */[
                                                /* id */id,
                                                /* layer */layer
                                              ];
                                      }), params[/* layers */8]), onMoveCard, /* array */[])), React.createElement("div", undefined, React.createElement("div", undefined, "readPosDelta: ", params[/* readPosDelta */0].toString()), React.createElement("div", undefined, "writePosDelta: ", params[/* writePosDelta */1].toString()), React.createElement("div", undefined, "writePosOffset: ", params[/* writePosOffset */2].toString()), React.createElement("div", undefined, "inputGain: ", params[/* inputGain */3].toString()), React.createElement("div", undefined, "outputGain: ", params[/* outputGain */4].toString()), React.createElement("div", undefined, "q: ", params[/* q */5].toString()), React.createElement("div", undefined, "transpose: ", params[/* transpose */6].toString()), React.createElement("div", undefined, "shouldClear: ", match ? "true" : "false")));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  defaultParams ,
  DecodeParams ,
  EncodeParams ,
  component ,
  make ,
  
}
/* component Not a pure module */
