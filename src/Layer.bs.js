// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Audio$Gayer from "./Audio.bs.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as MaterialUi_Card from "@jsiebern/bs-material-ui/src/MaterialUi_Card.bs.js";
import * as MIDICanvas$Gayer from "./MIDICanvas.bs.js";
import * as FloatSlider$Gayer from "./FloatSlider.bs.js";
import * as AnalysisCanvas$Gayer from "./AnalysisCanvas.bs.js";
import * as MaterialUi_CardMedia from "@jsiebern/bs-material-ui/src/MaterialUi_CardMedia.bs.js";
import * as MaterialUi_Typography from "@jsiebern/bs-material-ui/src/MaterialUi_Typography.bs.js";
import * as MaterialUi_CardContent from "@jsiebern/bs-material-ui/src/MaterialUi_CardContent.bs.js";
import * as CompositeOperationSelect$Gayer from "./CompositeOperationSelect.bs.js";

function slitscanOptions(json) {
  return /* record */[/* x */Json_decode.field("x", Json_decode.$$int, json)];
}

function cameraOptions(json) {
  return /* record */[/* slitscan */Json_decode.optional((function (param) {
                  return Json_decode.field("slitscan", slitscanOptions, param);
                }), json)];
}

var DecodeCameraOptions = /* module */[
  /* slitscanOptions */slitscanOptions,
  /* cameraOptions */cameraOptions
];

function slitscanOptions$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "x",
                r[/* x */0]
              ],
              /* [] */0
            ]);
}

function cameraOptions$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "slitscan",
                Json_encode.nullable(slitscanOptions$1, r[/* slitscan */0])
              ],
              /* [] */0
            ]);
}

var EncodeCameraOptions = /* module */[
  /* slitscanOptions */slitscanOptions$1,
  /* cameraOptions */cameraOptions$1
];

var defaultLayer_000 = /* content : Fill */Block.__(0, ["black"]);

var defaultLayer = /* record */[
  defaultLayer_000,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* rotation */0.0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none"
];

function oneCompleteTurnAfterNTicks(n) {
  return Canvas$Gayer.tau / n;
}

function transformMatrix(json) {
  var match = Json_decode.list(Json_decode.$$float, json);
  if (match) {
    var match$1 = match[1];
    if (match$1) {
      var match$2 = match$1[1];
      if (match$2) {
        var match$3 = match$2[1];
        if (match$3) {
          var match$4 = match$3[1];
          if (match$4) {
            var match$5 = match$4[1];
            if (match$5 && !match$5[1]) {
              return /* record */[
                      /* horizontalScaling */match[0],
                      /* horizontalSkewing */match$1[0],
                      /* verticalSkewing */match$2[0],
                      /* verticalScaling */match$3[0],
                      /* horizontalMoving */match$4[0],
                      /* verticalMoving */match$5[0]
                    ];
            } else {
              return Canvas$Gayer.defaultTransform;
            }
          } else {
            return Canvas$Gayer.defaultTransform;
          }
        } else {
          return Canvas$Gayer.defaultTransform;
        }
      } else {
        return Canvas$Gayer.defaultTransform;
      }
    } else {
      return Canvas$Gayer.defaultTransform;
    }
  } else {
    return Canvas$Gayer.defaultTransform;
  }
}

var rotation = Json_decode.$$float;

function layerByType(type_, json) {
  switch (type_) {
    case "analysis" : 
        var partial_arg = Audio$Gayer.AudioInput[/* DecodeAudioInput */1][/* audioInputSetting */0];
        return Json_decode.map((function (s) {
                      return /* Analysis */Block.__(4, [s]);
                    }), (function (param) {
                      return Json_decode.field("source", partial_arg, param);
                    }), json);
    case "draw" : 
        var partial_arg$1 = Canvas$Gayer.DrawCommand[/* DecodeDrawCommand */2][/* command */4];
        var partial_arg$2 = function (param) {
          return Json_decode.list(partial_arg$1, param);
        };
        return Json_decode.map((function (xs) {
                      return /* Draw */Block.__(1, [xs]);
                    }), (function (param) {
                      return Json_decode.field("cmds", partial_arg$2, param);
                    }), json);
    case "fill" : 
        return Json_decode.map((function (s) {
                      return /* Fill */Block.__(0, [s]);
                    }), (function (param) {
                      return Json_decode.field("style", Json_decode.string, param);
                    }), json);
    case "image" : 
        return Json_decode.map((function (s) {
                      return /* Image */Block.__(3, [s]);
                    }), (function (param) {
                      return Json_decode.field("url", Json_decode.string, param);
                    }), json);
    case "midi-keyboard" : 
        return /* MIDIKeyboard */0;
    case "pitchClasses" : 
        return Json_decode.map((function (xs) {
                      return /* PitchClasses */Block.__(5, [Curry._1(Music$Gayer.PitchSet[/* of_list */25], xs)]);
                    }), (function (param) {
                      return Json_decode.field("pc", (function (param) {
                                    return Json_decode.list(Json_decode.$$int, param);
                                  }), param);
                    }), json);
    case "reader" : 
        return Json_decode.map((function (i) {
                      return /* Reader */Block.__(6, [i]);
                    }), (function (param) {
                      return Json_decode.map(Canvas$Gayer.channel_of_int, (function (param) {
                                    return Json_decode.field("channel", Json_decode.$$int, param);
                                  }), param);
                    }), json);
    case "webcam" : 
        return Json_decode.map((function (s) {
                      return /* Webcam */Block.__(2, [s]);
                    }), (function (param) {
                      return Json_decode.field("options", cameraOptions, param);
                    }), json);
    default:
      throw [
            Json_decode.DecodeError,
            "Expected layer content, got " + JSON.stringify(json)
          ];
  }
}

function layerContent(json) {
  return Json_decode.andThen(layerByType, (function (param) {
                return Json_decode.field("type", Json_decode.string, param);
              }), json);
}

function layer(json) {
  return /* record */[
          /* content */Json_decode.field("content", layerContent, json),
          /* alpha */Json_decode.field("alpha", Json_decode.$$float, json),
          /* compositeOperation */Json_decode.map(Canvas$Gayer.compositeOperation_of_string, (function (param) {
                  return Json_decode.field("compositeOperation", Json_decode.string, param);
                }), json),
          /* rotation */Json_decode.field("rotation", rotation, json),
          /* transformMatrix */Json_decode.field("transformMatrix", transformMatrix, json),
          /* filters */Json_decode.field("filters", Json_decode.string, json)
        ];
}

var DecodeLayer = /* module */[
  /* transformMatrix */transformMatrix,
  /* rotation */rotation,
  /* layerByType */layerByType,
  /* layerContent */layerContent,
  /* layer */layer
];

function transformMatrix$1(param) {
  return Json_encode.list((function (prim) {
                return prim;
              }), /* :: */[
              param[/* horizontalScaling */0],
              /* :: */[
                param[/* horizontalSkewing */1],
                /* :: */[
                  param[/* verticalSkewing */2],
                  /* :: */[
                    param[/* verticalScaling */3],
                    /* :: */[
                      param[/* horizontalMoving */4],
                      /* :: */[
                        param[/* verticalMoving */5],
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

function layerContent$1(r) {
  if (typeof r === "number") {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "midi-keyboard"
                ],
                /* [] */0
              ]);
  } else {
    switch (r.tag | 0) {
      case 0 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "fill"
                      ],
                      /* :: */[
                        /* tuple */[
                          "style",
                          r[0]
                        ],
                        /* [] */0
                      ]
                    ]);
      case 1 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "draw"
                      ],
                      /* :: */[
                        /* tuple */[
                          "cmds",
                          Json_encode.list(Canvas$Gayer.DrawCommand[/* EncodeDrawCommand */1][/* command */3], r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 2 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "webcam"
                      ],
                      /* :: */[
                        /* tuple */[
                          "options",
                          cameraOptions$1(r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 3 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "image"
                      ],
                      /* :: */[
                        /* tuple */[
                          "url",
                          r[0]
                        ],
                        /* [] */0
                      ]
                    ]);
      case 4 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "analysis"
                      ],
                      /* :: */[
                        /* tuple */[
                          "source",
                          Curry._1(Audio$Gayer.AudioInput[/* EncodeAudioInput */0][/* audioInputSetting */0], r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 5 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "pitchClasses"
                      ],
                      /* :: */[
                        /* tuple */[
                          "pc",
                          Json_encode.list((function (prim) {
                                  return prim;
                                }), Curry._1(Music$Gayer.PitchSet[/* elements */19], r[0]))
                        ],
                        /* [] */0
                      ]
                    ]);
      case 6 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "reader"
                      ],
                      /* :: */[
                        /* tuple */[
                          "channel",
                          Canvas$Gayer.int_of_channel(r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      
    }
  }
}

function rotation$1(prim) {
  return prim;
}

function layer$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "content",
                layerContent$1(r[/* content */0])
              ],
              /* :: */[
                /* tuple */[
                  "alpha",
                  r[/* alpha */1]
                ],
                /* :: */[
                  /* tuple */[
                    "compositeOperation",
                    Canvas$Gayer.string_of_compositeOperation(r[/* compositeOperation */2])
                  ],
                  /* :: */[
                    /* tuple */[
                      "transformMatrix",
                      transformMatrix$1(r[/* transformMatrix */4])
                    ],
                    /* :: */[
                      /* tuple */[
                        "rotation",
                        r[/* rotation */3]
                      ],
                      /* :: */[
                        /* tuple */[
                          "filters",
                          r[/* filters */5]
                        ],
                        /* [] */0
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var EncodeLayer = /* module */[
  /* transformMatrix */transformMatrix$1,
  /* layerContent */layerContent$1,
  /* rotation */rotation$1,
  /* layer */layer$1
];

function renderLayerContent(layerContent$2, _, getAudio, setRef, setTick, millisPerTick, _$1, height) {
  var tmp;
  if (typeof layerContent$2 === "number") {
    tmp = ReasonReact.element(undefined, undefined, MIDICanvas$Gayer.make(setRef, /* array */[]));
  } else {
    switch (layerContent$2.tag | 0) {
      case 2 : 
          tmp = React.createElement("video", {
                ref: setRef,
                autoPlay: true,
                height: "120",
                muted: true,
                width: "120"
              });
          break;
      case 3 : 
          tmp = React.createElement("img", {
                ref: setRef,
                height: "120",
                src: layerContent$2[0],
                width: "120"
              });
          break;
      case 4 : 
          var match = Curry._1(getAudio, layerContent$2[0]);
          tmp = ReasonReact.element(undefined, undefined, AnalysisCanvas$Gayer.make(height, match[0], match[1], millisPerTick, setRef, setTick, /* array */[]));
          break;
      default:
        tmp = null;
    }
  }
  return React.createElement("div", {
              style: {
                display: "flex"
              }
            }, React.createElement("div", undefined, tmp), React.createElement("div", undefined, ReasonReact.element(undefined, undefined, MaterialUi_Typography.make(undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, undefined, /* array */[JSON.stringify(layerContent$1(layerContent$2), null, 2)]))));
}

var component = ReasonReact.statelessComponent("Layer");

function make(layer, changeLayer, $staropt$star, $staropt$star$1, getAudio, millisPerTick, width, height, _) {
  var setRef = $staropt$star !== undefined ? $staropt$star : (function () {
        return /* () */0;
      });
  var saveTick = $staropt$star$1 !== undefined ? $staropt$star$1 : (function () {
        return /* () */0;
      });
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
              return ReasonReact.element(undefined, undefined, MaterialUi_Card.make(undefined, undefined, undefined, undefined, undefined, undefined, {
                              display: "flex",
                              justifyContent: "space-between"
                            }, /* array */[
                              ReasonReact.element(undefined, undefined, MaterialUi_CardMedia.make(undefined, undefined, undefined, "dummy", undefined, undefined, /* array */[renderLayerContent(layer[/* content */0], changeLayer, getAudio, setRef, saveTick, millisPerTick, width, height)])),
                              ReasonReact.element(undefined, undefined, MaterialUi_CardContent.make(undefined, undefined, undefined, {
                                        height: "100%"
                                      }, /* array */[
                                        ReasonReact.element(undefined, undefined, FloatSlider$Gayer.make(undefined, undefined, "Alpha", layer[/* alpha */1], undefined, (function (value) {
                                                    return Curry._2(changeLayer, layer, /* record */[
                                                                /* content */layer[/* content */0],
                                                                /* alpha */value,
                                                                /* compositeOperation */layer[/* compositeOperation */2],
                                                                /* rotation */layer[/* rotation */3],
                                                                /* transformMatrix */layer[/* transformMatrix */4],
                                                                /* filters */layer[/* filters */5]
                                                              ]);
                                                  }), /* array */[])),
                                        ReasonReact.element(undefined, undefined, FloatSlider$Gayer.make(-0.5 * Canvas$Gayer.tau, 0.5 * Canvas$Gayer.tau, "Rotation", layer[/* rotation */3], 0.01, (function (value) {
                                                    return Curry._2(changeLayer, layer, /* record */[
                                                                /* content */layer[/* content */0],
                                                                /* alpha */layer[/* alpha */1],
                                                                /* compositeOperation */layer[/* compositeOperation */2],
                                                                /* rotation */value,
                                                                /* transformMatrix */layer[/* transformMatrix */4],
                                                                /* filters */layer[/* filters */5]
                                                              ]);
                                                  }), /* array */[])),
                                        React.createElement("div", undefined, ReasonReact.element(undefined, undefined, CompositeOperationSelect$Gayer.make(layer[/* compositeOperation */2], (function (newOperation) {
                                                        return Curry._2(changeLayer, layer, /* record */[
                                                                    /* content */layer[/* content */0],
                                                                    /* alpha */layer[/* alpha */1],
                                                                    /* compositeOperation */newOperation,
                                                                    /* rotation */layer[/* rotation */3],
                                                                    /* transformMatrix */layer[/* transformMatrix */4],
                                                                    /* filters */layer[/* filters */5]
                                                                  ]);
                                                      }), /* array */[])))
                                      ]))
                            ]));
            }),
          /* initialState */component[/* initialState */10],
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */component[/* reducer */12],
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

export {
  DecodeCameraOptions ,
  EncodeCameraOptions ,
  defaultLayer ,
  oneCompleteTurnAfterNTicks ,
  DecodeLayer ,
  EncodeLayer ,
  renderLayerContent ,
  component ,
  make ,
  
}
/* component Not a pure module */
