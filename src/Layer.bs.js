// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Regl$Gayer from "./Regl.bs.js";
import * as Belt_Option from "bs-platform/lib/es6/belt_Option.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as ReasonReact from "reason-react/src/ReasonReact.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as MaterialUIIcons from "bs-material-ui-icons/src/MaterialUIIcons.js";
import * as ReaderType$Gayer from "./ReaderType.bs.js";
import * as KeycodeUtil$Gayer from "./KeycodeUtil.bs.js";
import * as CameraOptions$Gayer from "./CameraOptions.bs.js";
import * as AnalysisOptions$Gayer from "./AnalysisOptions.bs.js";

function readable_string_type_of_layerContent(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          return "Mouse";
      case 1 : 
          return "Webcam";
      case 2 : 
          return "MIDI Input";
      case 3 : 
          return "Histogram";
      
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
          return "Fill";
      case 1 : 
          return "Draw Commands";
      case 2 : 
          return "Draw Commands (global)";
      case 3 : 
          return "Text";
      case 4 : 
          return "Slitscan";
      case 5 : 
          return "Image";
      case 6 : 
          return "Video";
      case 7 : 
          return "Analyzer";
      case 8 : 
          return "Pitch classes";
      case 9 : 
          return "Key Reader";
      case 10 : 
          return "Key Writer";
      case 11 : 
          return "Raw Audio Writer";
      case 12 : 
          return "Raw Audio Reader";
      case 13 : 
          return "Shader";
      case 14 : 
          return "Reader";
      
    }
  }
}

function string_type_of_layerContent(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          return "mouse";
      case 1 : 
          return "webcam";
      case 2 : 
          return "midi-keyboard";
      case 3 : 
          return "histogram";
      
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
          return "fill";
      case 1 : 
          return "draw";
      case 2 : 
          return "draw-global";
      case 3 : 
          return "text";
      case 4 : 
          return "slitscan";
      case 5 : 
          return "image";
      case 6 : 
          return "video";
      case 7 : 
          return "analyzer";
      case 8 : 
          return "pitch-classes";
      case 9 : 
          return "keycode-reader";
      case 10 : 
          return "keycode-writer";
      case 11 : 
          return "raw-audio-writer";
      case 12 : 
          return "raw-audio-reader";
      case 13 : 
          return "shader";
      case 14 : 
          return "reader";
      
    }
  }
}

function icon_of_layerContent(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Brush[/* make */0](/* array */[]));
      case 1 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Videocam[/* make */0](/* array */[]));
      case 2 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.MusicNote[/* make */0](/* array */[]));
      case 3 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.ShowChart[/* make */0](/* array */[]));
      
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.FormatPaint[/* make */0](/* array */[]));
      case 1 : 
      case 2 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.FormatListBulleted[/* make */0](/* array */[]));
      case 3 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.TextFields[/* make */0](/* array */[]));
      case 4 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Flip[/* make */0](/* array */[]));
      case 5 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Image[/* make */0](/* array */[]));
      case 6 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Movie[/* make */0](/* array */[]));
      case 7 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Mic[/* make */0](/* array */[]));
      case 8 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Tonality[/* make */0](/* array */[]));
      case 9 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Textsms[/* make */0](/* array */[]));
      case 10 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Keyboard[/* make */0](/* array */[]));
      case 11 : 
      case 12 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Voicemail[/* make */0](/* array */[]));
      case 13 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Filter[/* make */0](/* array */[]));
      case 14 : 
          return ReasonReact.element(undefined, undefined, MaterialUIIcons.Speaker[/* make */0](/* array */[]));
      
    }
  }
}

var defaultLayer_000 = /* content : Fill */Block.__(0, ["black"]);

var defaultLayer = /* record */[
  defaultLayer_000,
  /* enabled */true,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* rotation */0.0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none",
  /* id */undefined
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

function rawAudioEncodingByType(type_, json) {
  switch (type_) {
    case "float" : 
        return /* Float */0;
    case "int8" : 
        return Json_decode.map((function (c) {
                      return /* Int8 */[Canvas$Gayer.channel_of_int(c)];
                    }), (function (param) {
                      return Json_decode.field("channel", Json_decode.$$int, param);
                    }), json);
    default:
      return /* Int8 */[/* R */0];
  }
}

function rawAudioEncoding(json) {
  return Json_decode.andThen(rawAudioEncodingByType, (function (param) {
                return Json_decode.field("type", Json_decode.string, param);
              }), json);
}

function rawAudioFormat(json) {
  return /* record */[
          /* x */Json_decode.field("x", Json_decode.$$int, json),
          /* y */Json_decode.field("y", Json_decode.$$int, json),
          /* w */Json_decode.field("w", Json_decode.$$int, json),
          /* h */Json_decode.field("h", Json_decode.$$int, json),
          /* encoding */Json_decode.field("encoding", rawAudioEncoding, json),
          /* sampleRate */Json_decode.field("sampleRate", Json_decode.$$int, json)
        ];
}

function layerByType(type_, json) {
  switch (type_) {
    case "analysis" : 
        var partial_arg = AnalysisOptions$Gayer.DecodeAnalysisOptions[/* analysisOptions */2];
        return Json_decode.map((function (o) {
                      return /* Analysis */Block.__(7, [o]);
                    }), (function (param) {
                      return Json_decode.field("opts", partial_arg, param);
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
    case "draw-global" : 
        var partial_arg$3 = Canvas$Gayer.DrawCommand[/* DecodeDrawCommand */2][/* command */4];
        var partial_arg$4 = function (param) {
          return Json_decode.list(partial_arg$3, param);
        };
        return Json_decode.map((function (xs) {
                      return /* DrawGlobal */Block.__(2, [xs]);
                    }), (function (param) {
                      return Json_decode.field("cmds", partial_arg$4, param);
                    }), json);
    case "fill" : 
        return Json_decode.map((function (s) {
                      return /* Fill */Block.__(0, [s]);
                    }), (function (param) {
                      return Json_decode.field("style", Json_decode.string, param);
                    }), json);
    case "hand-drawn" : 
        return /* HandDrawn */0;
    case "histogram" : 
        return /* Histogram */3;
    case "image" : 
        return Json_decode.map((function (s) {
                      return /* Image */Block.__(5, [s]);
                    }), (function (param) {
                      return Json_decode.field("url", Json_decode.string, param);
                    }), json);
    case "keycode-reader" : 
        var partial_arg$5 = KeycodeUtil$Gayer.DecodeKeycodeFormat[/* keycodeFormat */0];
        return Json_decode.map((function (o) {
                      return /* KeycodeReader */Block.__(9, [o]);
                    }), (function (param) {
                      return Json_decode.field("fmt", partial_arg$5, param);
                    }), json);
    case "keycode-writer" : 
        var partial_arg$6 = KeycodeUtil$Gayer.DecodeKeycodeFormat[/* keycodeFormat */0];
        return Json_decode.map((function (o) {
                      return /* KeycodeWriter */Block.__(10, [o]);
                    }), (function (param) {
                      return Json_decode.field("fmt", partial_arg$6, param);
                    }), json);
    case "midi-keyboard" : 
        return /* MIDIKeyboard */2;
    case "pitchClasses" : 
        return Json_decode.map((function (xs) {
                      return /* PitchClasses */Block.__(8, [Curry._1(Music$Gayer.PitchSet[/* of_list */25], xs)]);
                    }), (function (param) {
                      return Json_decode.field("pc", (function (param) {
                                    return Json_decode.list(Json_decode.$$int, param);
                                  }), param);
                    }), json);
    case "raw-audio-reader" : 
        return Json_decode.map((function (o) {
                      return /* RawAudioReader */Block.__(12, [o]);
                    }), (function (param) {
                      return Json_decode.field("format", rawAudioFormat, param);
                    }), json);
    case "raw-audio-writer" : 
        return Json_decode.map((function (o) {
                      return /* RawAudioWriter */Block.__(11, [o]);
                    }), (function (param) {
                      return Json_decode.field("format", rawAudioFormat, param);
                    }), json);
    case "reader" : 
        var partial_arg$7 = ReaderType$Gayer.DecodeReaderType[/* readerType */1];
        return Json_decode.map((function (t) {
                      return /* Reader */Block.__(14, [t]);
                    }), (function (param) {
                      return Json_decode.field("readerType", partial_arg$7, param);
                    }), json);
    case "regl" : 
        var partial_arg$8 = Regl$Gayer.DecodeReglOptions[/* reglOptions */3];
        return Json_decode.map((function (o) {
                      return /* Regl */Block.__(13, [o]);
                    }), (function (param) {
                      return Json_decode.field("options", partial_arg$8, param);
                    }), json);
    case "slitscan" : 
        var partial_arg$9 = CameraOptions$Gayer.DecodeCameraOptions[/* cameraOptions */1];
        return Json_decode.map((function (s) {
                      return /* Slitscan */Block.__(4, [s]);
                    }), (function (param) {
                      return Json_decode.field("options", partial_arg$9, param);
                    }), json);
    case "text" : 
        return Json_decode.map((function (t) {
                      return /* Text */Block.__(3, [t]);
                    }), (function (param) {
                      return Json_decode.field("text", Json_decode.string, param);
                    }), json);
    case "video" : 
        return Json_decode.map((function (s) {
                      return /* Video */Block.__(6, [s]);
                    }), (function (param) {
                      return Json_decode.field("url", Json_decode.string, param);
                    }), json);
    case "webcam" : 
        return /* Webcam */1;
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
          /* enabled */Json_decode.field("enabled", Json_decode.bool, json),
          /* alpha */Json_decode.field("alpha", Json_decode.$$float, json),
          /* compositeOperation */Json_decode.map(Canvas$Gayer.compositeOperation_of_string, (function (param) {
                  return Json_decode.field("compositeOperation", Json_decode.string, param);
                }), json),
          /* rotation */Json_decode.field("rotation", rotation, json),
          /* transformMatrix */Json_decode.field("transformMatrix", transformMatrix, json),
          /* filters */Json_decode.field("filters", Json_decode.string, json),
          /* id */Json_decode.field("id", (function (param) {
                  return Json_decode.optional(Json_decode.string, param);
                }), json)
        ];
}

var DecodeLayer = /* module */[
  /* transformMatrix */transformMatrix,
  /* rotation */rotation,
  /* rawAudioEncodingByType */rawAudioEncodingByType,
  /* rawAudioEncoding */rawAudioEncoding,
  /* rawAudioFormat */rawAudioFormat,
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

function rawAudioEncoding$1(param) {
  if (param) {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "int8"
                ],
                /* :: */[
                  /* tuple */[
                    "channel",
                    Canvas$Gayer.int_of_channel(param[0])
                  ],
                  /* [] */0
                ]
              ]);
  } else {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "float"
                ],
                /* [] */0
              ]);
  }
}

function rawAudioFormat$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "x",
                r[/* x */0]
              ],
              /* :: */[
                /* tuple */[
                  "y",
                  r[/* y */1]
                ],
                /* :: */[
                  /* tuple */[
                    "w",
                    r[/* w */2]
                  ],
                  /* :: */[
                    /* tuple */[
                      "h",
                      r[/* h */3]
                    ],
                    /* :: */[
                      /* tuple */[
                        "encoding",
                        rawAudioEncoding$1(r[/* encoding */4])
                      ],
                      /* :: */[
                        /* tuple */[
                          "sampleRate",
                          r[/* sampleRate */5]
                        ],
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
    switch (r) {
      case 0 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "hand-drawn"
                      ],
                      /* [] */0
                    ]);
      case 1 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "webcam"
                      ],
                      /* [] */0
                    ]);
      case 2 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "midi-keyboard"
                      ],
                      /* [] */0
                    ]);
      case 3 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "histogram"
                      ],
                      /* [] */0
                    ]);
      
    }
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
                        "draw-global"
                      ],
                      /* :: */[
                        /* tuple */[
                          "cmds",
                          Json_encode.list(Canvas$Gayer.DrawCommand[/* EncodeDrawCommand */1][/* command */3], r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 3 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "text"
                      ],
                      /* :: */[
                        /* tuple */[
                          "text",
                          r[0]
                        ],
                        /* [] */0
                      ]
                    ]);
      case 4 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "slitscan"
                      ],
                      /* :: */[
                        /* tuple */[
                          "options",
                          CameraOptions$Gayer.EncodeCameraOptions[/* cameraOptions */1](r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 5 : 
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
      case 6 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "video"
                      ],
                      /* :: */[
                        /* tuple */[
                          "url",
                          r[0]
                        ],
                        /* [] */0
                      ]
                    ]);
      case 7 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "analysis"
                      ],
                      /* :: */[
                        /* tuple */[
                          "opts",
                          AnalysisOptions$Gayer.EncodeAnalysisOptions[/* analysisOptions */1](r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 8 : 
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
      case 9 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "keycode-reader"
                      ],
                      /* :: */[
                        /* tuple */[
                          "fmt",
                          KeycodeUtil$Gayer.EncodeKeycodeFormat[/* keycodeFormat */0](r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 10 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "keycode-writer"
                      ],
                      /* :: */[
                        /* tuple */[
                          "fmt",
                          KeycodeUtil$Gayer.EncodeKeycodeFormat[/* keycodeFormat */0](r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 11 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "raw-audio-writer"
                      ],
                      /* :: */[
                        /* tuple */[
                          "format",
                          rawAudioFormat$1(r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 12 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "raw-audio-reader"
                      ],
                      /* :: */[
                        /* tuple */[
                          "format",
                          rawAudioFormat$1(r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 13 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "regl"
                      ],
                      /* :: */[
                        /* tuple */[
                          "options",
                          Regl$Gayer.EncodeReglOptions[/* reglOptions */2](r[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      case 14 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "reader"
                      ],
                      /* :: */[
                        /* tuple */[
                          "readerType",
                          ReaderType$Gayer.EncodeReaderType[/* readerType */0](r[0])
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
                "id",
                Json_encode.nullable((function (prim) {
                        return prim;
                      }), r[/* id */7])
              ],
              /* :: */[
                /* tuple */[
                  "content",
                  layerContent$1(r[/* content */0])
                ],
                /* :: */[
                  /* tuple */[
                    "enabled",
                    r[/* enabled */1]
                  ],
                  /* :: */[
                    /* tuple */[
                      "alpha",
                      r[/* alpha */2]
                    ],
                    /* :: */[
                      /* tuple */[
                        "compositeOperation",
                        Canvas$Gayer.string_of_compositeOperation(r[/* compositeOperation */3])
                      ],
                      /* :: */[
                        /* tuple */[
                          "transformMatrix",
                          transformMatrix$1(r[/* transformMatrix */5])
                        ],
                        /* :: */[
                          /* tuple */[
                            "rotation",
                            r[/* rotation */4]
                          ],
                          /* :: */[
                            /* tuple */[
                              "filters",
                              r[/* filters */6]
                            ],
                            /* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]);
}

var EncodeLayer = /* module */[
  /* transformMatrix */transformMatrix$1,
  /* rawAudioEncoding */rawAudioEncoding$1,
  /* rawAudioFormat */rawAudioFormat$1,
  /* layerContent */layerContent$1,
  /* rotation */rotation$1,
  /* layer */layer$1
];

function getLayerKey(layer) {
  return Belt_Option.getWithDefault(layer[/* id */7], JSON.stringify(layerContent$1(layer[/* content */0])));
}

export {
  readable_string_type_of_layerContent ,
  string_type_of_layerContent ,
  icon_of_layerContent ,
  defaultLayer ,
  oneCompleteTurnAfterNTicks ,
  DecodeLayer ,
  EncodeLayer ,
  getLayerKey ,
  
}
/* Regl-Gayer Not a pure module */
