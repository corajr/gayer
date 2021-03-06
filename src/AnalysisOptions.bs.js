// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Audio$Gayer from "./Audio.bs.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";
import * as ReaderType$Gayer from "./ReaderType.bs.js";
import * as DrawCommand$Gayer from "./DrawCommand.bs.js";

function string_of_analysisSize(param) {
  if (typeof param === "number") {
    return "slit";
  } else {
    switch (param.tag | 0) {
      case 0 : 
          return "circular buffer";
      case 1 : 
          return "history";
      case 2 : 
          return "";
      
    }
  }
}

function analysisSize_of_string(param) {
  switch (param) {
    case "circular buffer" : 
        return /* CircularBuffer */Block.__(0, [/* record */[
                    /* w : Width */0,
                    /* h : Height */1
                  ]]);
    case "slit" : 
        return /* Slit */0;
    default:
      return /* History */Block.__(1, [/* record */[
                  /* w : Width */0,
                  /* h : Height */1
                ]]);
  }
}

function analysisSizeByType(type_, json) {
  switch (type_) {
    case "circular-buffer" : 
        return DrawCommand$Gayer.field2((function (w, h) {
                      return /* CircularBuffer */Block.__(0, [/* record */[
                                  /* w */w,
                                  /* h */h
                                ]]);
                    }), "w", DrawCommand$Gayer.DecodeDrawCommand[/* length */1], "h", DrawCommand$Gayer.DecodeDrawCommand[/* length */1], json);
    case "dest-rect" : 
        var partial_arg = DrawCommand$Gayer.DecodeDrawCommand[/* rect */2];
        return Json_decode.map((function (r) {
                      return /* DestRect */Block.__(2, [r]);
                    }), (function (param) {
                      return Json_decode.field("rect", partial_arg, param);
                    }), json);
    case "history" : 
        return DrawCommand$Gayer.field2((function (w, h) {
                      return /* History */Block.__(1, [/* record */[
                                  /* w */w,
                                  /* h */h
                                ]]);
                    }), "w", DrawCommand$Gayer.DecodeDrawCommand[/* length */1], "h", DrawCommand$Gayer.DecodeDrawCommand[/* length */1], json);
    case "slit" : 
        return /* Slit */0;
    default:
      return /* History */Block.__(1, [/* record */[
                  /* w : Width */0,
                  /* h : Height */1
                ]]);
  }
}

function analysisSize(json) {
  return Json_decode.andThen(analysisSizeByType, (function (param) {
                return Json_decode.field("type", Json_decode.string, param);
              }), json);
}

function analysisOptions(json) {
  return /* record */[
          /* input */Json_decode.field("input", Audio$Gayer.AudioInput[/* DecodeAudioInput */1][/* audioInputSetting */0], json),
          /* readerType */Json_decode.field("readerType", ReaderType$Gayer.DecodeReaderType[/* readerType */1], json),
          /* analysisSize */Json_decode.field("analysisSize", analysisSize, json)
        ];
}

var DecodeAnalysisOptions = /* module */[
  /* analysisSizeByType */analysisSizeByType,
  /* analysisSize */analysisSize,
  /* analysisOptions */analysisOptions
];

function analysisSize$1(param) {
  if (typeof param === "number") {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "slit"
                ],
                /* [] */0
              ]);
  } else {
    switch (param.tag | 0) {
      case 0 : 
          var match = param[0];
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "circular-buffer"
                      ],
                      /* :: */[
                        /* tuple */[
                          "w",
                          DrawCommand$Gayer.EncodeDrawCommand[/* length */1](match[/* w */0])
                        ],
                        /* :: */[
                          /* tuple */[
                            "h",
                            DrawCommand$Gayer.EncodeDrawCommand[/* length */1](match[/* h */1])
                          ],
                          /* [] */0
                        ]
                      ]
                    ]);
      case 1 : 
          var match$1 = param[0];
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "history"
                      ],
                      /* :: */[
                        /* tuple */[
                          "w",
                          DrawCommand$Gayer.EncodeDrawCommand[/* length */1](match$1[/* w */0])
                        ],
                        /* :: */[
                          /* tuple */[
                            "h",
                            DrawCommand$Gayer.EncodeDrawCommand[/* length */1](match$1[/* h */1])
                          ],
                          /* [] */0
                        ]
                      ]
                    ]);
      case 2 : 
          return Json_encode.object_(/* :: */[
                      /* tuple */[
                        "type",
                        "dest-rect"
                      ],
                      /* :: */[
                        /* tuple */[
                          "rect",
                          DrawCommand$Gayer.EncodeDrawCommand[/* rect */2](param[0])
                        ],
                        /* [] */0
                      ]
                    ]);
      
    }
  }
}

function analysisOptions$1(r) {
  return Json_encode.object_(/* :: */[
              /* tuple */[
                "input",
                Curry._1(Audio$Gayer.AudioInput[/* EncodeAudioInput */0][/* audioInputSetting */0], r[/* input */0])
              ],
              /* :: */[
                /* tuple */[
                  "readerType",
                  ReaderType$Gayer.EncodeReaderType[/* readerType */0](r[/* readerType */1])
                ],
                /* :: */[
                  /* tuple */[
                    "analysisSize",
                    analysisSize$1(r[/* analysisSize */2])
                  ],
                  /* [] */0
                ]
              ]
            ]);
}

var EncodeAnalysisOptions = /* module */[
  /* analysisSize */analysisSize$1,
  /* analysisOptions */analysisOptions$1
];

var defaultAnalysisOptions = /* record */[
  /* input : Mic */2,
  /* readerType : Channel */[/* R */0],
  /* analysisSize : Slit */0
];

var destRect = /* DestRect */Block.__(2, [/* record */[
      /* x : Add */Block.__(5, [
          /* Width */0,
          /* Pixels */Block.__(2, [-1])
        ]),
      /* y : Pixels */Block.__(2, [0]),
      /* w : Pixels */Block.__(2, [1]),
      /* h : Height */1
    ]]);

export {
  string_of_analysisSize ,
  analysisSize_of_string ,
  defaultAnalysisOptions ,
  destRect ,
  DecodeAnalysisOptions ,
  EncodeAnalysisOptions ,
  
}
/* Audio-Gayer Not a pure module */
