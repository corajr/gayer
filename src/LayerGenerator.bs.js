// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Layer$Gayer from "./Layer.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as MIDICanvas$Gayer from "./MIDICanvas.bs.js";
import * as AnalysisOptions$Gayer from "./AnalysisOptions.bs.js";

function img(url) {
  return /* record */[
          /* content : Image */Block.__(4, [url]),
          /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */2],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
          /* filters */Layer$Gayer.defaultLayer[/* filters */6],
          /* id */Layer$Gayer.defaultLayer[/* id */7]
        ];
}

function video(url) {
  return /* record */[
          /* content : Video */Block.__(5, [url]),
          /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */2],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
          /* filters */Layer$Gayer.defaultLayer[/* filters */6],
          /* id */Layer$Gayer.defaultLayer[/* id */7]
        ];
}

var reader_000 = /* content : Reader */Block.__(13, [/* Channel */[/* R */0]]);

var reader_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var reader_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var reader_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var reader_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var reader_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var reader_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var reader_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var reader = /* record */[
  reader_000,
  reader_001,
  reader_002,
  reader_003,
  reader_004,
  reader_005,
  reader_006,
  reader_007
];

var saturationReader_000 = /* content : Reader */Block.__(13, [/* Saturation */0]);

var saturationReader_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var saturationReader_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var saturationReader_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var saturationReader_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var saturationReader_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var saturationReader_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var saturationReader_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var saturationReader = /* record */[
  saturationReader_000,
  saturationReader_001,
  saturationReader_002,
  saturationReader_003,
  saturationReader_004,
  saturationReader_005,
  saturationReader_006,
  saturationReader_007
];

var keycodeReader_000 = /* content : KeycodeReader */Block.__(8, [/* AsciiAsHeight */0]);

var keycodeReader_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var keycodeReader_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var keycodeReader_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var keycodeReader_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var keycodeReader_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var keycodeReader_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var keycodeReader_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var keycodeReader = /* record */[
  keycodeReader_000,
  keycodeReader_001,
  keycodeReader_002,
  keycodeReader_003,
  keycodeReader_004,
  keycodeReader_005,
  keycodeReader_006,
  keycodeReader_007
];

var keycodeWriter_000 = /* content : KeycodeWriter */Block.__(9, [/* AsciiAsHeight */0]);

var keycodeWriter_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var keycodeWriter_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var keycodeWriter_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var keycodeWriter_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var keycodeWriter_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var keycodeWriter_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var keycodeWriter_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var keycodeWriter = /* record */[
  keycodeWriter_000,
  keycodeWriter_001,
  keycodeWriter_002,
  keycodeWriter_003,
  keycodeWriter_004,
  keycodeWriter_005,
  keycodeWriter_006,
  keycodeWriter_007
];

var histogram_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var histogram_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var histogram_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var histogram_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var histogram_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var histogram = /* record */[
  /* content : Histogram */3,
  histogram_001,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  histogram_004,
  histogram_005,
  histogram_006,
  histogram_007
];

var rawAudioFormat = /* record */[
  /* x */0,
  /* y */0,
  /* w */64,
  /* h */64,
  /* encoding : Int8 */[/* R */0],
  /* sampleRate */44100
];

var rawAudioWriter_000 = /* content : RawAudioWriter */Block.__(10, [rawAudioFormat]);

var rawAudioWriter_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var rawAudioWriter_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var rawAudioWriter_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var rawAudioWriter_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var rawAudioWriter_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var rawAudioWriter_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var rawAudioWriter_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var rawAudioWriter = /* record */[
  rawAudioWriter_000,
  rawAudioWriter_001,
  rawAudioWriter_002,
  rawAudioWriter_003,
  rawAudioWriter_004,
  rawAudioWriter_005,
  rawAudioWriter_006,
  rawAudioWriter_007
];

var rawAudioReader_000 = /* content : RawAudioReader */Block.__(11, [rawAudioFormat]);

var rawAudioReader_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var rawAudioReader_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var rawAudioReader_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var rawAudioReader_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var rawAudioReader_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var rawAudioReader_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var rawAudioReader_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var rawAudioReader = /* record */[
  rawAudioReader_000,
  rawAudioReader_001,
  rawAudioReader_002,
  rawAudioReader_003,
  rawAudioReader_004,
  rawAudioReader_005,
  rawAudioReader_006,
  rawAudioReader_007
];

function pitchFilter(pc) {
  return /* record */[
          /* content : PitchClasses */Block.__(7, [pc]),
          /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */2],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
          /* filters */Layer$Gayer.defaultLayer[/* filters */6],
          /* id */Layer$Gayer.defaultLayer[/* id */7]
        ];
}

function fill($staropt$star, fillStyle) {
  var alpha = $staropt$star !== undefined ? $staropt$star : 1.0;
  return /* record */[
          /* content : Fill */Block.__(0, [fillStyle]),
          /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
          /* alpha */alpha,
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
          /* filters */Layer$Gayer.defaultLayer[/* filters */6],
          /* id */Layer$Gayer.defaultLayer[/* id */7]
        ];
}

function drawGlobal($staropt$star, cmds) {
  var alpha = $staropt$star !== undefined ? $staropt$star : 1.0;
  return /* record */[
          /* content : DrawGlobal */Block.__(2, [cmds]),
          /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
          /* alpha */alpha,
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
          /* filters */Layer$Gayer.defaultLayer[/* filters */6],
          /* id */Layer$Gayer.defaultLayer[/* id */7]
        ];
}

function draw($staropt$star, cmds) {
  var alpha = $staropt$star !== undefined ? $staropt$star : 1.0;
  return /* record */[
          /* content : Draw */Block.__(1, [cmds]),
          /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
          /* alpha */alpha,
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
          /* filters */Layer$Gayer.defaultLayer[/* filters */6],
          /* id */Layer$Gayer.defaultLayer[/* id */7]
        ];
}

function text($staropt$star, $staropt$star$1, $staropt$star$2, $staropt$star$3, $staropt$star$4, $staropt$star$5, $staropt$star$6, s) {
  var x = $staropt$star !== undefined ? $staropt$star : /* Divide */Block.__(6, [
        /* Width */0,
        /* Constant */Block.__(0, [2])
      ]);
  var y = $staropt$star$1 !== undefined ? $staropt$star$1 : /* Divide */Block.__(6, [
        /* Height */1,
        /* Constant */Block.__(0, [2])
      ]);
  var font = $staropt$star$2 !== undefined ? $staropt$star$2 : "48px monospace";
  var align = $staropt$star$3 !== undefined ? $staropt$star$3 : "center";
  var baseline = $staropt$star$4 !== undefined ? $staropt$star$4 : "middle";
  var color = $staropt$star$5 !== undefined ? $staropt$star$5 : "white";
  var fillOrStroke = $staropt$star$6 !== undefined ? $staropt$star$6 : /* Fill */0;
  var drawTextCommands = fillOrStroke ? /* :: */[
      /* SetStrokeStyle */Block.__(4, [color]),
      /* :: */[
        /* StrokeText */Block.__(7, [
            s,
            x,
            y
          ]),
        /* [] */0
      ]
    ] : /* :: */[
      /* SetFillStyle */Block.__(3, [color]),
      /* :: */[
        /* FillText */Block.__(6, [
            s,
            x,
            y
          ]),
        /* [] */0
      ]
    ];
  return drawGlobal(undefined, /* :: */[
              /* SetFont */Block.__(0, [font]),
              /* :: */[
                /* SetTextAlign */Block.__(1, [align]),
                /* :: */[
                  /* SetTextBaseline */Block.__(2, [baseline]),
                  drawTextCommands
                ]
              ]
            ]);
}

function sobel(key) {
  return /* record */[
          /* content : Regl */Block.__(12, [/* Sobel */Block.__(0, [/* record */[/* sourceLayer */key]])]),
          /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */2],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
          /* filters */Layer$Gayer.defaultLayer[/* filters */6],
          /* id */Layer$Gayer.defaultLayer[/* id */7]
        ];
}

function displace(source, displace$1) {
  return /* record */[
          /* content : Regl */Block.__(12, [/* Displacement */Block.__(1, [/* record */[
                    /* displacementSourceLayer */source,
                    /* displacementMap */displace$1
                  ]])]),
          /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */2],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
          /* filters */Layer$Gayer.defaultLayer[/* filters */6],
          /* id */Layer$Gayer.defaultLayer[/* id */7]
        ];
}

function analyzer($staropt$star, $staropt$star$1, $staropt$star$2, input) {
  var includeHistory = $staropt$star !== undefined ? $staropt$star : true;
  var readerType = $staropt$star$1 !== undefined ? $staropt$star$1 : /* Channel */[/* R */0];
  var analysisSize = $staropt$star$2 !== undefined ? $staropt$star$2 : /* WithHistory */Block.__(0, [/* record */[
          /* w : Width */0,
          /* h : Height */1
        ]]);
  if (includeHistory) {
    return /* record */[
            /* content : Analysis */Block.__(6, [/* record */[
                  /* input */input,
                  /* readerType */readerType,
                  /* analysisSize */analysisSize
                ]]),
            /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
            /* alpha */Layer$Gayer.defaultLayer[/* alpha */2],
            /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
            /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
            /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
            /* filters */Layer$Gayer.defaultLayer[/* filters */6],
            /* id */Layer$Gayer.defaultLayer[/* id */7]
          ];
  } else {
    return /* record */[
            /* content : Analysis */Block.__(6, [/* record */[
                  /* input */input,
                  /* readerType */AnalysisOptions$Gayer.defaultAnalysisOptions[/* readerType */1],
                  /* analysisSize */AnalysisOptions$Gayer.defaultAnalysisOptions[/* analysisSize */2]
                ]]),
            /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
            /* alpha */Layer$Gayer.defaultLayer[/* alpha */2],
            /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
            /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
            /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
            /* filters */Layer$Gayer.defaultLayer[/* filters */6],
            /* id */Layer$Gayer.defaultLayer[/* id */7]
          ];
  }
}

var webcam_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var webcam_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var webcam_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var webcam_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var webcam_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var webcam_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var webcam_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var webcam = /* record */[
  /* content : Webcam */1,
  webcam_001,
  webcam_002,
  webcam_003,
  webcam_004,
  webcam_005,
  webcam_006,
  webcam_007
];

var slitscan_000 = /* content : Slitscan */Block.__(3, [/* record */[
      /* sourceLayerKey */"webcam",
      /* slitscan : StaticX */Block.__(0, [320])
    ]]);

var slitscan_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var slitscan_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var slitscan_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var slitscan_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var slitscan_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var slitscan_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var slitscan_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var slitscan = /* record */[
  slitscan_000,
  slitscan_001,
  slitscan_002,
  slitscan_003,
  slitscan_004,
  slitscan_005,
  slitscan_006,
  slitscan_007
];

var hubble = img("media/hubble_ultra_deep_field.jpg");

var spacy_001 = /* :: */[
  pitchFilter(Music$Gayer.cMajor),
  /* :: */[
    reader,
    /* [] */0
  ]
];

var spacy = /* :: */[
  hubble,
  spacy_001
];

var drawSelfFullScreen = /* DrawImage */Block.__(10, [
    /* Self */0,
    /* record */[
      /* x : Pixels */Block.__(2, [0]),
      /* y : Pixels */Block.__(2, [0]),
      /* w : Width */0,
      /* h : Height */1
    ]
  ]);

var init = img("media/harmony.png");

var init$1 = drawGlobal(undefined, /* :: */[
      /* DrawImage */Block.__(10, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(2, [0]),
            /* y : Pixels */Block.__(2, [-24]),
            /* w : Width */0,
            /* h : Height */1
          ]
        ]),
      /* :: */[
        /* DrawImage */Block.__(10, [
            /* Self */0,
            /* record */[
              /* x : Pixels */Block.__(2, [0]),
              /* y : Pixels */Block.__(2, [-48]),
              /* w : Width */0,
              /* h : Height */1
            ]
          ]),
        /* [] */0
      ]
    ]);

var harmony_000 = /* record */[
  /* content */init[/* content */0],
  /* enabled */init[/* enabled */1],
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* rotation */init[/* rotation */4],
  /* transformMatrix : record */[
    /* horizontalScaling */Canvas$Gayer.defaultTransform[/* horizontalScaling */0],
    /* horizontalSkewing */Canvas$Gayer.defaultTransform[/* horizontalSkewing */1],
    /* verticalSkewing */Canvas$Gayer.defaultTransform[/* verticalSkewing */2],
    /* verticalScaling */Canvas$Gayer.defaultTransform[/* verticalScaling */3],
    /* horizontalMoving */Canvas$Gayer.defaultTransform[/* horizontalMoving */4],
    /* verticalMoving */48.0
  ],
  /* filters */"blur(2px)",
  /* id */init[/* id */7]
];

var harmony_001 = /* :: */[
  sobel("root"),
  /* :: */[
    /* record */[
      /* content */init$1[/* content */0],
      /* enabled */init$1[/* enabled */1],
      /* alpha */0.5,
      /* compositeOperation : Difference */20,
      /* rotation */init$1[/* rotation */4],
      /* transformMatrix */init$1[/* transformMatrix */5],
      /* filters */init$1[/* filters */6],
      /* id */init$1[/* id */7]
    ],
    /* :: */[
      pitchFilter(Music$Gayer.cSharpMajor),
      /* :: */[
        reader,
        /* [] */0
      ]
    ]
  ]
];

var harmony = /* :: */[
  harmony_000,
  harmony_001
];

function rotateLayer(rotation) {
  return drawGlobal(undefined, /* :: */[
              /* Translate */Block.__(9, [
                  /* Divide */Block.__(6, [
                      /* Width */0,
                      /* Constant */Block.__(0, [2])
                    ]),
                  /* Divide */Block.__(6, [
                      /* Height */1,
                      /* Constant */Block.__(0, [2])
                    ])
                ]),
              /* :: */[
                /* Rotate */Block.__(8, [rotation]),
                /* :: */[
                  /* Translate */Block.__(9, [
                      /* Negate */Block.__(4, [/* Divide */Block.__(6, [
                              /* Width */0,
                              /* Constant */Block.__(0, [2])
                            ])]),
                      /* Negate */Block.__(4, [/* Divide */Block.__(6, [
                              /* Width */0,
                              /* Constant */Block.__(0, [2])
                            ])])
                    ]),
                  /* :: */[
                    drawSelfFullScreen,
                    /* [] */0
                  ]
                ]
              ]
            ]);
}

var init$2 = drawGlobal(undefined, /* :: */[
      drawSelfFullScreen,
      /* [] */0
    ]);

var blurLayer_000 = /* content */init$2[/* content */0];

var blurLayer_001 = /* enabled */init$2[/* enabled */1];

var blurLayer_002 = /* alpha */init$2[/* alpha */2];

var blurLayer_003 = /* compositeOperation */init$2[/* compositeOperation */3];

var blurLayer_004 = /* rotation */init$2[/* rotation */4];

var blurLayer_005 = /* transformMatrix */init$2[/* transformMatrix */5];

var blurLayer_007 = /* id */init$2[/* id */7];

var blurLayer = /* record */[
  blurLayer_000,
  blurLayer_001,
  blurLayer_002,
  blurLayer_003,
  blurLayer_004,
  blurLayer_005,
  /* filters */"blur(2px)",
  blurLayer_007
];

var init$3 = drawGlobal(undefined, /* :: */[
      /* DrawImageSourceDest */Block.__(11, [
          /* Self */0,
          /* record */[
            /* x : Add */Block.__(5, [
                /* Width */0,
                /* Pixels */Block.__(2, [-1])
              ]),
            /* y : Pixels */Block.__(2, [0]),
            /* w : Pixels */Block.__(2, [1]),
            /* h : Height */1
          ],
          /* record */[
            /* x : Add */Block.__(5, [
                /* Width */0,
                /* Pixels */Block.__(2, [-1])
              ]),
            /* y : Pixels */Block.__(2, [0]),
            /* w : Pixels */Block.__(2, [1]),
            /* h : Height */1
          ]
        ]),
      /* [] */0
    ]);

var squareColumnLayer_000 = /* content */init$3[/* content */0];

var squareColumnLayer_001 = /* enabled */init$3[/* enabled */1];

var squareColumnLayer_004 = /* rotation */init$3[/* rotation */4];

var squareColumnLayer_005 = /* transformMatrix */init$3[/* transformMatrix */5];

var squareColumnLayer_006 = /* filters */init$3[/* filters */6];

var squareColumnLayer_007 = /* id */init$3[/* id */7];

var squareColumnLayer = /* record */[
  squareColumnLayer_000,
  squareColumnLayer_001,
  /* alpha */0.75,
  /* compositeOperation : Multiply */11,
  squareColumnLayer_004,
  squareColumnLayer_005,
  squareColumnLayer_006,
  squareColumnLayer_007
];

var squareLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(10, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(2, [0]),
            /* y : Pixels */Block.__(2, [0]),
            /* w : Width */0,
            /* h : Height */1
          ]
        ]),
      /* [] */0
    ]]);

var squareLayer_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var squareLayer_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var squareLayer_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var squareLayer_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var squareLayer_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var squareLayer_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var squareLayer = /* record */[
  squareLayer_000,
  squareLayer_001,
  squareLayer_002,
  /* compositeOperation : Multiply */11,
  squareLayer_004,
  squareLayer_005,
  squareLayer_006,
  squareLayer_007
];

function singleNoteDrawCommands() {
  return /* :: */[
          /* SetFillStyle */Block.__(3, ["red"]),
          /* :: */[
            /* FillRect */Block.__(5, [/* record */[
                  /* x : Pixels */Block.__(2, [0]),
                  /* y : Note */Block.__(3, [60]),
                  /* w : Width */0,
                  /* h : Pixels */Block.__(2, [1])
                ]]),
            /* [] */0
          ]
        ];
}

function singleNoteLayer(note) {
  return /* record */[
          /* content : Draw */Block.__(1, [singleNoteDrawCommands(note)]),
          /* enabled */Layer$Gayer.defaultLayer[/* enabled */1],
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */2],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */4],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5],
          /* filters */Layer$Gayer.defaultLayer[/* filters */6],
          /* id */Layer$Gayer.defaultLayer[/* id */7]
        ];
}

var historyLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(10, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(2, [-1]),
            /* y : Pixels */Block.__(2, [0]),
            /* w : Width */0,
            /* h : Height */1
          ]
        ]),
      /* [] */0
    ]]);

var historyLayer_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var historyLayer_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var historyLayer_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var historyLayer_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var historyLayer_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var historyLayer_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var historyLayer_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var historyLayer = /* record */[
  historyLayer_000,
  historyLayer_001,
  historyLayer_002,
  historyLayer_003,
  historyLayer_004,
  historyLayer_005,
  historyLayer_006,
  historyLayer_007
];

var historyBackAndForthLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImageSourceDest */Block.__(11, [
          /* Self */0,
          /* record */[
            /* x : Divide */Block.__(6, [
                /* Width */0,
                /* Constant */Block.__(0, [2])
              ]),
            /* y : Pixels */Block.__(2, [0]),
            /* w : Divide */Block.__(6, [
                /* Width */0,
                /* Constant */Block.__(0, [2])
              ]),
            /* h : Height */1
          ],
          /* record */[
            /* x : Add */Block.__(5, [
                /* Divide */Block.__(6, [
                    /* Width */0,
                    /* Constant */Block.__(0, [2])
                  ]),
                /* Pixels */Block.__(2, [1])
              ]),
            /* y : Pixels */Block.__(2, [0]),
            /* w : Divide */Block.__(6, [
                /* Width */0,
                /* Constant */Block.__(0, [2])
              ]),
            /* h : Height */1
          ]
        ]),
      /* :: */[
        /* DrawImageSourceDest */Block.__(11, [
            /* Self */0,
            /* record */[
              /* x : Pixels */Block.__(2, [1]),
              /* y : Pixels */Block.__(2, [0]),
              /* w : Divide */Block.__(6, [
                  /* Width */0,
                  /* Constant */Block.__(0, [2])
                ]),
              /* h : Height */1
            ],
            /* record */[
              /* x : Pixels */Block.__(2, [0]),
              /* y : Pixels */Block.__(2, [0]),
              /* w : Divide */Block.__(6, [
                  /* Width */0,
                  /* Constant */Block.__(0, [2])
                ]),
              /* h : Height */1
            ]
          ]),
        /* [] */0
      ]
    ]]);

var historyBackAndForthLayer_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var historyBackAndForthLayer_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var historyBackAndForthLayer_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var historyBackAndForthLayer_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var historyBackAndForthLayer_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var historyBackAndForthLayer_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var historyBackAndForthLayer_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var historyBackAndForthLayer = /* record */[
  historyBackAndForthLayer_000,
  historyBackAndForthLayer_001,
  historyBackAndForthLayer_002,
  historyBackAndForthLayer_003,
  historyBackAndForthLayer_004,
  historyBackAndForthLayer_005,
  historyBackAndForthLayer_006,
  historyBackAndForthLayer_007
];

var drosteLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(10, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(2, [1]),
            /* y : Pixels */Block.__(2, [1]),
            /* w : Add */Block.__(5, [
                /* Width */0,
                /* Pixels */Block.__(2, [-1])
              ]),
            /* h : Add */Block.__(5, [
                /* Height */1,
                /* Pixels */Block.__(2, [-1])
              ])
          ]
        ]),
      /* [] */0
    ]]);

var drosteLayer_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var drosteLayer_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var drosteLayer_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var drosteLayer_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var drosteLayer_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var drosteLayer_006 = /* filters */"hue-rotate(" + ((1.0 / 30.0 * (5.0 / 6.0)).toString() + "turn)");

var drosteLayer_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var drosteLayer = /* record */[
  drosteLayer_000,
  drosteLayer_001,
  drosteLayer_002,
  drosteLayer_003,
  drosteLayer_004,
  drosteLayer_005,
  drosteLayer_006,
  drosteLayer_007
];

var midiKeyboard_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var midiKeyboard_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var midiKeyboard_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var midiKeyboard_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var midiKeyboard_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var midiKeyboard_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var midiKeyboard_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var midiKeyboard = /* record */[
  /* content : MIDIKeyboard */2,
  midiKeyboard_001,
  midiKeyboard_002,
  midiKeyboard_003,
  midiKeyboard_004,
  midiKeyboard_005,
  midiKeyboard_006,
  midiKeyboard_007
];

var midiColors_000 = /* content : Draw */Block.__(1, [MIDICanvas$Gayer.makeNoteColors(MIDICanvas$Gayer.oneRainbow)]);

var midiColors_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var midiColors_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var midiColors_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var midiColors_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var midiColors_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var midiColors_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var midiColors = /* record */[
  midiColors_000,
  midiColors_001,
  midiColors_002,
  /* compositeOperation : Multiply */11,
  midiColors_004,
  midiColors_005,
  midiColors_006,
  midiColors_007
];

var handDrawn_001 = /* enabled */Layer$Gayer.defaultLayer[/* enabled */1];

var handDrawn_002 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */2];

var handDrawn_003 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */3];

var handDrawn_004 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */4];

var handDrawn_005 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */5];

var handDrawn_006 = /* filters */Layer$Gayer.defaultLayer[/* filters */6];

var handDrawn_007 = /* id */Layer$Gayer.defaultLayer[/* id */7];

var handDrawn = /* record */[
  /* content : HandDrawn */0,
  handDrawn_001,
  handDrawn_002,
  handDrawn_003,
  handDrawn_004,
  handDrawn_005,
  handDrawn_006,
  handDrawn_007
];

var idCounter = /* record */[/* contents */0];

function maybeAddId(layer) {
  var match = layer[/* id */7];
  if (match !== undefined) {
    return layer;
  } else {
    var nextId = idCounter[0];
    idCounter[0] = nextId + 1 | 0;
    return /* record */[
            /* content */layer[/* content */0],
            /* enabled */layer[/* enabled */1],
            /* alpha */layer[/* alpha */2],
            /* compositeOperation */layer[/* compositeOperation */3],
            /* rotation */layer[/* rotation */4],
            /* transformMatrix */layer[/* transformMatrix */5],
            /* filters */layer[/* filters */6],
            /* id */Layer$Gayer.string_type_of_layerContent(layer[/* content */0]) + ("-" + String(nextId))
          ];
  }
}

var allLayerTypes = /* array */[
  /* tuple */[
    "image",
    hubble
  ],
  /* tuple */[
    "analyzer",
    analyzer(undefined, undefined, undefined, /* Mic */2)
  ],
  /* tuple */[
    "reader",
    reader
  ],
  /* tuple */[
    "webcam",
    webcam
  ],
  /* tuple */[
    "slitscan",
    slitscan
  ],
  /* tuple */[
    "edge detect",
    sobel("root")
  ],
  /* tuple */[
    "displace",
    displace("root", "root")
  ],
  /* tuple */[
    "midi-keyboard",
    midiKeyboard
  ],
  /* tuple */[
    "computer keyboard",
    keycodeWriter
  ],
  /* tuple */[
    "ASCII",
    keycodeReader
  ],
  /* tuple */[
    "mouse-drawGlobal",
    handDrawn
  ],
  /* tuple */[
    "drawGlobal (commands)",
    drawGlobal(undefined, /* :: */[
          /* SetFillStyle */Block.__(3, ["red"]),
          /* [] */0
        ])
  ],
  /* tuple */[
    "fill",
    fill(0.0125, "white")
  ],
  /* tuple */[
    "pitch filter",
    pitchFilter(Music$Gayer.cMajor)
  ],
  /* tuple */[
    "blur",
    blurLayer
  ],
  /* tuple */[
    "rotate (1 deg)",
    rotateLayer(Canvas$Gayer.degreesToRadians(1.0))
  ],
  /* tuple */[
    "rotate (90 deg)",
    rotateLayer(Canvas$Gayer.degreesToRadians(90.0))
  ],
  /* tuple */[
    "square values (column)",
    squareColumnLayer
  ],
  /* tuple */[
    "square values (whole image)",
    squareLayer
  ],
  /* tuple */[
    "raw-audio-writer",
    rawAudioWriter
  ],
  /* tuple */[
    "raw-audio-reader",
    rawAudioReader
  ],
  /* tuple */[
    "saturation reader",
    saturationReader
  ]
];

var defaultSize = Canvas$Gayer.defaultSize;

var defaultTransform = Canvas$Gayer.defaultTransform;

var tau = Canvas$Gayer.tau;

var degreesToRadians = Canvas$Gayer.degreesToRadians;

export {
  defaultSize ,
  defaultTransform ,
  tau ,
  degreesToRadians ,
  img ,
  video ,
  reader ,
  saturationReader ,
  keycodeReader ,
  keycodeWriter ,
  histogram ,
  rawAudioFormat ,
  rawAudioWriter ,
  rawAudioReader ,
  pitchFilter ,
  fill ,
  drawGlobal ,
  draw ,
  text ,
  sobel ,
  displace ,
  analyzer ,
  webcam ,
  slitscan ,
  hubble ,
  spacy ,
  drawSelfFullScreen ,
  harmony ,
  rotateLayer ,
  blurLayer ,
  squareColumnLayer ,
  squareLayer ,
  singleNoteDrawCommands ,
  singleNoteLayer ,
  historyLayer ,
  historyBackAndForthLayer ,
  drosteLayer ,
  midiKeyboard ,
  midiColors ,
  handDrawn ,
  idCounter ,
  maybeAddId ,
  allLayerTypes ,
  
}
/* hubble Not a pure module */
