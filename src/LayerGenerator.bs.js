// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Layer$Gayer from "./Layer.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as MIDICanvas$Gayer from "./MIDICanvas.bs.js";

function img(url) {
  return /* record */[
          /* content : Image */Block.__(3, [url]),
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */1],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */3],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4],
          /* filters */Layer$Gayer.defaultLayer[/* filters */5],
          /* id */Layer$Gayer.defaultLayer[/* id */6]
        ];
}

function video(url) {
  return /* record */[
          /* content : Video */Block.__(4, [url]),
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */1],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */3],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4],
          /* filters */Layer$Gayer.defaultLayer[/* filters */5],
          /* id */Layer$Gayer.defaultLayer[/* id */6]
        ];
}

var reader_000 = /* content : Reader */Block.__(9, [/* Channel */[/* R */0]]);

var reader_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var reader_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var reader_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var reader_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var reader_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var reader_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var reader = /* record */[
  reader_000,
  reader_001,
  reader_002,
  reader_003,
  reader_004,
  reader_005,
  reader_006
];

var saturationReader_000 = /* content : Reader */Block.__(9, [/* Saturation */0]);

var saturationReader_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var saturationReader_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var saturationReader_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var saturationReader_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var saturationReader_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var saturationReader_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var saturationReader = /* record */[
  saturationReader_000,
  saturationReader_001,
  saturationReader_002,
  saturationReader_003,
  saturationReader_004,
  saturationReader_005,
  saturationReader_006
];

var keycodeReader_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var keycodeReader_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var keycodeReader_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var keycodeReader_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var keycodeReader_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var keycodeReader_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var keycodeReader = /* record */[
  /* content : KeycodeReader */3,
  keycodeReader_001,
  keycodeReader_002,
  keycodeReader_003,
  keycodeReader_004,
  keycodeReader_005,
  keycodeReader_006
];

var keycodeWriter_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var keycodeWriter_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var keycodeWriter_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var keycodeWriter_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var keycodeWriter_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var keycodeWriter_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var keycodeWriter = /* record */[
  /* content : KeycodeWriter */4,
  keycodeWriter_001,
  keycodeWriter_002,
  keycodeWriter_003,
  keycodeWriter_004,
  keycodeWriter_005,
  keycodeWriter_006
];

var histogram_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var histogram_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var histogram_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var histogram_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var histogram = /* record */[
  /* content : Histogram */5,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  histogram_003,
  histogram_004,
  histogram_005,
  histogram_006
];

var rawAudioFormat = /* record */[
  /* x */0,
  /* y */0,
  /* w */64,
  /* h */32,
  /* encoding : Int8 */[/* R */0],
  /* sampleRate */44100
];

var rawAudioWriter_000 = /* content : RawAudioWriter */Block.__(7, [rawAudioFormat]);

var rawAudioWriter_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var rawAudioWriter_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var rawAudioWriter_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var rawAudioWriter_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var rawAudioWriter_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var rawAudioWriter_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var rawAudioWriter = /* record */[
  rawAudioWriter_000,
  rawAudioWriter_001,
  rawAudioWriter_002,
  rawAudioWriter_003,
  rawAudioWriter_004,
  rawAudioWriter_005,
  rawAudioWriter_006
];

var rawAudioReader_000 = /* content : RawAudioReader */Block.__(8, [rawAudioFormat]);

var rawAudioReader_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var rawAudioReader_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var rawAudioReader_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var rawAudioReader_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var rawAudioReader_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var rawAudioReader_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var rawAudioReader = /* record */[
  rawAudioReader_000,
  rawAudioReader_001,
  rawAudioReader_002,
  rawAudioReader_003,
  rawAudioReader_004,
  rawAudioReader_005,
  rawAudioReader_006
];

function pitchFilter(pc) {
  return /* record */[
          /* content : PitchClasses */Block.__(6, [pc]),
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */1],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */3],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4],
          /* filters */Layer$Gayer.defaultLayer[/* filters */5],
          /* id */Layer$Gayer.defaultLayer[/* id */6]
        ];
}

function fill($staropt$star, fillStyle) {
  var alpha = $staropt$star !== undefined ? $staropt$star : 1.0;
  return /* record */[
          /* content : Fill */Block.__(0, [fillStyle]),
          /* alpha */alpha,
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */3],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4],
          /* filters */Layer$Gayer.defaultLayer[/* filters */5],
          /* id */Layer$Gayer.defaultLayer[/* id */6]
        ];
}

function draw($staropt$star, cmds) {
  var alpha = $staropt$star !== undefined ? $staropt$star : 1.0;
  return /* record */[
          /* content : Draw */Block.__(1, [cmds]),
          /* alpha */alpha,
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */3],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4],
          /* filters */Layer$Gayer.defaultLayer[/* filters */5],
          /* id */Layer$Gayer.defaultLayer[/* id */6]
        ];
}

function text($staropt$star, $staropt$star$1, $staropt$star$2, $staropt$star$3, $staropt$star$4, $staropt$star$5, $staropt$star$6, s) {
  var x = $staropt$star !== undefined ? $staropt$star : /* Divide */Block.__(5, [
        /* Width */0,
        /* Constant */Block.__(0, [2])
      ]);
  var y = $staropt$star$1 !== undefined ? $staropt$star$1 : /* Divide */Block.__(5, [
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
  return draw(undefined, /* :: */[
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

var regl_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var regl_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var regl_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var regl_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var regl_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var regl_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var regl = /* record */[
  /* content : Regl */6,
  regl_001,
  regl_002,
  regl_003,
  regl_004,
  regl_005,
  regl_006
];

var sobel_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var sobel_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var sobel_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var sobel_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var sobel_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var sobel_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var sobel = /* record */[
  /* content : Regl */6,
  sobel_001,
  sobel_002,
  sobel_003,
  sobel_004,
  sobel_005,
  sobel_006
];

var analyzer_000 = /* content : Analysis */Block.__(5, [/* Mic */2]);

var analyzer_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var analyzer_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var analyzer_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var analyzer_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var analyzer_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var analyzer_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var analyzer = /* record */[
  analyzer_000,
  analyzer_001,
  analyzer_002,
  analyzer_003,
  analyzer_004,
  analyzer_005,
  analyzer_006
];

var webcam_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var webcam_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var webcam_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var webcam_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var webcam_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var webcam_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var webcam = /* record */[
  /* content : Webcam */1,
  webcam_001,
  webcam_002,
  webcam_003,
  webcam_004,
  webcam_005,
  webcam_006
];

var slitscan_000 = /* content : Slitscan */Block.__(2, [/* record */[
      /* sourceLayerKey */"webcam",
      /* slitscan : StaticX */Block.__(0, [320])
    ]]);

var slitscan_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var slitscan_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var slitscan_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var slitscan_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var slitscan_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var slitscan_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var slitscan = /* record */[
  slitscan_000,
  slitscan_001,
  slitscan_002,
  slitscan_003,
  slitscan_004,
  slitscan_005,
  slitscan_006
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
      /* x : Pixels */Block.__(1, [0]),
      /* y : Pixels */Block.__(1, [0]),
      /* w : Width */0,
      /* h : Height */1
    ]
  ]);

var init = img("media/harmony.png");

var init$1 = draw(undefined, /* :: */[
      /* DrawImage */Block.__(10, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(1, [0]),
            /* y : Pixels */Block.__(1, [-24]),
            /* w : Width */0,
            /* h : Height */1
          ]
        ]),
      /* :: */[
        /* DrawImage */Block.__(10, [
            /* Self */0,
            /* record */[
              /* x : Pixels */Block.__(1, [0]),
              /* y : Pixels */Block.__(1, [-48]),
              /* w : Width */0,
              /* h : Height */1
            ]
          ]),
        /* [] */0
      ]
    ]);

var harmony_000 = /* record */[
  /* content */init[/* content */0],
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* rotation */init[/* rotation */3],
  /* transformMatrix : record */[
    /* horizontalScaling */Canvas$Gayer.defaultTransform[/* horizontalScaling */0],
    /* horizontalSkewing */Canvas$Gayer.defaultTransform[/* horizontalSkewing */1],
    /* verticalSkewing */Canvas$Gayer.defaultTransform[/* verticalSkewing */2],
    /* verticalScaling */Canvas$Gayer.defaultTransform[/* verticalScaling */3],
    /* horizontalMoving */Canvas$Gayer.defaultTransform[/* horizontalMoving */4],
    /* verticalMoving */48.0
  ],
  /* filters */"blur(2px)",
  /* id */init[/* id */6]
];

var harmony_001 = /* :: */[
  sobel,
  /* :: */[
    /* record */[
      /* content */init$1[/* content */0],
      /* alpha */0.5,
      /* compositeOperation : Difference */20,
      /* rotation */init$1[/* rotation */3],
      /* transformMatrix */init$1[/* transformMatrix */4],
      /* filters */init$1[/* filters */5],
      /* id */init$1[/* id */6]
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

var rotateLayer = draw(undefined, /* :: */[
      /* Translate */Block.__(9, [
          /* Pixels */Block.__(1, [Canvas$Gayer.defaultSize / 2 | 0]),
          /* Pixels */Block.__(1, [Canvas$Gayer.defaultSize / 2 | 0])
        ]),
      /* :: */[
        /* Rotate */Block.__(8, [Layer$Gayer.oneCompleteTurnAfterNTicks(Canvas$Gayer.defaultSize / 2 | 0)]),
        /* :: */[
          /* Translate */Block.__(9, [
              /* Negate */Block.__(3, [/* Pixels */Block.__(1, [Canvas$Gayer.defaultSize / 2 | 0])]),
              /* Negate */Block.__(3, [/* Pixels */Block.__(1, [Canvas$Gayer.defaultSize / 2 | 0])])
            ]),
          /* :: */[
            drawSelfFullScreen,
            /* [] */0
          ]
        ]
      ]
    ]);

var init$2 = draw(undefined, /* :: */[
      drawSelfFullScreen,
      /* [] */0
    ]);

var blurLayer_000 = /* content */init$2[/* content */0];

var blurLayer_001 = /* alpha */init$2[/* alpha */1];

var blurLayer_002 = /* compositeOperation */init$2[/* compositeOperation */2];

var blurLayer_003 = /* rotation */init$2[/* rotation */3];

var blurLayer_004 = /* transformMatrix */init$2[/* transformMatrix */4];

var blurLayer_006 = /* id */init$2[/* id */6];

var blurLayer = /* record */[
  blurLayer_000,
  blurLayer_001,
  blurLayer_002,
  blurLayer_003,
  blurLayer_004,
  /* filters */"blur(2px)",
  blurLayer_006
];

var init$3 = draw(undefined, /* :: */[
      /* DrawImageSourceDest */Block.__(11, [
          /* Self */0,
          /* record */[
            /* x : Add */Block.__(4, [
                /* Width */0,
                /* Pixels */Block.__(1, [-1])
              ]),
            /* y : Pixels */Block.__(1, [0]),
            /* w : Pixels */Block.__(1, [1]),
            /* h : Height */1
          ],
          /* record */[
            /* x : Add */Block.__(4, [
                /* Width */0,
                /* Pixels */Block.__(1, [-1])
              ]),
            /* y : Pixels */Block.__(1, [0]),
            /* w : Pixels */Block.__(1, [1]),
            /* h : Height */1
          ]
        ]),
      /* [] */0
    ]);

var squareColumnLayer_000 = /* content */init$3[/* content */0];

var squareColumnLayer_003 = /* rotation */init$3[/* rotation */3];

var squareColumnLayer_004 = /* transformMatrix */init$3[/* transformMatrix */4];

var squareColumnLayer_005 = /* filters */init$3[/* filters */5];

var squareColumnLayer_006 = /* id */init$3[/* id */6];

var squareColumnLayer = /* record */[
  squareColumnLayer_000,
  /* alpha */0.75,
  /* compositeOperation : Multiply */11,
  squareColumnLayer_003,
  squareColumnLayer_004,
  squareColumnLayer_005,
  squareColumnLayer_006
];

var squareLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(10, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(1, [0]),
            /* y : Pixels */Block.__(1, [0]),
            /* w : Width */0,
            /* h : Height */1
          ]
        ]),
      /* [] */0
    ]]);

var squareLayer_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var squareLayer_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var squareLayer_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var squareLayer_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var squareLayer_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var squareLayer = /* record */[
  squareLayer_000,
  squareLayer_001,
  /* compositeOperation : Multiply */11,
  squareLayer_003,
  squareLayer_004,
  squareLayer_005,
  squareLayer_006
];

function singleNoteDrawCommands() {
  return /* :: */[
          /* SetFillStyle */Block.__(3, ["red"]),
          /* :: */[
            /* FillRect */Block.__(5, [/* record */[
                  /* x : Pixels */Block.__(1, [0]),
                  /* y : Note */Block.__(2, [60]),
                  /* w : Width */0,
                  /* h : Pixels */Block.__(1, [1])
                ]]),
            /* [] */0
          ]
        ];
}

function singleNoteLayer(note) {
  return /* record */[
          /* content : Draw */Block.__(1, [singleNoteDrawCommands(note)]),
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */1],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */3],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4],
          /* filters */Layer$Gayer.defaultLayer[/* filters */5],
          /* id */Layer$Gayer.defaultLayer[/* id */6]
        ];
}

var historyLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(10, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(1, [-1]),
            /* y : Pixels */Block.__(1, [0]),
            /* w : Width */0,
            /* h : Height */1
          ]
        ]),
      /* [] */0
    ]]);

var historyLayer_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var historyLayer_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var historyLayer_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var historyLayer_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var historyLayer_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var historyLayer_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var historyLayer = /* record */[
  historyLayer_000,
  historyLayer_001,
  historyLayer_002,
  historyLayer_003,
  historyLayer_004,
  historyLayer_005,
  historyLayer_006
];

var historyBackAndForthLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImageSourceDest */Block.__(11, [
          /* Self */0,
          /* record */[
            /* x : Divide */Block.__(5, [
                /* Width */0,
                /* Constant */Block.__(0, [2])
              ]),
            /* y : Pixels */Block.__(1, [0]),
            /* w : Divide */Block.__(5, [
                /* Width */0,
                /* Constant */Block.__(0, [2])
              ]),
            /* h : Height */1
          ],
          /* record */[
            /* x : Add */Block.__(4, [
                /* Divide */Block.__(5, [
                    /* Width */0,
                    /* Constant */Block.__(0, [2])
                  ]),
                /* Pixels */Block.__(1, [1])
              ]),
            /* y : Pixels */Block.__(1, [0]),
            /* w : Divide */Block.__(5, [
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
              /* x : Pixels */Block.__(1, [1]),
              /* y : Pixels */Block.__(1, [0]),
              /* w : Divide */Block.__(5, [
                  /* Width */0,
                  /* Constant */Block.__(0, [2])
                ]),
              /* h : Height */1
            ],
            /* record */[
              /* x : Pixels */Block.__(1, [0]),
              /* y : Pixels */Block.__(1, [0]),
              /* w : Divide */Block.__(5, [
                  /* Width */0,
                  /* Constant */Block.__(0, [2])
                ]),
              /* h : Height */1
            ]
          ]),
        /* [] */0
      ]
    ]]);

var historyBackAndForthLayer_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var historyBackAndForthLayer_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var historyBackAndForthLayer_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var historyBackAndForthLayer_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var historyBackAndForthLayer_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var historyBackAndForthLayer_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var historyBackAndForthLayer = /* record */[
  historyBackAndForthLayer_000,
  historyBackAndForthLayer_001,
  historyBackAndForthLayer_002,
  historyBackAndForthLayer_003,
  historyBackAndForthLayer_004,
  historyBackAndForthLayer_005,
  historyBackAndForthLayer_006
];

var drosteLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(10, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(1, [1]),
            /* y : Pixels */Block.__(1, [1]),
            /* w : Add */Block.__(4, [
                /* Width */0,
                /* Pixels */Block.__(1, [-1])
              ]),
            /* h : Add */Block.__(4, [
                /* Height */1,
                /* Pixels */Block.__(1, [-1])
              ])
          ]
        ]),
      /* [] */0
    ]]);

var drosteLayer_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var drosteLayer_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var drosteLayer_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var drosteLayer_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var drosteLayer_005 = /* filters */"hue-rotate(" + ((1.0 / 30.0 * (5.0 / 6.0)).toString() + "turn)");

var drosteLayer_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var drosteLayer = /* record */[
  drosteLayer_000,
  drosteLayer_001,
  drosteLayer_002,
  drosteLayer_003,
  drosteLayer_004,
  drosteLayer_005,
  drosteLayer_006
];

var midiKeyboard_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var midiKeyboard_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var midiKeyboard_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var midiKeyboard_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var midiKeyboard_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var midiKeyboard_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var midiKeyboard = /* record */[
  /* content : MIDIKeyboard */2,
  midiKeyboard_001,
  midiKeyboard_002,
  midiKeyboard_003,
  midiKeyboard_004,
  midiKeyboard_005,
  midiKeyboard_006
];

var midiColors_000 = /* content : Draw */Block.__(1, [MIDICanvas$Gayer.makeNoteColors(MIDICanvas$Gayer.oneRainbow)]);

var midiColors_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var midiColors_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var midiColors_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var midiColors_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var midiColors_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var midiColors = /* record */[
  midiColors_000,
  midiColors_001,
  /* compositeOperation : Multiply */11,
  midiColors_003,
  midiColors_004,
  midiColors_005,
  midiColors_006
];

var handDrawn_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var handDrawn_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var handDrawn_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var handDrawn_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var handDrawn_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var handDrawn_006 = /* id */Layer$Gayer.defaultLayer[/* id */6];

var handDrawn = /* record */[
  /* content : HandDrawn */0,
  handDrawn_001,
  handDrawn_002,
  handDrawn_003,
  handDrawn_004,
  handDrawn_005,
  handDrawn_006
];

var allLayerTypes = /* array */[
  /* tuple */[
    "image",
    hubble
  ],
  /* tuple */[
    "analyzer",
    analyzer
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
    "mouse-draw",
    handDrawn
  ],
  /* tuple */[
    "draw (commands)",
    draw(undefined, /* :: */[
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
    "rotate",
    rotateLayer
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

export {
  defaultSize ,
  defaultTransform ,
  tau ,
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
  draw ,
  text ,
  regl ,
  sobel ,
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
  allLayerTypes ,
  
}
/* hubble Not a pure module */
