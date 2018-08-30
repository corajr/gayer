// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Layer$Gayer from "./Layer.bs.js";
import * as Music$Gayer from "./Music.bs.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as Params$Gayer from "./Params.bs.js";
import * as MIDICanvas$Gayer from "./MIDICanvas.bs.js";

function img(url) {
  return /* record */[
          /* content : Image */Block.__(3, [url]),
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */1],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */3],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4],
          /* filters */Layer$Gayer.defaultLayer[/* filters */5]
        ];
}

function video(url) {
  return /* record */[
          /* content : Video */Block.__(4, [url]),
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */1],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */3],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4],
          /* filters */Layer$Gayer.defaultLayer[/* filters */5]
        ];
}

var reader_000 = /* content : Reader */Block.__(7, [/* R */0]);

var reader_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var reader_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var reader_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var reader = /* record */[
  reader_000,
  /* alpha */1.0,
  /* compositeOperation : Multiply */11,
  reader_003,
  reader_004,
  reader_005
];

function pitchFilter(pc) {
  return /* record */[
          /* content : PitchClasses */Block.__(6, [pc]),
          /* alpha */Layer$Gayer.defaultLayer[/* alpha */1],
          /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2],
          /* rotation */Layer$Gayer.defaultLayer[/* rotation */3],
          /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4],
          /* filters */Layer$Gayer.defaultLayer[/* filters */5]
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
          /* filters */Layer$Gayer.defaultLayer[/* filters */5]
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
          /* filters */Layer$Gayer.defaultLayer[/* filters */5]
        ];
}

var analyzer_000 = /* content : Analysis */Block.__(5, [/* Mic */1]);

var analyzer_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var analyzer_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var analyzer_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var analyzer_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var analyzer_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var analyzer = /* record */[
  analyzer_000,
  analyzer_001,
  analyzer_002,
  analyzer_003,
  analyzer_004,
  analyzer_005
];

var webcam_000 = /* content : Webcam */Block.__(2, [/* record */[/* slitscan */undefined]]);

var webcam_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var webcam_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var webcam_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var webcam_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var webcam_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var webcam = /* record */[
  webcam_000,
  webcam_001,
  webcam_002,
  webcam_003,
  webcam_004,
  webcam_005
];

var slitscan_000 = /* content : Webcam */Block.__(2, [/* record */[/* slitscan *//* record */[/* x */320]]]);

var slitscan_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var slitscan_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var slitscan_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var slitscan_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var slitscan_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var slitscan = /* record */[
  slitscan_000,
  slitscan_001,
  slitscan_002,
  slitscan_003,
  slitscan_004,
  slitscan_005
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

var drawSelfFullScreen = /* DrawImage */Block.__(4, [
    /* Self */0,
    /* record */[
      /* x : Pixels */Block.__(0, [0]),
      /* y : Pixels */Block.__(0, [0]),
      /* w : Width */0,
      /* h : Height */1
    ]
  ]);

var init = img("media/harmony.png");

var init$1 = draw(undefined, /* :: */[
      /* DrawImage */Block.__(4, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(0, [0]),
            /* y : Pixels */Block.__(0, [-24]),
            /* w : Width */0,
            /* h : Height */1
          ]
        ]),
      /* :: */[
        /* DrawImage */Block.__(4, [
            /* Self */0,
            /* record */[
              /* x : Pixels */Block.__(0, [0]),
              /* y : Pixels */Block.__(0, [-48]),
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
  /* filters */"blur(2px)"
];

var harmony_001 = /* :: */[
  /* record */[
    /* content */init$1[/* content */0],
    /* alpha */0.5,
    /* compositeOperation : Difference */20,
    /* rotation */init$1[/* rotation */3],
    /* transformMatrix */init$1[/* transformMatrix */4],
    /* filters */init$1[/* filters */5]
  ],
  /* :: */[
    pitchFilter(Music$Gayer.cMajor),
    /* :: */[
      reader,
      /* [] */0
    ]
  ]
];

var harmony = /* :: */[
  harmony_000,
  harmony_001
];

var rotateLayer = draw(undefined, /* :: */[
      /* Translate */Block.__(3, [
          /* Pixels */Block.__(0, [Canvas$Gayer.defaultSize / 2 | 0]),
          /* Pixels */Block.__(0, [Canvas$Gayer.defaultSize / 2 | 0])
        ]),
      /* :: */[
        /* Rotate */Block.__(2, [Layer$Gayer.oneCompleteTurnAfterNTicks(Canvas$Gayer.defaultSize / 2 | 0)]),
        /* :: */[
          /* Translate */Block.__(3, [
              /* Negate */Block.__(2, [/* Pixels */Block.__(0, [Canvas$Gayer.defaultSize / 2 | 0])]),
              /* Negate */Block.__(2, [/* Pixels */Block.__(0, [Canvas$Gayer.defaultSize / 2 | 0])])
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

var blurLayer = /* record */[
  blurLayer_000,
  blurLayer_001,
  blurLayer_002,
  blurLayer_003,
  blurLayer_004,
  /* filters */"blur(2px)"
];

var init$3 = draw(undefined, /* :: */[
      /* DrawImage */Block.__(4, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(0, [0]),
            /* y : Pixels */Block.__(0, [0]),
            /* w : Pixels */Block.__(0, [1]),
            /* h : Pixels */Block.__(0, [1])
          ]
        ]),
      /* [] */0
    ]);

var squareColumnLayer_000 = /* content */init$3[/* content */0];

var squareColumnLayer_001 = /* alpha */init$3[/* alpha */1];

var squareColumnLayer_003 = /* rotation */init$3[/* rotation */3];

var squareColumnLayer_004 = /* transformMatrix */init$3[/* transformMatrix */4];

var squareColumnLayer_005 = /* filters */init$3[/* filters */5];

var squareColumnLayer = /* record */[
  squareColumnLayer_000,
  squareColumnLayer_001,
  /* compositeOperation : Multiply */11,
  squareColumnLayer_003,
  squareColumnLayer_004,
  squareColumnLayer_005
];

var squareLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(4, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(0, [0]),
            /* y : Pixels */Block.__(0, [0]),
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

var squareLayer = /* record */[
  squareLayer_000,
  squareLayer_001,
  /* compositeOperation : Multiply */11,
  squareLayer_003,
  squareLayer_004,
  squareLayer_005
];

var allLayerTypes_001 = /* :: */[
  analyzer,
  /* :: */[
    webcam,
    /* :: */[
      slitscan,
      /* :: */[
        fill(0.0125, "white"),
        /* :: */[
          pitchFilter(Music$Gayer.cMajor),
          /* :: */[
            blurLayer,
            /* :: */[
              rotateLayer,
              /* :: */[
                squareColumnLayer,
                /* :: */[
                  squareLayer,
                  /* :: */[
                    reader,
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
];

var allLayerTypes = /* :: */[
  hubble,
  allLayerTypes_001
];

var harmonyParams_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var harmonyParams_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var harmonyParams_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var harmonyParams_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var harmonyParams_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var harmonyParams_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var harmonyParams_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var harmonyParams_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var harmonyParams_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var harmonyParams_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var harmonyParams = /* record */[
  harmonyParams_000,
  harmonyParams_001,
  harmonyParams_002,
  harmonyParams_003,
  /* millisPerTick */25,
  harmonyParams_005,
  harmonyParams_006,
  harmonyParams_007,
  harmonyParams_008,
  harmonyParams_009,
  harmonyParams_010,
  /* layers */harmony
];

var harmonyIntensified_000 = harmonyParams_000;

var harmonyIntensified_001 = harmonyParams_001;

var harmonyIntensified_002 = harmonyParams_002;

var harmonyIntensified_003 = harmonyParams_003;

var harmonyIntensified_005 = harmonyParams_005;

var harmonyIntensified_006 = harmonyParams_006;

var harmonyIntensified_007 = harmonyParams_007;

var harmonyIntensified_008 = harmonyParams_008;

var harmonyIntensified_009 = harmonyParams_009;

var harmonyIntensified_010 = harmonyParams_010;

var harmonyIntensified_011 = /* layers : :: */[
  img("media/harmony_intensified.png"),
  /* :: */[
    reader,
    /* [] */0
  ]
];

var harmonyIntensified = /* record */[
  harmonyIntensified_000,
  harmonyIntensified_001,
  harmonyIntensified_002,
  harmonyIntensified_003,
  /* millisPerTick */25,
  harmonyIntensified_005,
  harmonyIntensified_006,
  harmonyIntensified_007,
  harmonyIntensified_008,
  harmonyIntensified_009,
  harmonyIntensified_010,
  harmonyIntensified_011
];

var feedback_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var feedback_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var feedback_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var feedback_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var feedback_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var feedback_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var feedback_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var feedback_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var feedback_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var feedback_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var feedback_011 = /* layers : :: */[
  webcam,
  /* :: */[
    /* record */[
      /* content : Analysis */Block.__(5, [/* Mic */1]),
      /* alpha */0.5,
      analyzer_002,
      analyzer_003,
      analyzer_004,
      analyzer_005
    ],
    /* :: */[
      pitchFilter(Music$Gayer.cMajor),
      /* :: */[
        reader,
        /* [] */0
      ]
    ]
  ]
];

var feedback = /* record */[
  feedback_000,
  feedback_001,
  feedback_002,
  feedback_003,
  feedback_004,
  /* audioInputSetting : Mic */1,
  feedback_006,
  feedback_007,
  feedback_008,
  feedback_009,
  feedback_010,
  feedback_011
];

/* :: */[
  slitscan,
  /* :: */[
    /* record */[
      /* content : Analysis */Block.__(5, [/* Mic */1]),
      /* alpha */0.25,
      analyzer_002,
      analyzer_003,
      analyzer_004,
      analyzer_005
    ],
    /* :: */[
      pitchFilter(Music$Gayer.cMajor),
      /* :: */[
        reader,
        /* [] */0
      ]
    ]
  ]
];

var slitscanParams_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var slitscanParams_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var slitscanParams_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var slitscanParams_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var slitscanParams_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var slitscanParams_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var slitscanParams_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var slitscanParams_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var slitscanParams_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var slitscanParams_011 = /* layers : :: */[
  slitscan,
  /* :: */[
    /* record */[
      /* content : Analysis */Block.__(5, [/* Mic */1]),
      /* alpha */0.25,
      analyzer_002,
      analyzer_003,
      analyzer_004,
      analyzer_005
    ],
    /* :: */[
      pitchFilter(Music$Gayer.cMajor),
      /* :: */[
        reader,
        /* [] */0
      ]
    ]
  ]
];

var slitscanParams = /* record */[
  slitscanParams_000,
  slitscanParams_001,
  slitscanParams_002,
  slitscanParams_003,
  slitscanParams_004,
  /* audioInputSetting : Mic */1,
  slitscanParams_006,
  slitscanParams_007,
  slitscanParams_008,
  slitscanParams_009,
  /* shouldClear */false,
  slitscanParams_011
];

var whiteboardParams_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var whiteboardParams_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var whiteboardParams_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var whiteboardParams_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var whiteboardParams_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var whiteboardParams_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var whiteboardParams_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var whiteboardParams_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var whiteboardParams_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var whiteboardParams_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var whiteboardParams_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var whiteboardParams_011 = /* layers : :: */[
  /* record */[
    /* content : Webcam */Block.__(2, [/* record */[/* slitscan */undefined]]),
    webcam_001,
    webcam_002,
    webcam_003,
    webcam_004,
    /* filters */"contrast(300%) invert(100%)"
  ],
  /* :: */[
    pitchFilter(Music$Gayer.pentatonic),
    /* :: */[
      reader,
      /* [] */0
    ]
  ]
];

var whiteboardParams = /* record */[
  whiteboardParams_000,
  whiteboardParams_001,
  whiteboardParams_002,
  whiteboardParams_003,
  whiteboardParams_004,
  whiteboardParams_005,
  whiteboardParams_006,
  whiteboardParams_007,
  whiteboardParams_008,
  whiteboardParams_009,
  whiteboardParams_010,
  whiteboardParams_011
];

var isItACrime_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var isItACrime_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var isItACrime_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var isItACrime_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var isItACrime_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var isItACrime_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var isItACrime_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var isItACrime_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var isItACrime_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var isItACrime_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var isItACrime_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var isItACrime_011 = /* layers : :: */[
  img("media/is_it_a_crime_large.png"),
  /* :: */[
    /* record */[
      /* content : Reader */Block.__(7, [/* A */3]),
      /* alpha */1.0,
      /* compositeOperation : Multiply */11,
      reader_003,
      reader_004,
      reader_005
    ],
    /* [] */0
  ]
];

var isItACrime = /* record */[
  isItACrime_000,
  isItACrime_001,
  isItACrime_002,
  isItACrime_003,
  isItACrime_004,
  isItACrime_005,
  isItACrime_006,
  isItACrime_007,
  isItACrime_008,
  isItACrime_009,
  isItACrime_010,
  isItACrime_011
];

var tughra_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var tughra_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var tughra_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var tughra_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var tughra_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var tughra_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var tughra_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var tughra_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var tughra_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var tughra_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var tughra_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var tughra_011 = /* layers : :: */[
  img("media/suleiman.jpg"),
  /* :: */[
    reader,
    /* [] */0
  ]
];

var tughra = /* record */[
  tughra_000,
  tughra_001,
  tughra_002,
  tughra_003,
  tughra_004,
  tughra_005,
  tughra_006,
  tughra_007,
  tughra_008,
  tughra_009,
  tughra_010,
  tughra_011
];

var iChing_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var iChing_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var iChing_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var iChing_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var iChing_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var iChing_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var iChing_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var iChing_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var iChing_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var iChing_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var iChing_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var iChing_011 = /* layers : :: */[
  img("media/king_wen.png"),
  /* :: */[
    pitchFilter(Music$Gayer.majorHexatonic),
    /* :: */[
      reader,
      /* [] */0
    ]
  ]
];

var iChing = /* record */[
  iChing_000,
  iChing_001,
  iChing_002,
  iChing_003,
  iChing_004,
  iChing_005,
  iChing_006,
  iChing_007,
  iChing_008,
  iChing_009,
  iChing_010,
  iChing_011
];

function singleNoteDrawCommands() {
  return /* :: */[
          /* SetFillStyle */Block.__(0, ["red"]),
          /* :: */[
            /* FillRect */Block.__(1, [/* record */[
                  /* x : Pixels */Block.__(0, [0]),
                  /* y : Note */Block.__(1, [60]),
                  /* w : Width */0,
                  /* h : Pixels */Block.__(0, [1])
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
          /* filters */Layer$Gayer.defaultLayer[/* filters */5]
        ];
}

var singleNote_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var singleNote_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var singleNote_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var singleNote_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var singleNote_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var singleNote_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var singleNote_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var singleNote_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var singleNote_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var singleNote_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var singleNote_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var singleNote_011 = /* layers : :: */[
  singleNoteLayer(60),
  /* :: */[
    reader,
    /* [] */0
  ]
];

var singleNote = /* record */[
  singleNote_000,
  singleNote_001,
  singleNote_002,
  singleNote_003,
  singleNote_004,
  singleNote_005,
  singleNote_006,
  singleNote_007,
  singleNote_008,
  singleNote_009,
  singleNote_010,
  singleNote_011
];

var historyLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(4, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(0, [-1]),
            /* y : Pixels */Block.__(0, [0]),
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

var historyLayer = /* record */[
  historyLayer_000,
  historyLayer_001,
  historyLayer_002,
  historyLayer_003,
  historyLayer_004,
  historyLayer_005
];

var drosteLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(4, [
          /* Self */0,
          /* record */[
            /* x : Pixels */Block.__(0, [1]),
            /* y : Pixels */Block.__(0, [1]),
            /* w : Add */Block.__(3, [
                /* Width */0,
                /* Pixels */Block.__(0, [-1])
              ]),
            /* h : Add */Block.__(3, [
                /* Height */1,
                /* Pixels */Block.__(0, [-1])
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

var drosteLayer = /* record */[
  drosteLayer_000,
  drosteLayer_001,
  drosteLayer_002,
  drosteLayer_003,
  drosteLayer_004,
  drosteLayer_005
];

var history_002 = /* readPosOffset */Canvas$Gayer.defaultSize - 1 | 0;

var history_003 = /* writePosOffset */Canvas$Gayer.defaultSize - 1 | 0;

var history_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var history_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var history_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var history_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var history_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var history_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var history_011 = /* layers : :: */[
  analyzer,
  /* :: */[
    historyLayer,
    /* :: */[
      /* record */[
        /* content : Reader */Block.__(7, [/* R */0]),
        /* alpha */0.0,
        /* compositeOperation : Multiply */11,
        reader_003,
        reader_004,
        reader_005
      ],
      /* [] */0
    ]
  ]
];

var history = /* record */[
  /* readPosDelta */0,
  /* writePosDelta */0,
  history_002,
  history_003,
  history_004,
  history_005,
  history_006,
  history_007,
  history_008,
  history_009,
  /* shouldClear */false,
  history_011
];

var debussyFile_000 = /* content : Analysis */Block.__(5, [/* AudioFile */Block.__(0, ["media/la_cathedrale_engloutie.m4a"])]);

var debussyFile_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var debussyFile_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var debussyFile_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var debussyFile_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var debussyFile_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var debussyFile = /* record */[
  debussyFile_000,
  debussyFile_001,
  debussyFile_002,
  debussyFile_003,
  debussyFile_004,
  debussyFile_005
];

var debussy_002 = history_002;

var debussy_003 = history_003;

var debussy_004 = history_004;

var debussy_005 = history_005;

var debussy_006 = history_006;

var debussy_007 = history_007;

var debussy_008 = history_008;

var debussy_009 = history_009;

var debussy_011 = /* layers : :: */[
  debussyFile,
  /* :: */[
    historyLayer,
    /* :: */[
      reader,
      /* [] */0
    ]
  ]
];

var debussy = /* record */[
  /* readPosDelta */0,
  /* writePosDelta */0,
  debussy_002,
  debussy_003,
  debussy_004,
  debussy_005,
  debussy_006,
  debussy_007,
  debussy_008,
  debussy_009,
  /* shouldClear */false,
  debussy_011
];

var droste_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var droste_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var droste_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var droste_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var droste_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var droste_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var droste_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var droste_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var droste_011 = /* layers : :: */[
  analyzer,
  /* :: */[
    drosteLayer,
    /* :: */[
      reader,
      /* [] */0
    ]
  ]
];

var droste = /* record */[
  /* readPosDelta */0,
  /* writePosDelta */0,
  droste_002,
  droste_003,
  droste_004,
  droste_005,
  droste_006,
  droste_007,
  droste_008,
  droste_009,
  /* shouldClear */false,
  droste_011
];

var midiKeyboard_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var midiKeyboard_002 = /* compositeOperation */Layer$Gayer.defaultLayer[/* compositeOperation */2];

var midiKeyboard_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var midiKeyboard_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var midiKeyboard_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var midiKeyboard = /* record */[
  /* content : MIDIKeyboard */0,
  midiKeyboard_001,
  midiKeyboard_002,
  midiKeyboard_003,
  midiKeyboard_004,
  midiKeyboard_005
];

var midiColors_000 = /* content : Draw */Block.__(1, [MIDICanvas$Gayer.makeNoteColors(MIDICanvas$Gayer.oneRainbow)]);

var midiColors_001 = /* alpha */Layer$Gayer.defaultLayer[/* alpha */1];

var midiColors_003 = /* rotation */Layer$Gayer.defaultLayer[/* rotation */3];

var midiColors_004 = /* transformMatrix */Layer$Gayer.defaultLayer[/* transformMatrix */4];

var midiColors_005 = /* filters */Layer$Gayer.defaultLayer[/* filters */5];

var midiColors = /* record */[
  midiColors_000,
  midiColors_001,
  /* compositeOperation : Multiply */11,
  midiColors_003,
  midiColors_004,
  midiColors_005
];

var midi_002 = history_002;

var midi_003 = history_003;

var midi_004 = history_004;

var midi_005 = history_005;

var midi_006 = history_006;

var midi_007 = history_007;

var midi_008 = history_008;

var midi_009 = history_009;

var midi_011 = /* layers : :: */[
  midiKeyboard,
  /* :: */[
    historyLayer,
    /* :: */[
      reader,
      /* [] */0
    ]
  ]
];

var midi = /* record */[
  /* readPosDelta */0,
  /* writePosDelta */0,
  midi_002,
  midi_003,
  midi_004,
  midi_005,
  midi_006,
  midi_007,
  midi_008,
  midi_009,
  /* shouldClear */false,
  midi_011
];

var midiDroste_002 = droste_002;

var midiDroste_003 = droste_003;

var midiDroste_004 = droste_004;

var midiDroste_005 = droste_005;

var midiDroste_006 = droste_006;

var midiDroste_007 = droste_007;

var midiDroste_008 = droste_008;

var midiDroste_009 = droste_009;

var midiDroste_011 = /* layers : :: */[
  midiKeyboard,
  /* :: */[
    drosteLayer,
    /* :: */[
      reader,
      /* [] */0
    ]
  ]
];

var midiDroste = /* record */[
  /* readPosDelta */0,
  /* writePosDelta */0,
  midiDroste_002,
  midiDroste_003,
  midiDroste_004,
  midiDroste_005,
  midiDroste_006,
  midiDroste_007,
  midiDroste_008,
  midiDroste_009,
  /* shouldClear */false,
  midiDroste_011
];

var readFromCenterLine_002 = /* readPosOffset */Canvas$Gayer.defaultSize / 2 | 0;

var readFromCenterLine_003 = /* writePosOffset */Canvas$Gayer.defaultSize / 2 | 0;

var readFromCenterLine_004 = history_004;

var readFromCenterLine_005 = history_005;

var readFromCenterLine_006 = history_006;

var readFromCenterLine_007 = history_007;

var readFromCenterLine_008 = history_008;

var readFromCenterLine_009 = history_009;

var readFromCenterLine_011 = history_011;

var readFromCenterLine = /* record */[
  /* readPosDelta */0,
  /* writePosDelta */0,
  readFromCenterLine_002,
  readFromCenterLine_003,
  readFromCenterLine_004,
  readFromCenterLine_005,
  readFromCenterLine_006,
  readFromCenterLine_007,
  readFromCenterLine_008,
  readFromCenterLine_009,
  /* shouldClear */false,
  readFromCenterLine_011
];

var vinyl_002 = readFromCenterLine_002;

var vinyl_003 = readFromCenterLine_003;

var vinyl_004 = readFromCenterLine_004;

var vinyl_005 = readFromCenterLine_005;

var vinyl_006 = readFromCenterLine_006;

var vinyl_007 = readFromCenterLine_007;

var vinyl_008 = readFromCenterLine_008;

var vinyl_009 = readFromCenterLine_009;

var vinyl_011 = /* layers : :: */[
  rotateLayer,
  /* :: */[
    analyzer,
    /* :: */[
      reader,
      /* [] */0
    ]
  ]
];

var vinyl = /* record */[
  /* readPosDelta */0,
  /* writePosDelta */0,
  vinyl_002,
  vinyl_003,
  vinyl_004,
  vinyl_005,
  vinyl_006,
  vinyl_007,
  vinyl_008,
  vinyl_009,
  /* shouldClear */false,
  vinyl_011
];

var videoURL = "media/nonfree/kishi_bashi-say_yeah.mp4";

var video_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var video_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var video_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var video_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var video_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var video_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var video_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var video_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var video_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var video_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var video_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var video_011 = /* layers : :: */[
  video(videoURL),
  /* :: */[
    /* record */[
      /* content : Analysis */Block.__(5, [/* AudioFromVideo */Block.__(1, [videoURL])]),
      analyzer_001,
      analyzer_002,
      analyzer_003,
      analyzer_004,
      analyzer_005
    ],
    /* :: */[
      reader,
      /* [] */0
    ]
  ]
];

var video$1 = /* record */[
  video_000,
  video_001,
  video_002,
  video_003,
  video_004,
  video_005,
  video_006,
  video_007,
  video_008,
  video_009,
  video_010,
  video_011
];

var presets_000 = /* tuple */[
  "Spacy",
  /* record */[
    /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0],
    /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1],
    /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2],
    /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3],
    /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4],
    /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5],
    /* inputGain */Params$Gayer.defaultParams[/* inputGain */6],
    /* outputGain */Params$Gayer.defaultParams[/* outputGain */7],
    /* q */Params$Gayer.defaultParams[/* q */8],
    /* transpose */Params$Gayer.defaultParams[/* transpose */9],
    /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10],
    /* layers */spacy
  ]
];

var presets_001 = /* :: */[
  /* tuple */[
    "Single note",
    singleNote
  ],
  /* :: */[
    /* tuple */[
      "Slitscan",
      slitscanParams
    ],
    /* :: */[
      /* tuple */[
        "History",
        history
      ],
      /* :: */[
        /* tuple */[
          "Rotation",
          vinyl
        ],
        /* :: */[
          /* tuple */[
            "Tughra of Suleiman",
            tughra
          ],
          /* :: */[
            /* tuple */[
              "Is it a crime?",
              isItACrime
            ],
            /* :: */[
              /* tuple */[
                "MIDI",
                midi
              ],
              /* :: */[
                /* tuple */[
                  "Mic feedback (may be loud!)",
                  feedback
                ],
                /* :: */[
                  /* tuple */[
                    "Empty",
                    /* record */[
                      /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0],
                      /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1],
                      /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2],
                      /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3],
                      /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4],
                      /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5],
                      /* inputGain */Params$Gayer.defaultParams[/* inputGain */6],
                      /* outputGain */Params$Gayer.defaultParams[/* outputGain */7],
                      /* q */Params$Gayer.defaultParams[/* q */8],
                      /* transpose */Params$Gayer.defaultParams[/* transpose */9],
                      /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10],
                      /* layers : [] */0
                    ]
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
];

var presets = /* :: */[
  presets_000,
  presets_001
];

var defaultSize = Canvas$Gayer.defaultSize;

var defaultTransform = Canvas$Gayer.defaultTransform;

var tau = Canvas$Gayer.tau;

export {
  defaultSize ,
  defaultTransform ,
  tau ,
  img ,
  reader ,
  pitchFilter ,
  fill ,
  draw ,
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
  allLayerTypes ,
  harmonyParams ,
  harmonyIntensified ,
  feedback ,
  slitscanParams ,
  whiteboardParams ,
  isItACrime ,
  tughra ,
  iChing ,
  singleNoteDrawCommands ,
  singleNoteLayer ,
  singleNote ,
  historyLayer ,
  drosteLayer ,
  history ,
  debussyFile ,
  debussy ,
  droste ,
  midiKeyboard ,
  midiColors ,
  midi ,
  midiDroste ,
  readFromCenterLine ,
  vinyl ,
  videoURL ,
  video$1 as video,
  presets ,
  
}
/* hubble Not a pure module */
