// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Music$Gayer from "./Music.bs.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as Params$Gayer from "./Params.bs.js";

var baseLayer_000 = /* content : Fill */Block.__(0, ["black"]);

var baseLayer = /* record */[
  baseLayer_000,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none"
];

var analyzer_000 = /* content : Analysis */Block.__(4, [/* Mic */1]);

var analyzer = /* record */[
  analyzer_000,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none"
];

var webcam_000 = /* content : Webcam */Block.__(2, [/* record */[/* slitscan */undefined]]);

var webcam = /* record */[
  webcam_000,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none"
];

var slitscan_000 = /* content : Webcam */Block.__(2, [/* record */[/* slitscan *//* record */[/* x */320]]]);

var slitscan = /* record */[
  slitscan_000,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none"
];

var reader_000 = /* content : Reader */Block.__(6, [/* R */0]);

var reader = /* record */[
  reader_000,
  /* alpha */0.1,
  /* compositeOperation : SourceOver */0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none"
];

function pitchFilter(pc) {
  return /* record */[
          /* content : PitchClasses */Block.__(5, [pc]),
          /* alpha */1.0,
          /* compositeOperation : DestinationOut */6,
          /* transformMatrix */Canvas$Gayer.defaultTransform,
          /* filters */"none"
        ];
}

function fill($staropt$star, fillStyle) {
  var alpha = $staropt$star !== undefined ? $staropt$star : 1.0;
  return /* record */[
          /* content : Fill */Block.__(0, [fillStyle]),
          /* alpha */alpha,
          /* compositeOperation : SourceOver */0,
          /* transformMatrix */Canvas$Gayer.defaultTransform,
          /* filters */"none"
        ];
}

function draw($staropt$star, cmds) {
  var alpha = $staropt$star !== undefined ? $staropt$star : 1.0;
  return /* record */[
          /* content : Draw */Block.__(1, [cmds]),
          /* alpha */alpha,
          /* compositeOperation : SourceOver */0,
          /* transformMatrix */Canvas$Gayer.defaultTransform,
          /* filters */"none"
        ];
}

function img(url) {
  return /* record */[
          /* content : Image */Block.__(3, [url]),
          /* alpha */1.0,
          /* compositeOperation : SourceOver */0,
          /* transformMatrix */Canvas$Gayer.defaultTransform,
          /* filters */"none"
        ];
}

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

var init = img("media/harmony.png");

var init$1 = draw(undefined, /* :: */[
      /* DrawImage */Block.__(2, [
          /* Self */0,
          /* record */[
            /* x */0,
            /* y */-24,
            /* w */120,
            /* h */120
          ]
        ]),
      /* :: */[
        /* DrawImage */Block.__(2, [
            /* Self */0,
            /* record */[
              /* x */0,
              /* y */-48,
              /* w */120,
              /* h */120
            ]
          ]),
        /* [] */0
      ]
    ]);

var harmony_000 = /* record */[
  /* content */init[/* content */0],
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
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
    /* transformMatrix */init$1[/* transformMatrix */3],
    /* filters */init$1[/* filters */4]
  ],
  /* :: */[
    pitchFilter(Music$Gayer.cMajor),
    /* :: */[
      /* record */[
        /* content : Reader */Block.__(6, [/* R */0]),
        /* alpha */0.0,
        /* compositeOperation : SourceOver */0,
        /* transformMatrix */Canvas$Gayer.defaultTransform,
        /* filters */"none"
      ],
      /* [] */0
    ]
  ]
];

var harmony = /* :: */[
  harmony_000,
  harmony_001
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
            reader,
            /* [] */0
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
      /* content : Analysis */Block.__(4, [/* Mic */1]),
      /* alpha */0.5,
      /* compositeOperation : SourceOver */0,
      /* transformMatrix */Canvas$Gayer.defaultTransform,
      /* filters */"none"
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
    /* alpha */1.0,
    /* compositeOperation : SourceOver */0,
    /* transformMatrix */Canvas$Gayer.defaultTransform,
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

var slitscanParams_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var slitscanParams_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var slitscanParams_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var slitscanParams_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var slitscanParams_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var slitscanParams_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var slitscanParams_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var slitscanParams_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var slitscanParams_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var slitscanParams_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var slitscanParams_011 = /* layers : :: */[
  slitscan,
  /* :: */[
    /* record */[
      /* content : Analysis */Block.__(4, [/* Mic */1]),
      /* alpha */0.25,
      /* compositeOperation : SourceOver */0,
      /* transformMatrix */Canvas$Gayer.defaultTransform,
      /* filters */"none"
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
  slitscanParams_005,
  slitscanParams_006,
  slitscanParams_007,
  slitscanParams_008,
  slitscanParams_009,
  /* shouldClear */false,
  slitscanParams_011
];

var isItACrime_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var isItACrime_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var isItACrime_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var isItACrime_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var isItACrime_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var isItACrime_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var isItACrime_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var isItACrime_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var isItACrime_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var isItACrime_011 = /* layers : :: */[
  img("media/is_it_a_crime.png"),
  /* :: */[
    /* record */[
      /* content : Reader */Block.__(6, [/* A */3]),
      /* alpha */0.1,
      /* compositeOperation : SourceOver */0,
      /* transformMatrix */Canvas$Gayer.defaultTransform,
      /* filters */"none"
    ],
    /* [] */0
  ]
];

var isItACrime = /* record */[
  isItACrime_000,
  isItACrime_001,
  isItACrime_002,
  isItACrime_003,
  /* millisPerTick */33,
  isItACrime_005,
  isItACrime_006,
  /* outputGain */0.2,
  isItACrime_008,
  isItACrime_009,
  isItACrime_010,
  isItACrime_011
];

var tughra_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var tughra_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var tughra_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var tughra_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var tughra_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var tughra_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var tughra_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var tughra_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var tughra_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var tughra_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var tughra_011 = /* layers : :: */[
  img("media/suleiman.jpg"),
  /* :: */[
    /* record */[
      /* content : Reader */Block.__(6, [/* R */0]),
      /* alpha */0.25,
      /* compositeOperation : SourceOver */0,
      /* transformMatrix */Canvas$Gayer.defaultTransform,
      /* filters */"none"
    ],
    /* [] */0
  ]
];

var tughra = /* record */[
  tughra_000,
  tughra_001,
  tughra_002,
  tughra_003,
  /* millisPerTick */33,
  tughra_005,
  tughra_006,
  tughra_007,
  tughra_008,
  tughra_009,
  tughra_010,
  tughra_011
];

var debussyFile_000 = /* content : Analysis */Block.__(4, [/* AudioFile */["media/la_cathedrale_engloutie.m4a"]]);

var debussyFile = /* record */[
  debussyFile_000,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none"
];

var debussy_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var debussy_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var debussy_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var debussy_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var debussy_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var debussy_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var debussy_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var debussy_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var debussy_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var debussy_011 = /* layers : :: */[
  debussyFile,
  /* :: */[
    reader,
    /* [] */0
  ]
];

var debussy = /* record */[
  debussy_000,
  debussy_001,
  debussy_002,
  debussy_003,
  debussy_004,
  /* audioInputSetting : PinkNoise */0,
  debussy_006,
  debussy_007,
  debussy_008,
  debussy_009,
  /* shouldClear */false,
  debussy_011
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
  /* record */[
    /* content : Draw */Block.__(1, [/* :: */[
          /* SetFillStyle */Block.__(0, ["red"]),
          /* :: */[
            /* FillRect */Block.__(1, [/* record */[
                  /* x */0,
                  /* y */60,
                  /* w */120,
                  /* h */1
                ]]),
            /* [] */0
          ]
        ]]),
    /* alpha */1.0,
    /* compositeOperation : SourceOver */0,
    /* transformMatrix */Canvas$Gayer.defaultTransform,
    /* filters */"none"
  ],
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
      /* DrawImage */Block.__(2, [
          /* Self */0,
          /* record */[
            /* x */-1,
            /* y */0,
            /* w */120,
            /* h */120
          ]
        ]),
      /* [] */0
    ]]);

var historyLayer = /* record */[
  historyLayer_000,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none"
];

var drosteLayer_000 = /* content : Draw */Block.__(1, [/* :: */[
      /* DrawImage */Block.__(2, [
          /* Self */0,
          /* record */[
            /* x */1,
            /* y */1,
            /* w */119,
            /* h */119
          ]
        ]),
      /* [] */0
    ]]);

var drosteLayer = /* record */[
  drosteLayer_000,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"hue-rotate(30deg)"
];

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
      pitchFilter(Music$Gayer.majorHexatonic),
      /* :: */[
        /* record */[
          /* content : Reader */Block.__(6, [/* R */0]),
          /* alpha */0.0,
          /* compositeOperation : SourceOver */0,
          /* transformMatrix */Canvas$Gayer.defaultTransform,
          /* filters */"none"
        ],
        /* [] */0
      ]
    ]
  ]
];

var history = /* record */[
  /* readPosDelta */0,
  /* writePosDelta */0,
  /* readPosOffset */119,
  /* writePosOffset */119,
  history_004,
  history_005,
  history_006,
  history_007,
  history_008,
  history_009,
  /* shouldClear */false,
  history_011
];

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
      pitchFilter(Music$Gayer.majorHexatonic),
      /* :: */[
        /* record */[
          /* content : Reader */Block.__(6, [/* R */0]),
          /* alpha */0.0,
          /* compositeOperation : SourceOver */0,
          /* transformMatrix */Canvas$Gayer.defaultTransform,
          /* filters */"none"
        ],
        /* [] */0
      ]
    ]
  ]
];

var droste = /* record */[
  /* readPosDelta */0,
  /* writePosDelta */0,
  /* readPosOffset */2,
  /* writePosOffset */0,
  droste_004,
  droste_005,
  droste_006,
  droste_007,
  droste_008,
  droste_009,
  /* shouldClear */false,
  droste_011
];

var midiKeyboard = /* record */[
  /* content : MIDIKeyboard */0,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0,
  /* transformMatrix */Canvas$Gayer.defaultTransform,
  /* filters */"none"
];

var midi_000 = /* readPosDelta */Params$Gayer.defaultParams[/* readPosDelta */0];

var midi_001 = /* writePosDelta */Params$Gayer.defaultParams[/* writePosDelta */1];

var midi_002 = /* readPosOffset */Params$Gayer.defaultParams[/* readPosOffset */2];

var midi_003 = /* writePosOffset */Params$Gayer.defaultParams[/* writePosOffset */3];

var midi_004 = /* millisPerTick */Params$Gayer.defaultParams[/* millisPerTick */4];

var midi_005 = /* audioInputSetting */Params$Gayer.defaultParams[/* audioInputSetting */5];

var midi_006 = /* inputGain */Params$Gayer.defaultParams[/* inputGain */6];

var midi_007 = /* outputGain */Params$Gayer.defaultParams[/* outputGain */7];

var midi_008 = /* q */Params$Gayer.defaultParams[/* q */8];

var midi_009 = /* transpose */Params$Gayer.defaultParams[/* transpose */9];

var midi_010 = /* shouldClear */Params$Gayer.defaultParams[/* shouldClear */10];

var midi_011 = /* layers : :: */[
  midiKeyboard,
  /* :: */[
    reader,
    /* [] */0
  ]
];

var midi = /* record */[
  midi_000,
  midi_001,
  midi_002,
  midi_003,
  midi_004,
  midi_005,
  midi_006,
  midi_007,
  midi_008,
  midi_009,
  midi_010,
  midi_011
];

var presets_000 = /* tuple */[
  "MIDI",
  midi
];

var presets_001 = /* :: */[
  /* tuple */[
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
  ],
  /* :: */[
    /* tuple */[
      "Droste",
      droste
    ],
    /* :: */[
      /* tuple */[
        "Single note",
        singleNote
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
              "History",
              history
            ],
            /* :: */[
              /* tuple */[
                "King Wen",
                iChing
              ],
              /* :: */[
                /* tuple */[
                  "Whiteboard",
                  whiteboardParams
                ],
                /* :: */[
                  /* tuple */[
                    "Slitscan",
                    slitscanParams
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
    ]
  ]
];

var presets = /* :: */[
  presets_000,
  presets_001
];

var defaultTransform = Canvas$Gayer.defaultTransform;

export {
  defaultTransform ,
  baseLayer ,
  analyzer ,
  webcam ,
  slitscan ,
  reader ,
  pitchFilter ,
  fill ,
  draw ,
  img ,
  hubble ,
  spacy ,
  harmony ,
  allLayerTypes ,
  harmonyParams ,
  harmonyIntensified ,
  feedback ,
  whiteboardParams ,
  slitscanParams ,
  isItACrime ,
  tughra ,
  debussyFile ,
  debussy ,
  iChing ,
  singleNote ,
  historyLayer ,
  drosteLayer ,
  history ,
  droste ,
  midiKeyboard ,
  midi ,
  presets ,
  
}
/* hubble Not a pure module */
