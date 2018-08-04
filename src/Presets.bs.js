// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as Block from "bs-platform/lib/es6/block.js";
import * as Audio$Gayer from "./Audio.bs.js";
import * as Music$Gayer from "./Music.bs.js";

var analyzer = /* record */[
  /* content : Analysis */0,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0
];

var webcam = /* record */[
  /* content : Webcam */Block.__(1, [/* record */[/* slitscan */undefined]]),
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0
];

var reader = /* record */[
  /* content : Reader */Block.__(4, [/* R */0]),
  /* alpha */0.0,
  /* compositeOperation : SourceOver */0
];

function pitchFilter(pc) {
  return /* record */[
          /* content : PitchClasses */Block.__(3, [pc]),
          /* alpha */1.0,
          /* compositeOperation : DestinationOut */6
        ];
}

function fill($staropt$star, fillStyle) {
  var alpha = $staropt$star !== undefined ? $staropt$star : 1.0;
  return /* record */[
          /* content : Fill */Block.__(0, [fillStyle]),
          /* alpha */alpha,
          /* compositeOperation : SourceOver */0
        ];
}

function img(url) {
  return /* record */[
          /* content : Image */Block.__(2, [url]),
          /* alpha */1.0,
          /* compositeOperation : SourceOver */0
        ];
}

var hubble_000 = /* content : Image */Block.__(2, ["media/hubble_ultra_deep_field.jpg"]);

var hubble = /* record */[
  hubble_000,
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0
];

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

var allLayerTypes_001 = /* :: */[
  analyzer,
  /* :: */[
    webcam,
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
];

var allLayerTypes = /* :: */[
  hubble,
  allLayerTypes_001
];

var defaultParams = /* record */[
  /* readPosDelta */1,
  /* writePosDelta */1,
  /* writePosOffset */0,
  /* audioInputSetting : PinkNoise */0,
  /* inputGain */1.0,
  /* outputGain */0.1,
  /* q */Audio$Gayer.defaultQ,
  /* transpose */0,
  /* shouldClear */true,
  /* layers */spacy
];

var feedback_009 = /* layers : :: */[
  webcam,
  /* :: */[
    /* record */[
      /* content : Analysis */0,
      /* alpha */0.5,
      /* compositeOperation : SourceOver */0
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
  /* readPosDelta */1,
  /* writePosDelta */1,
  /* writePosOffset */0,
  /* audioInputSetting : Mic */1,
  /* inputGain */1.0,
  /* outputGain */0.1,
  /* q */Audio$Gayer.defaultQ,
  /* transpose */0,
  /* shouldClear */true,
  feedback_009
];

var presets_000 = /* tuple */[
  "Default",
  defaultParams
];

var presets_001 = /* :: */[
  /* tuple */[
    "Feedback (may be loud!)",
    feedback
  ],
  /* :: */[
    /* tuple */[
      "Overstuffed",
      /* record */[
        /* readPosDelta */1,
        /* writePosDelta */1,
        /* writePosOffset */0,
        /* audioInputSetting : PinkNoise */0,
        /* inputGain */1.0,
        /* outputGain */0.1,
        /* q */Audio$Gayer.defaultQ,
        /* transpose */0,
        /* shouldClear */true,
        /* layers */allLayerTypes
      ]
    ],
    /* :: */[
      /* tuple */[
        "Empty",
        /* record */[
          /* readPosDelta */1,
          /* writePosDelta */1,
          /* writePosOffset */0,
          /* audioInputSetting : PinkNoise */0,
          /* inputGain */1.0,
          /* outputGain */0.1,
          /* q */Audio$Gayer.defaultQ,
          /* transpose */0,
          /* shouldClear */true,
          /* layers : [] */0
        ]
      ],
      /* [] */0
    ]
  ]
];

var presets = /* :: */[
  presets_000,
  presets_001
];

var slitscan = /* record */[
  /* content : Webcam */Block.__(1, [/* record */[/* slitscan *//* record */[/* x */60]]]),
  /* alpha */1.0,
  /* compositeOperation : SourceOver */0
];

export {
  analyzer ,
  webcam ,
  slitscan ,
  reader ,
  pitchFilter ,
  fill ,
  img ,
  hubble ,
  spacy ,
  allLayerTypes ,
  defaultParams ,
  feedback ,
  presets ,
  
}
/* spacy Not a pure module */
