open Audio;
open Layer;
open Music;
open Params;

let spacy = [
  {
    content: Image("media/hubble_ultra_deep_field.jpg"),
    alpha: 1.0,
    compositeOperation: SourceOver,
  },
  {content: Analysis, alpha: 0.5, compositeOperation: SourceOver},
  {
    content: Webcam({slitscan: None}),
    /* content: Webcam({slitscan: Some({x: 60})}), */
    alpha: 0.25,
    compositeOperation: SourceOver,
  },
  {content: Fill("white"), alpha: 0.0125, compositeOperation: SourceOver},
  {
    content: PitchClasses(cMajor),
    alpha: 1.0,
    compositeOperation: DestinationOut,
  },
  {content: Reader(R), alpha: 0.0, compositeOperation: SourceOver},
];

let allLayerTypes = [
  {
    content: Image("media/hubble_ultra_deep_field.jpg"),
    alpha: 1.0,
    compositeOperation: SourceOver,
  },
  {content: Analysis, alpha: 0.5, compositeOperation: SourceOver},
  {
    content: Webcam({slitscan: None}),
    /* content: Webcam({slitscan: Some({x: 60})}), */
    alpha: 0.25,
    compositeOperation: SourceOver,
  },
  {content: Fill("white"), alpha: 0.0125, compositeOperation: SourceOver},
  {
    content: PitchClasses(cMajor),
    alpha: 1.0,
    compositeOperation: DestinationOut,
  },
  {content: Reader(R), alpha: 0.0, compositeOperation: SourceOver},
];

let defaultParams: params = {
  readPosDelta: 1,
  writePosDelta: 1,
  writePosOffset: 0,
  audioInputSetting: PinkNoise,
  inputGain: 1.0,
  outputGain: 0.1,
  q: defaultQ,
  transpose: 0,
  shouldClear: true,
  layers: spacy,
};

let presets = [
  ("Default", defaultParams),
  ("Feedback (may be loud!)", {...defaultParams, audioInputSetting: Mic}),
  ("Overstuffed", {...defaultParams, layers: allLayerTypes}),
  ("Empty", {...defaultParams, layers: []}),
];
