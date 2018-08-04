open Audio;
open Layer;
open Music;
open Params;

let analyzer = {
  content: Analysis,
  alpha: 1.0,
  compositeOperation: SourceOver,
};

let webcam = {
  content: Webcam({slitscan: None}),
  /* content: Webcam({slitscan: Some({x: 60})}), */
  alpha: 1.0,
  compositeOperation: SourceOver,
};

let slitscan = {
  content: Webcam({slitscan: Some({x: 60})}),
  alpha: 1.0,
  compositeOperation: SourceOver,
};

let reader = {
  content: Reader(R),
  alpha: 0.0,
  compositeOperation: SourceOver,
};

let pitchFilter = pc => {
  content: PitchClasses(pc),
  alpha: 1.0,
  compositeOperation: DestinationOut,
};

let fill = (~alpha: float=1.0, fillStyle: string) => {
  content: Fill(fillStyle),
  alpha,
  compositeOperation: SourceOver,
};

let img = url => {
  content: Image(url),
  alpha: 1.0,
  compositeOperation: SourceOver,
};

let hubble = img("media/hubble_ultra_deep_field.jpg");

let spacy = [hubble, pitchFilter(cMajor), reader];

let allLayerTypes = [
  hubble,
  analyzer,
  webcam,
  fill(~alpha=0.0125, "white"),
  pitchFilter(cMajor),
  reader,
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

let feedback = {
  ...defaultParams,
  audioInputSetting: Mic,
  layers: [webcam, {...analyzer, alpha: 0.5}, pitchFilter(cMajor), reader],
};

let presets = [
  ("Default", defaultParams),
  ("Feedback (may be loud!)", feedback),
  ("Overstuffed", {...defaultParams, layers: allLayerTypes}),
  ("Empty", {...defaultParams, layers: []}),
];
