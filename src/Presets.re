open Audio;
open Layer;
open Music;
open Params;

let defaultTransform = Canvas.defaultTransform;

let baseLayer = {
  content: Fill("black"),
  alpha: 1.0,
  compositeOperation: SourceOver,
  transformMatrix: defaultTransform,
};

let analyzer = {...baseLayer, content: Analysis(Mic)};

let webcam = {...baseLayer, content: Webcam({slitscan: None})};

let slitscan = {...baseLayer, content: Webcam({slitscan: Some({x: 320})})};

let reader = {...baseLayer, content: Reader(R), alpha: 0.0};

let pitchFilter = pc => {
  ...baseLayer,
  content: PitchClasses(pc),
  compositeOperation: DestinationOut,
};

let fill = (~alpha: float=1.0, fillStyle: string) => {
  ...baseLayer,
  content: Fill(fillStyle),
  alpha,
};

let img = url => {...baseLayer, content: Image(url)};

let hubble = img("media/hubble_ultra_deep_field.jpg");

let spacy = [hubble, pitchFilter(cMajor), reader];

let harmony = [
  fill("black", ~alpha=0.1),
  {
    ...img("media/harmony.png"),
    transformMatrix: {
      ...defaultTransform,
      verticalMoving: 48.0,
    },
  },
  {...analyzer, alpha: 0.1, compositeOperation: Overlay},
  {...slitscan, alpha: 0.1, compositeOperation: Overlay},
  pitchFilter(cMajor),
  reader,
];

let allLayerTypes = [
  hubble,
  analyzer,
  webcam,
  slitscan,
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

let harmonyParams = {...defaultParams, layers: harmony, shouldClear: false};

let harmonyIntensified = {
  ...harmonyParams,
  layers: [img("media/harmony_intensified.png"), reader],
};

let feedback = {
  ...defaultParams,
  audioInputSetting: Mic,
  layers: [webcam, {...analyzer, alpha: 0.5}, pitchFilter(cMajor), reader],
};

let slitscanParams = {
  ...defaultParams,
  shouldClear: false,
  layers: [
    slitscan,
    {...analyzer, alpha: 0.25},
    pitchFilter(cMajor),
    reader,
  ],
};

let debussyFile = {
  ...baseLayer,
  content: Analysis(AudioFile("media/sade/is_it_a_crime.mp3")),
};

let debussy = {
  ...defaultParams,
  shouldClear: false,
  audioInputSetting: PinkNoise,
  layers: [debussyFile, reader],
};

let presets = [
  ("Harmony", harmonyParams),
  /* ("Harmony (intensified)", harmonyIntensified), */
  ("Slitscan", slitscanParams),
  ("Debussy", debussy),
  ("Feedback (may be loud!)", feedback),
  ("Overstuffed", {...defaultParams, layers: allLayerTypes}),
  ("Spacy", {...defaultParams, layers: spacy}),
  ("Empty", {...defaultParams, layers: []}),
];
