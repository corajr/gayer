open Audio;
open Layer;
open Music;
open Params;

let analyzer = {
  content: Analysis(Mic),
  alpha: 1.0,
  compositeOperation: SourceOver,
};

let webcam = {
  content: Webcam({slitscan: None}),
  alpha: 1.0,
  compositeOperation: SourceOver,
};

let slitscan = {
  content: Webcam({slitscan: Some({x: 320})}),
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

let harmony = [
  img("media/harmony.png"),
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

let harmonyParams = {
  ...defaultParams,
  layers: harmony,
  transpose: (-48),
  shouldClear: false,
};

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
  content: Analysis(AudioFile("media/sade/is_it_a_crime.mp3")),
  alpha: 1.0,
  compositeOperation: SourceOver,
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
