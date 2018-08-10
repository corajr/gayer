open Audio;
open Canvas.DrawCommand;
open Layer;
open Music;
open Params;

let defaultTransform = Canvas.defaultTransform;

let baseLayer = {
  content: Fill("black"),
  alpha: 1.0,
  compositeOperation: SourceOver,
  transformMatrix: defaultTransform,
  filters: "none",
};

let analyzer = {...baseLayer, content: Analysis(Mic)};

let webcam = {...baseLayer, content: Webcam({slitscan: None})};

let slitscan = {...baseLayer, content: Webcam({slitscan: Some({x: 320})})};

let reader = {...baseLayer, content: Reader(R), alpha: 0.1};

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

let draw = (~alpha: float=1.0, cmds: list(command)) => {
  ...baseLayer,
  content: Draw(cmds),
  alpha,
};

let img = url => {...baseLayer, content: Image(url)};

let hubble = img("media/hubble_ultra_deep_field.jpg");

let spacy = [hubble, pitchFilter(cMajor), reader];

let harmony = [
  /* fill("black", ~alpha=0.1), */
  {
    ...img("media/harmony.png"),
    transformMatrix: {
      ...defaultTransform,
      verticalMoving: 48.0,
    },
    alpha: 1.0,
    compositeOperation: SourceOver,
    filters: "blur(2px)",
  },
  {
    ...
      draw([
        DrawImage(Self, {x: 0, y: (-24), w: 120, h: 120}),
        DrawImage(Self, {x: 0, y: (-48), w: 120, h: 120}),
      ]),
    compositeOperation: Difference,
    alpha: 0.5,
  },
  pitchFilter(cMajor),
  {...reader, alpha: 0.0},
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

let harmonyParams = {
  ...defaultParams,
  millisPerTick: 25,
  layers: harmony,
  /* shouldClear: false, */
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

let whiteboardParams = {
  ...defaultParams,
  layers: [
    {...webcam, filters: "contrast(300%) invert(100%)"},
    pitchFilter(pentatonic),
    reader,
  ],
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

let isItACrime = {
  ...defaultParams,
  outputGain: 0.2,
  millisPerTick: 33,
  layers: [
    img("media/is_it_a_crime.png"),
    {...reader, content: Reader(A)},
  ],
};

let tughra = {
  ...defaultParams,
  millisPerTick: 33,
  layers: [
    img("media/suleiman.jpg"),
    {...reader, content: Reader(R), alpha: 0.25},
  ],
};

let debussyFile = {
  ...baseLayer,
  content: Analysis(AudioFile("media/la_cathedrale_engloutie.m4a")),
};

let debussy = {
  ...defaultParams,
  shouldClear: false,
  audioInputSetting: PinkNoise,
  layers: [debussyFile, reader],
};

let iChing = {
  ...defaultParams,
  layers: [img("media/king_wen.png"), pitchFilter(majorHexatonic), reader],
};

let singleNote = {
  ...defaultParams,
  layers: [
    {
      ...baseLayer,
      content:
        Draw([SetFillStyle("red"), FillRect({x: 0, y: 60, w: 120, h: 1})]),
    },
    reader,
  ],
};

let historyLayer = {
  ...baseLayer,
  content: Draw([DrawImage(Self, {x: (-1), y: 0, w: 120, h: 120})]),
};

let history = {
  ...defaultParams,
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: 119,
  writePosOffset: 119,
  shouldClear: false,
  layers: [
    analyzer,
    historyLayer,
    pitchFilter(majorHexatonic),
    {...reader, alpha: 0.0},
  ],
};

let presets = [
  ("Spacy", {...defaultParams, layers: spacy}),
  ("Single note", singleNote),
  ("Tughra of Suleiman", tughra),
  ("Is it a crime?", isItACrime),
  /* ("Harmony", harmonyParams), */
  ("History", history),
  ("King Wen", iChing),
  ("Whiteboard", whiteboardParams),
  ("Slitscan", slitscanParams),
  /* ("Debussy", debussy), */
  ("Mic feedback (may be loud!)", feedback),
  ("Empty", {...defaultParams, layers: []}),
];
