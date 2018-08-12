open Audio;
open Canvas.DrawCommand;
open Layer;
open Music;
open Params;

let defaultSize = Canvas.defaultSize;
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

let reader = {
  ...baseLayer,
  content: Reader(R),
  alpha: 1.0,
  compositeOperation: Multiply,
};

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
        DrawImage(
          Self,
          {x: Pixels(0), y: Pixels(-24), w: Width, h: Height},
        ),
        DrawImage(
          Self,
          {x: Pixels(0), y: Pixels(-48), w: Width, h: Height},
        ),
      ]),
    compositeOperation: Difference,
    alpha: 0.5,
  },
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

let squareLayer = {
  ...baseLayer,
  content:
    Draw([
      DrawImage(Self, {x: Pixels(0), y: Pixels(0), w: Width, h: Height}),
    ]),
  compositeOperation: Multiply,
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
  layers: [img("media/suleiman.jpg"), reader],
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
        Draw([
          SetFillStyle("red"),
          FillRect({x: Pixels(0), y: Note(60), w: Width, h: Pixels(1)}),
        ]),
    },
    reader,
  ],
};

let historyLayer = {
  ...baseLayer,
  content:
    Draw([
      DrawImage(Self, {x: Pixels(-1), y: Pixels(0), w: Width, h: Height}),
    ]),
};

let drosteLayer = {
  ...baseLayer,
  content:
    Draw([
      DrawImage(Self, {x: Pixels(1), y: Pixels(0), w: Width, h: Height}),
    ]),
  filters:
    "hue-rotate(" ++ Js.Float.toString(1.0 /. 30.0 *. (5.0 /. 6.0)) ++ "turn)",
};

let history = {
  ...defaultParams,
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: defaultSize - 1,
  writePosOffset: defaultSize - 1,
  shouldClear: false,
  layers: [
    analyzer,
    /* squareLayer, */
    historyLayer,
    /* pitchFilter(majorHexatonic), */
    {...reader, alpha: 0.0},
  ],
};

let debussyFile = {
  ...baseLayer,
  content: Analysis(AudioFile("media/la_cathedrale_engloutie.m4a")),
  /* content: Analysis(AudioFile("media/sade/is_it_a_crime.mp3")), */
};

let debussy = {...history, layers: [debussyFile, historyLayer, reader]};

let droste = {
  ...defaultParams,
  readPosDelta: 0,
  writePosDelta: 0,
  shouldClear: false,
  layers: [analyzer, drosteLayer, reader],
};

let midiKeyboard = {...baseLayer, content: MIDIKeyboard};

/* TODO: think of a more elegant way to do this */
let midiColors = {
  ...baseLayer,
  content: Draw(MIDICanvas.makeNoteColors(MIDICanvas.oneRainbow)),
  compositeOperation: Multiply,
};

let midi = {...history, layers: [midiKeyboard, historyLayer, reader]};

let midiDroste = {...droste, layers: [midiKeyboard, drosteLayer, reader]};

let presets = [
  ("Spacy", {...defaultParams, layers: spacy}),
  ("Droste", droste),
  ("Single note", singleNote),
  ("Tughra of Suleiman", tughra),
  ("Is it a crime?", isItACrime),
  ("MIDI", midi),
  /* ("Debussy", debussy), */
  /* ("Harmony", harmonyParams), */
  ("History", history),
  ("King Wen", iChing),
  ("Whiteboard", whiteboardParams),
  ("Slitscan", slitscanParams),
  ("Mic feedback (may be loud!)", feedback),
  ("Empty", {...defaultParams, layers: []}),
];
