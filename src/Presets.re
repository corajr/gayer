open Audio;
open Canvas.DrawCommand;
open Layer;
open Music;
open Params;

let defaultSize = Canvas.defaultSize;
let defaultTransform = Canvas.defaultTransform;

let analyzer = {...defaultLayer, content: Analysis(Mic)};

let webcam = {...defaultLayer, content: Webcam({slitscan: None})};

let slitscan = {
  ...defaultLayer,
  content: Webcam({slitscan: Some({x: 320})}),
};

let reader = {
  ...defaultLayer,
  content: Reader(R),
  alpha: 1.0,
  compositeOperation: Multiply,
};

let pitchFilter = pc => {
  ...defaultLayer,
  content: PitchClasses(pc),
  compositeOperation: DestinationOut,
};

let fill = (~alpha: float=1.0, fillStyle: string) => {
  ...defaultLayer,
  content: Fill(fillStyle),
  alpha,
};

let draw = (~alpha: float=1.0, cmds: list(command)) => {
  ...defaultLayer,
  content: Draw(cmds),
  alpha,
};

let img = url => {...defaultLayer, content: Image(url)};

let hubble = img("media/hubble_ultra_deep_field.jpg");

let spacy = [hubble, pitchFilter(cMajor), reader];

let drawSelfFullScreen =
  DrawImage(Self, {x: Pixels(0), y: Pixels(0), w: Width, h: Height});

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

let rotateLayer = {
  ...draw([drawSelfFullScreen]),
  transformMatrix: {
    ...defaultTransform,
    horizontalMoving: float_of_int(defaultSize) *. 0.5,
    verticalMoving: float_of_int(defaultSize) *. (-0.5),
  },
  rotation: oneCompleteTurnAfterNTicks(defaultSize),
};

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
  ...defaultLayer,
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
  layers: [
    img("media/is_it_a_crime_large.png"),
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

let singleNoteDrawCommands = note => [
  SetFillStyle("red"),
  FillRect({x: Pixels(0), y: Note(60), w: Width, h: Pixels(1)}),
];

let singleNoteLayer = note => {
  ...defaultLayer,
  content: Draw(singleNoteDrawCommands(note)),
};

let singleNote = {...defaultParams, layers: [singleNoteLayer(60), reader]};

let historyLayer = {
  ...defaultLayer,
  content:
    Draw([
      DrawImage(Self, {x: Pixels(-1), y: Pixels(0), w: Width, h: Height}),
    ]),
};

let drosteLayer = {
  ...defaultLayer,
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
  ...defaultLayer,
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

let midiKeyboard = {...defaultLayer, content: MIDIKeyboard};

/* TODO: think of a more elegant way to do this */
let midiColors = {
  ...defaultLayer,
  content: Draw(MIDICanvas.makeNoteColors(MIDICanvas.oneRainbow)),
  compositeOperation: Multiply,
};

let midi = {...history, layers: [midiKeyboard, historyLayer, reader]};

let midiDroste = {...droste, layers: [midiKeyboard, drosteLayer, reader]};

let readFromCenterLine = {
  ...history,
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: defaultSize / 2,
  writePosOffset: defaultSize / 2,
};

let sun = {
  ...readFromCenterLine,
  layers: [
    draw([
      Translate(Pixels(defaultSize / 2), Pixels(defaultSize / 2)),
      Rotate(oneCompleteTurnAfterNTicks(defaultSize)),
      Translate(Negate(Pixels(defaultSize / 2)), Pixels(defaultSize / 2)),
      FillRect({x: Pixels(0), y: Pixels(0), w: Width, h: Pixels(1)}),
      /* ...singleNoteDrawCommands(60), */
    ]),
    reader,
  ],
};

let presets = [
  ("Sun", sun),
  ("Spacy", {...defaultParams, layers: spacy}),
  ("Droste", droste),
  ("Single note", singleNote),
  ("Tughra of Suleiman", tughra),
  ("Is it a crime?", isItACrime),
  ("MIDI", midi),
  ("Audio file", debussy),
  /* ("Harmony", harmonyParams), */
  ("History", history),
  ("King Wen", iChing),
  ("Whiteboard", whiteboardParams),
  ("Slitscan", slitscanParams),
  ("Mic feedback (may be loud!)", feedback),
  ("Empty", {...defaultParams, layers: []}),
];
