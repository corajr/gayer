open Audio;
open Canvas.DrawCommand;
open Layer;
open Music;
open Params;
open Score;
open RawAudio;

let defaultSize = Canvas.defaultSize;
let defaultTransform = Canvas.defaultTransform;
let tau = Canvas.tau;

/* ## Layer definitions */
/* */
/* GAYER is built up from a series of layers. Convenience methods are provided
   here for creating several commonly used layers. */

let img = url => {...defaultLayer, content: Image(url)};
let video = url => {...defaultLayer, content: Video(url)};

let reader = {...defaultLayer, content: Reader(Channel(R))};

let saturationReader = {...defaultLayer, content: Reader(Saturation)};

let keycodeReader = {...defaultLayer, content: KeycodeReader};

let keycodeWriter = {...defaultLayer, content: KeycodeWriter};

let histogram = {
  ...defaultLayer,
  content: Histogram,
  alpha: 1.0,
  compositeOperation: SourceOver,
};

let rawAudioFormat = {
  x: 0,
  y: 0,
  w: 64,
  h: 32,
  encoding: Int8(R),
  sampleRate: 44100,
};

let rawAudioWriter = {
  ...defaultLayer,
  content: RawAudioWriter(rawAudioFormat),
};
let rawAudioReader = {
  ...defaultLayer,
  content: RawAudioReader(rawAudioFormat),
};

let pitchFilter = pc => {...defaultLayer, content: PitchClasses(pc)};

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

type fillOrStroke =
  | Fill
  | Stroke;

let text =
    (
      ~x: length=Divide(Width, Constant(2)),
      ~y: length=Divide(Height, Constant(2)),
      ~font: string="48px monospace",
      ~align: string="center",
      ~baseline: string="middle",
      ~color: string="white",
      ~fillOrStroke: fillOrStroke=Fill,
      s: string,
    ) => {
  let drawTextCommands =
    switch (fillOrStroke) {
    | Fill => [SetFillStyle(color), FillText(s, x, y)]
    | Stroke => [SetStrokeStyle(color), StrokeText(s, x, y)]
    };

  draw([
    SetFont(font),
    SetTextAlign(align),
    SetTextBaseline(baseline),
    ...drawTextCommands,
  ]);
};

let regl = {...defaultLayer, content: Regl};

let sobel = {...defaultLayer, content: Regl};

let analyzer = {...defaultLayer, content: Analysis(Mic)};

let webcam = {...defaultLayer, content: Webcam};

let slitscan = {
  ...defaultLayer,
  content: Slitscan({sourceLayerKey: "webcam", slitscan: StaticX(320)}),
};

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
  sobel,
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
  pitchFilter(cSharpMajor),
  reader,
];

let rotateLayer =
  draw([
    Translate(Pixels(defaultSize / 2), Pixels(defaultSize / 2)),
    Rotate(oneCompleteTurnAfterNTicks(defaultSize / 2)),
    Translate(
      Negate(Pixels(defaultSize / 2)),
      Negate(Pixels(defaultSize / 2)),
    ),
    drawSelfFullScreen,
  ]);

let blurLayer = {...draw([drawSelfFullScreen]), filters: "blur(2px)"};

let squareColumnLayer = {
  ...
    draw([
      DrawImageSourceDest(
        Self,
        {
          x: Add(Width, Pixels(-1)),
          y: Pixels(0),
          w: Pixels(1),
          h: Height,
        },
        {
          x: Add(Width, Pixels(-1)),
          y: Pixels(0),
          w: Pixels(1),
          h: Height,
        },
      ),
    ]),
  alpha: 0.75,
  compositeOperation: Multiply,
};

let squareLayer = {
  ...defaultLayer,
  content:
    Draw([
      DrawImage(Self, {x: Pixels(0), y: Pixels(0), w: Width, h: Height}),
    ]),
  compositeOperation: Multiply,
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

let historyBackAndForthLayer = {
  ...defaultLayer,
  content:
    Draw([
      DrawImageSourceDest(
        Self,
        {
          x: Divide(Width, Constant(2)),
          y: Pixels(0),
          w: Divide(Width, Constant(2)),
          h: Height,
        },
        {
          x: Add(Divide(Width, Constant(2)), Pixels(1)),
          y: Pixels(0),
          w: Divide(Width, Constant(2)),
          h: Height,
        },
      ),
      DrawImageSourceDest(
        Self,
        {
          x: Pixels(1),
          y: Pixels(0),
          w: Divide(Width, Constant(2)),
          h: Height,
        },
        {
          x: Pixels(0),
          y: Pixels(0),
          w: Divide(Width, Constant(2)),
          h: Height,
        },
      ),
    ]),
};

let drosteLayer = {
  ...defaultLayer,
  content:
    Draw([
      DrawImage(
        Self,
        {
          x: Pixels(1),
          y: Pixels(1),
          w: Add(Width, Pixels(-1)),
          h: Add(Height, Pixels(-1)),
        },
      ),
    ]),
  filters:
    "hue-rotate(" ++ Js.Float.toString(1.0 /. 30.0 *. (5.0 /. 6.0)) ++ "turn)",
};

let midiKeyboard = {...defaultLayer, content: MIDIKeyboard};

/* TODO: think of a more elegant way to do this */
let midiColors = {
  ...defaultLayer,
  content: Draw(MIDICanvas.makeNoteColors(MIDICanvas.oneRainbow)),
  compositeOperation: Multiply,
};

let handDrawn = {...defaultLayer, content: HandDrawn};

let allLayerTypes = [
  hubble,
  analyzer,
  webcam,
  slitscan,
  handDrawn,
  fill(~alpha=0.0125, "white"),
  pitchFilter(cMajor),
  blurLayer,
  rotateLayer,
  squareColumnLayer,
  squareLayer,
  rawAudioWriter,
  rawAudioReader,
  saturationReader,
  reader,
];

/* ## Param definitions */
/* These are the params definitions, which include not only a list of layers but
   a variety of other parameters as well. Complete definitions are available in
   Params.re. */

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

let webcamParams = {
  ...defaultParams,
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: 0,
  writePosOffset: 0,
  layers: [
    {
      ...analyzer,
      transformMatrix: {
        ...defaultTransform,
        horizontalScaling: float_of_int(defaultSize),
      },
    },
    {...webcam, compositeOperation: Multiply},
    reader,
  ],
};

let webcamEdgeDetect = {
  ...defaultParams,
  layers: [webcam, sobel, pitchFilter(cMajor), reader],
};

let slitscanParams = {
  ...defaultParams,
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: defaultSize - 1,
  writePosOffset: defaultSize - 1,
  shouldClear: false,
  layers: [
    analyzer,
    /* squareColumnLayer, */
    {...webcam, alpha: 0.0},
    {...slitscan, compositeOperation: Overlay},
    /* pitchFilter(cMajor), */
    historyLayer,
    reader,
  ],
};

let slitscanEdgeDetectParams = {
  ...defaultParams,
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: defaultSize - 1,
  writePosOffset: defaultSize - 1,
  shouldClear: false,
  layers: [
    slitscan,
    /* sobel, */
    analyzer,
    /* pitchFilter(cMajor), */
    historyLayer,
    reader,
  ],
};

let slitscanHistogramParams = {
  ...slitscanParams,
  layers: [
    slitscan,
    histogram,
    {...analyzer, compositeOperation: Screen},
    historyLayer,
    reader,
  ],
};
let whiteboardParams = {
  ...defaultParams,
  layers: [
    {...webcam, filters: "contrast(300%) invert(100%)"},
    pitchFilter(pentatonic),
    reader,
  ],
};

let isItACrime = {
  ...defaultParams,
  layers: [
    img("media/is_it_a_crime_large.png"),
    {...reader, content: Reader(Channel(A))},
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
    /* blurLayer, */
    /* {...squareColumnLayer, alpha: 1.0}, */
    /* {...pitchFilter(cMajor), alpha: 0.01}, */
    {...reader, alpha: 0.0},
  ],
};

let historyHalving = {
  ...defaultParams,
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: defaultSize - 1,
  writePosOffset: defaultSize - 1,
  shouldClear: false,
  layers: [
    analyzer,
    squareColumnLayer,
    historyLayer,
    draw([
      DrawImage(
        Self,
        {
          x: Pixels(0),
          y: Pixels(0),
          w: Divide(Width, Constant(2)),
          h: Height,
        },
      ),
    ]),
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
  layers: [
    {
      ...analyzer,
      transformMatrix: {
        ...defaultTransform,
        horizontalScaling: float_of_int(defaultSize),
      },
    },
    drosteLayer,
    reader,
  ],
};

let fourSeasons = {
  ...defaultParams,
  layers: [
    img("media/four_seasons.jpg"),
    /* sobel, */
    /* histogram, */
    pitchFilter(cMinor),
    saturationReader,
    /* reader, */
  ],
};

let handDrawnParams = {
  ...defaultParams,
  layers: [fill("black"), handDrawn, reader],
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

let historyBackAndForth = {
  ...readFromCenterLine,
  layers: [analyzer, historyBackAndForthLayer, reader],
};

let vinyl = {...readFromCenterLine, layers: [rotateLayer, analyzer, reader]};

let videoURL = "media/nonfree/kishi_bashi-say_yeah.mp4";
let video = {
  ...defaultParams,
  layers: [
    video(videoURL),
    {...analyzer, content: Analysis(AudioFromVideo(videoURL))},
    /* pitchFilter(cMajor), */
    reader,
  ],
};

let lesTresRichesHeures = {
  ...defaultParams,
  outputGain: 0.05,
  layers: [
    img("media/les_tres_riches_heures.jpg"),
    sobel,
    pitchFilter(majorHexatonic),
    reader,
  ],
};

let histogram = {
  ...defaultParams,
  layers: [hubble, pitchFilter(cMajor), histogram],
};

let rawAudio = {...defaultParams, layers: [rawAudioWriter, rawAudioReader]};

let rawAudioAndSpacy = {
  ...defaultParams,
  layers: List.append(spacy, [rawAudioWriter, rawAudioReader]),
};

let keycodeParams = {
  ...defaultParams,
  layers: [
    fill("black"),
    keycodeWriter,
    reader,
    {...keycodeReader, alpha: 0.5},
  ],
};

let welcomeAudio = {
  ...defaultParams,
  layers: [
    fill("black"),
    text("GAYER", ~color="red", ~fillOrStroke=Stroke),
    saturationReader,
  ],
};

let presetsWithoutLayerIds = [
  ("Keycode", keycodeParams),
  ("Welcome", {...defaultParams, layers: [fill("black"), text("GAYER")]}),
  /* ("Spectrogram", {...defaultParams, layers: [analyzer]}), */
  ("Welcome (Audio)", welcomeAudio),
  /* ("Regl", {...defaultParams, layers: [regl]}), */
  ("Spacy", {...defaultParams, layers: spacy}),
  ("Single note", singleNote),
  /* ("Hand-drawn", handDrawnParams), */
  /* ("Webcam", webcamEdgeDetect), */
  ("Webcam (edge detection)", webcamEdgeDetect),
  ("Slitscan", slitscanParams),
  /* ("Slitscan (edge detection)", slitscanEdgeDetectParams), */
  /* ("Slitscan (color histogram)", slitscanHistogramParams), */
  ("History", history),
  /* ("History (-|-)", historyBackAndForth), */
  /* ("Video", video), */
  ("Rotation", vinyl),
  /* ("Angle", droste), */
  ("Tughra of Suleiman", tughra),
  ("Four Seasons", fourSeasons),
  ({js|Les Très Riches Heures|js}, lesTresRichesHeures),
  ("Is it a crime?", isItACrime),
  ("MIDI (requires MIDI keyboard)", midi),
  ("Audio file", debussy),
  ("Harmony", harmonyParams),
  /* ("King Wen", iChing), */
  /* ("Whiteboard", whiteboardParams), */
  /* ("Mic feedback (may be loud!)", feedback), */
  /* ("Raw audio (can feedback!)", rawAudio), */
  /* ("Raw audio and spacy", rawAudioAndSpacy), */
  ("Empty", {...defaultParams, layers: []}),
];

let idCounter = ref(0);

let addIds =
  List.map(layer => {
    let nextId = idCounter^;
    idCounter := nextId + 1;
    {...layer, id: Some(string_of_int(nextId))};
  });

let presets: list((string, params)) =
  List.map(
    ((name, preset)) => (
      name,
      {...preset, layers: addIds(preset.layers)},
    ),
    presetsWithoutLayerIds,
  );

let exampleScore: score = {
  events:
    Array.map(
      ((eventTitle, params)) => {
        params,
        transition: Manual,
        eventTitle: Some(eventTitle),
      },
      Array.of_list(presets),
    ),
  scoreMetadata: {
    title: "Example",
    authors: ["@cora_jr"],
  },
};
