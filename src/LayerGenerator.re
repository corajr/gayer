open AnalysisOptions;
open Audio.AudioInput;
open Canvas.DrawCommand;
open Layer;
open Music;
open RawAudio;
open Regl;

/* ## Layer definitions */
/* */
/* GAYER is built up from a series of layers. Convenience methods are provided
   here for creating several commonly used layers. */

let defaultSize = Canvas.defaultSize;
let defaultTransform = Canvas.defaultTransform;
let tau = Canvas.tau;

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

let sobel = key => {
  ...defaultLayer,
  content: Regl(Sobel({sourceLayer: key})),
};

let displace = (source, displace) => {
  ...defaultLayer,
  content:
    Regl(
      Displacement({
        displacementSourceLayer: source,
        displacementMap: displace,
      }),
    ),
};

let analyzer = (input: audioInputSetting) => {
  ...defaultLayer,
  content: Analysis({...defaultAnalysisOptions, input}),
};

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
  sobel("root"),
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

let idCounter = ref(0);

let allLayerTypes = [|
  ("image", hubble),
  ("analyzer", analyzer(Mic)),
  ("reader", reader),
  ("webcam", webcam),
  ("slitscan", slitscan),
  ("edge detect", sobel("root")),
  ("displace", displace("root", "root")),
  ("midi-keyboard", midiKeyboard),
  ("computer keyboard", keycodeWriter),
  ("ASCII", keycodeReader),
  ("mouse-draw", handDrawn),
  ("draw (commands)", draw([SetFillStyle("red")])),
  ("fill", fill(~alpha=0.0125, "white")),
  ("pitch filter", pitchFilter(cMajor)),
  ("blur", blurLayer),
  ("rotate", rotateLayer),
  ("square values (column)", squareColumnLayer),
  ("square values (whole image)", squareLayer),
  ("raw-audio-writer", rawAudioWriter),
  ("raw-audio-reader", rawAudioReader),
  ("saturation reader", saturationReader),
|];
