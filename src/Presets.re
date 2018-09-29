open Audio;
open DrawCommand;
open Layer;
open LayerGenerator;
open Music;
open Params;
open Score;
open RawAudio;

/* ## Param definitions */
/* These are the params definitions, which include not only a list of layers but
   a variety of other parameters as well. Complete definitions are available in
   Params.re. */

let singleNote = {
  ...defaultParams,
  millisPerTick: 100,
  layers: [
    text(
      "GAYER is a tool to help you turn images into sound (and vice versa).\n"
      ++ "You should be hearing a single note -- MIDI note 60, AKA middle C.\n\n"
      ++ "If you don't hear anything, please check your volume settings.",
    ),
    singleNoteLayer(60),
    reader,
  ],
};

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
  layers: [
    webcam,
    {...analyzer(Mic), alpha: 0.5},
    pitchFilter(cMajor),
    reader,
  ],
};

let webcamParams = {
  ...defaultParams,
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: 0,
  writePosOffset: 0,
  layers: [
    {
      ...analyzer(Mic),
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
  layers: [
    /* {...webcam, enabled: false}, */
    webcam,
    sobel("webcam"),
    pitchFilter(cMajor),
    reader,
  ],
};

let slitscanParams = {
  ...defaultParams,
  shouldClear: false,
  layers: [
    analyzer(Mic, ~analysisSize=Slit),
    /* squareColumnLayer, */
    {...webcam, alpha: 0.0, id: Some("webcam4slitscan")},
    {...sobel("webcam4slitscan"), alpha: 0.0, id: Some("sobel4slitscan")},
    {...slitscan("sobel4slitscan"), alpha: 0.5},
    /* pitchFilter(cMajor), */
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
    analyzer(Mic),
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
    analyzer(Mic),
    squareColumnLayer,
    historyLayer,
    drawGlobal([
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

let debussyFile = analyzer(AudioFile("media/la_cathedrale_engloutie.m4a"));

let debussy = {...history, layers: [debussyFile, historyLayer, reader]};

let equationFile = analyzer(AudioFile("media/equation.ogg"));

let equation = {...history, layers: [equationFile, reader]};

let dissolve = {
  ...defaultParams,
  writePosOffset: 2,
  shouldClear: false,
  layers: [
    analyzer(Mic, ~analysisSize=Slit),
    {...drosteLayer, alpha: 0.1},
    {...reader, alpha: 0.0},
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

let midi = {
  ...history,
  layers: [fill("black"), midiKeyboard, {...reader, alpha: 0.0}],
};

let midiDroste = {...dissolve, layers: [midiKeyboard, drosteLayer, reader]};

let readFromCenterLine = {
  ...history,
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: defaultSize / 2,
  writePosOffset: defaultSize / 2,
};

let historyBackAndForth = {
  ...readFromCenterLine,
  layers: [analyzer(Mic), historyBackAndForthLayer, reader],
};

let vinyl = {
  ...readFromCenterLine,
  layers: [rotateLayer(degreesToRadians(1.0)), analyzer(Mic), reader],
};

let videoURL = "media/nonfree/kishi_bashi-say_yeah.mp4";
let video = {
  ...defaultParams,
  layers: [
    video(videoURL),
    analyzer(AudioFromVideo(videoURL)),
    /* pitchFilter(cMajor), */
    reader,
  ],
};

let lesTresRichesHeures = {
  ...defaultParams,
  outputGain: 0.1,
  readPosDelta: (-1),
  layers: [
    img("media/les_tres_riches_heures.jpg"),
    sobel("root"),
    pitchFilter(majorHexatonic),
    reader,
  ],
};

let histogramParams = {
  ...defaultParams,
  layers: [webcam, histogram, saturationReader],
};

let rawAudio = {
  ...defaultParams,
  millisPerTick: 92,
  layers: [
    webcam,
    {...fill("cyan"), compositeOperation: Multiply},
    {...rawAudioWriter, compositeOperation: Lighter},
    rawAudioReader,
  ],
};

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

let welcome = {
  ...defaultParams,
  layers: [
    text(
      "Welcome to GAYER, the Graphical Audio plaYER!\n"
      ++ "Please press the >| button at the top of the screen to begin.",
    ),
    fill("white"),
    {...drawText("GAYER", ~color="black"), alpha: 0.5},
  ],
};

let welcomeAudio = {
  ...defaultParams,
  layers: [
    fill("black"),
    drawText("GAYER", ~color="red", ~fillOrStroke=Stroke),
    saturationReader,
  ],
};

let displaceParams = {
  ...history,
  shouldClear: true,
  layers: [
    {
      ...analyzer(Mic, ~analysisSize=History({w: Width, h: Height})),
      id: Some("analyzer"),
    },
    reader,
    {...webcam, alpha: 0.0, id: Some("webcam")},
    displace("webcam", "analyzer"),
  ],
};

let rawAudioWarning = {
  ...defaultParams,
  layers: [
    text(
      "WARNING! The next mode has a high potential for audio feedback.\n"
      ++ "Please lower your volume before continuing.",
    ),
    fill("red"),
    drawText("WARNING!", ~color="black"),
  ],
};

let presetsWithoutLayerIds = [
  ("Welcome", welcome),
  ("Single note", singleNote),
  ("Spacy", {...defaultParams, layers: spacy}),
  ("Tughra of Suleiman", tughra),
  ({js|Les Très Riches Heures|js}, lesTresRichesHeures),
  ("Four Seasons", fourSeasons),
  ("Is it a crime?", isItACrime),
  ("Audio file", equation),
  ("Webcam (edge detection)", webcamEdgeDetect),
  ("Mic (CQT spectrogram)", history),
  /* ("Hand-drawn", handDrawnParams), */
  /* ("Slitscan", slitscanParams), */
  ("Histogram", histogramParams),
  ("Keycode", keycodeParams),
  ("Displace", displaceParams),
  /* ("History (-|-)", historyBackAndForth), */
  /* ("Video", video), */
  /* ("Rotation", vinyl), */
  ("Dissolve", dissolve),
  ("MIDI (requires MIDI keyboard)", midi),
  /* ("Harmony", harmonyParams), */
  /* ("King Wen", iChing), */
  /* ("Mic feedback (may be loud!)", feedback), */
  ("Raw Audio Warning", rawAudioWarning),
  ("Raw audio (can feedback!)", rawAudio),
  /* ("Raw audio and spacy", rawAudioAndSpacy), */
  ("Empty", {...defaultParams, layers: []}),
];

let addIds = List.map(maybeAddId);

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
