open Audio;
open Canvas.DrawCommand;
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

let singleNote = {...defaultParams, layers: [singleNoteLayer(60), reader]};

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
  readPosDelta: 0,
  writePosDelta: 0,
  readPosOffset: defaultSize - 1,
  writePosOffset: defaultSize - 1,
  shouldClear: false,
  layers: [
    analyzer(Mic),
    /* squareColumnLayer, */
    {...webcam, alpha: 0.0},
    slitscan,
    /* sobel, */
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
    analyzer(Mic),
    /* pitchFilter(cMajor), */
    historyLayer,
    reader,
  ],
};

let slitscanHistogramParams = {
  ...slitscanParams,
  layers: [webcam, slitscan, histogram, historyLayer, reader],
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
    historyLayer,
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

let debussyFile = analyzer(AudioFile("media/la_cathedrale_engloutie.m4a"));

let debussy = {...history, layers: [debussyFile, historyLayer, reader]};

let droste = {
  ...defaultParams,
  readPosDelta: 0,
  writePosDelta: 0,
  shouldClear: false,
  layers: [
    {
      ...analyzer(Mic),
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

let midi = {
  ...history,
  layers: [fill("black"), midiKeyboard, {...reader, alpha: 0.0}],
};

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
  layers: [analyzer(Mic), historyBackAndForthLayer, reader],
};

let vinyl = {
  ...readFromCenterLine,
  layers: [rotateLayer, analyzer(Mic), reader],
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
  outputGain: 0.05,
  layers: [
    img("media/les_tres_riches_heures.jpg"),
    sobel("root"),
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
  (
    "Displace",
    {
      ...defaultParams,
      layers: [hubble, webcam, displace("root", "webcam")],
    },
  ),
  ("Keycode", keycodeParams),
  ("Welcome", {...defaultParams, layers: [fill("black"), text("GAYER")]}),
  /* ("Spectrogram", {...defaultParams, layers: [analyzer(Mic)]}), */
  ("Welcome (Audio)", welcomeAudio),
  ("Spacy", {...defaultParams, layers: spacy}),
  ("Single note", singleNote),
  /* ("Hand-drawn", handDrawnParams), */
  /* ("Webcam", webcamEdgeDetect), */
  ("Webcam (edge detection)", webcamEdgeDetect),
  ("Slitscan", slitscanParams),
  /* ("Slitscan (edge detection)", slitscanEdgeDetectParams), */
  ("Slitscan (color histogram)", slitscanHistogramParams),
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

let addIds =
  List.map(layer => {
    let nextId = idCounter^;
    idCounter := nextId + 1;
    {
      ...layer,
      id:
        Some(
          string_type_of_layerContent(layer.content)
          ++ "-"
          ++ string_of_int(nextId),
        ),
    };
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
