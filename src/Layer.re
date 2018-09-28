open AnalysisOptions;
open Audio.AudioInput;
open CameraOptions;
open Canvas;
open KeycodeUtil;
open MIDICanvas;
open Music;
open RawAudio;
open ReaderType;
open Regl;

type layerContent =
  | Fill(string)
  | Draw(list(DrawCommand.command))
  | DrawGlobal(list(DrawCommand.command))
  | Text(string)
  | HandDrawn
  | Webcam
  | Slitscan(cameraOptions)
  | Image(string)
  | Video(string)
  | Analysis(analysisOptions)
  | PitchClasses(PitchSet.t)
  | MIDIKeyboard
  | KeycodeReader(keycodeFormat)
  | KeycodeWriter(keycodeFormat)
  | RawAudioWriter(rawAudioFormat)
  | RawAudioReader(rawAudioFormat)
  | Histogram
  | Regl(reglOptions)
  | Reader(readerType);

let readable_string_type_of_layerContent =
  fun
  | Fill(_) => "Fill"
  | Draw(_) => "Draw Commands"
  | DrawGlobal(_) => "Draw Commands (global)"
  | HandDrawn => "Mouse"
  | Webcam => "Webcam"
  | Slitscan(_) => "Slitscan"
  | Text(_) => "Text"
  | Image(_) => "Image"
  | Video(_) => "Video"
  | Analysis(_) => "Analyzer"
  | PitchClasses(_) => "Pitch classes"
  | MIDIKeyboard => "MIDI Input"
  | KeycodeReader(_) => "Key Reader"
  | KeycodeWriter(_) => "Key Writer"
  | RawAudioWriter(_) => "Raw Audio Writer"
  | RawAudioReader(_) => "Raw Audio Reader"
  | Histogram => "Histogram"
  | Regl(_) => "Shader"
  | Reader(_) => "Reader";

let string_type_of_layerContent =
  fun
  | Fill(_) => "fill"
  | Draw(_) => "draw"
  | DrawGlobal(_) => "draw-global"
  | HandDrawn => "mouse"
  | Webcam => "webcam"
  | Slitscan(_) => "slitscan"
  | Text(_) => "text"
  | Image(_) => "image"
  | Video(_) => "video"
  | Analysis(_) => "analyzer"
  | PitchClasses(_) => "pitch-classes"
  | MIDIKeyboard => "midi-keyboard"
  | KeycodeReader(_) => "keycode-reader"
  | KeycodeWriter(_) => "keycode-writer"
  | RawAudioWriter(_) => "raw-audio-writer"
  | RawAudioReader(_) => "raw-audio-reader"
  | Histogram => "histogram"
  | Regl(_) => "shader"
  | Reader(_) => "reader";

let icon_of_layerContent =
  MaterialUIIcons.(
    fun
    | Fill(_) => <FormatPaint />
    | Draw(_) => <FormatListBulleted />
    | DrawGlobal(_) => <FormatListBulleted />
    | Text(_) => <TextFields />
    | HandDrawn => <Brush />
    | Webcam => <Videocam />
    | Slitscan(_) => <Flip />
    | Image(_) => <Image />
    | Video(_) => <Movie />
    | Analysis(_) => <Mic />
    | PitchClasses(_) => <Tonality />
    | MIDIKeyboard => <MusicNote />
    | KeycodeReader(_) => <Textsms />
    | KeycodeWriter(_) => <Keyboard />
    | RawAudioWriter(_) => <Voicemail />
    | RawAudioReader(_) => <Voicemail />
    | Histogram => <ShowChart />
    | Regl(_) => <Filter />
    | Reader(_) => <Speaker />
  );

type layer = {
  content: layerContent,
  enabled: bool,
  alpha: float,
  compositeOperation,
  rotation,
  transformMatrix,
  filters: string,
  tickPeriod: int,
  tickPhase: int,
  id: option(string),
};

let defaultLayer = {
  content: Fill("black"),
  enabled: true,
  alpha: 1.0,
  compositeOperation: SourceOver,
  transformMatrix: defaultTransform,
  rotation: 0.0,
  filters: "none",
  tickPeriod: 1,
  tickPhase: 0,
  id: None,
};

let oneCompleteTurnAfterNTicks: int => rotation = n => tau /. float_of_int(n);

module DecodeLayer = {
  let transformMatrix = json =>
    Json.Decode.(
      switch (json |> list(float)) {
      | [a, b, c, d, e, f] => {
          horizontalScaling: a,
          horizontalSkewing: b,
          verticalSkewing: c,
          verticalScaling: d,
          horizontalMoving: e,
          verticalMoving: f,
        }
      | _ => defaultTransform
      }
    );

  let rotation = json => Json.Decode.(json |> float);

  let rawAudioEncodingByType = (type_, json) =>
    Json.Decode.(
      switch (type_) {
      | "float" => Float
      | "int8" =>
        json |> map(c => Int8(channel_of_int(c)), field("channel", int))
      | _ => Int8(R)
      }
    );

  let rawAudioEncoding = json =>
    Json.Decode.(
      json |> (field("type", string) |> andThen(rawAudioEncodingByType))
    );

  let rawAudioFormat = json =>
    Json.Decode.{
      x: json |> field("x", int),
      y: json |> field("y", int),
      w: json |> field("w", int),
      h: json |> field("h", int),
      encoding: json |> field("encoding", rawAudioEncoding),
      sampleRate: json |> field("sampleRate", int),
    };

  let layerByType = (type_, json) =>
    Json.Decode.(
      switch (type_) {
      | "midi-keyboard" => MIDIKeyboard
      | "hand-drawn" => HandDrawn
      | "webcam" => Webcam
      | "keycode-writer" =>
        json
        |> map(
             o => KeycodeWriter(o),
             field("fmt", DecodeKeycodeFormat.keycodeFormat),
           )
      | "keycode-reader" =>
        json
        |> map(
             o => KeycodeReader(o),
             field("fmt", DecodeKeycodeFormat.keycodeFormat),
           )
      | "text" => json |> map(t => Text(t), field("text", string))

      | "regl" =>
        json
        |> map(
             o => Regl(o),
             field("options", DecodeReglOptions.reglOptions),
           )

      | "slitscan" =>
        json
        |> map(
             s => Slitscan(s),
             field("options", DecodeCameraOptions.cameraOptions),
           )
      | "image" => json |> map(s => Image(s), field("url", string))
      | "video" => json |> map(s => Video(s), field("url", string))
      | "raw-audio-writer" =>
        json |> map(o => RawAudioWriter(o), field("format", rawAudioFormat))
      | "raw-audio-reader" =>
        json |> map(o => RawAudioReader(o), field("format", rawAudioFormat))
      | "histogram" => Histogram
      | "reader" =>
        json
        |> map(
             t => Reader(t),
             field("readerType", DecodeReaderType.readerType),
           )
      | "analysis" =>
        json
        |> map(
             o => Analysis(o),
             field("opts", DecodeAnalysisOptions.analysisOptions),
           )

      | "fill" => json |> map(s => Fill(s), field("style", string))
      | "draw" =>
        json
        |> map(
             xs => Draw(xs),
             field("cmds", list(DrawCommand.DecodeDrawCommand.command)),
           )
      | "draw-global" =>
        json
        |> map(
             xs => DrawGlobal(xs),
             field("cmds", list(DrawCommand.DecodeDrawCommand.command)),
           )
      | "pitchClasses" =>
        json
        |> map(
             xs => PitchClasses(PitchSet.of_list(xs)),
             field("pc", list(int)),
           )
      | _ =>
        raise @@
        DecodeError(
          "Expected layer content, got " ++ Js.Json.stringify(json),
        )
      }
    );

  let layerContent = json =>
    Json.Decode.(json |> (field("type", string) |> andThen(layerByType)));

  let layer = json =>
    Json.Decode.{
      id: json |> field("id", optional(string)),
      content: json |> field("content", layerContent),
      enabled: json |> field("enabled", bool),
      alpha: json |> field("alpha", float),
      tickPeriod: json |> field("tickPeriod", int),
      tickPhase: json |> field("tickPhase", int),
      compositeOperation:
        json
        |> map(
             compositeOperation_of_string,
             field("compositeOperation", string),
           ),
      transformMatrix: json |> field("transformMatrix", transformMatrix),
      filters: json |> field("filters", string),
      rotation: json |> field("rotation", rotation),
    };
};

module EncodeLayer = {
  let transformMatrix =
      (
        {
          horizontalScaling,
          horizontalSkewing,
          verticalSkewing,
          verticalScaling,
          horizontalMoving,
          verticalMoving,
        },
      ) =>
    Json.Encode.(
      list(
        float,
        [
          horizontalScaling,
          horizontalSkewing,
          verticalSkewing,
          verticalScaling,
          horizontalMoving,
          verticalMoving,
        ],
      )
    );

  let rawAudioEncoding =
    Json.Encode.(
      fun
      | Float => object_([("type", string("float"))])
      | Int8(channel) =>
        object_([
          ("type", string("int8")),
          ("channel", int(int_of_channel(channel))),
        ])
    );
  let rawAudioFormat = r =>
    Json.Encode.(
      object_([
        ("x", int(r.x)),
        ("y", int(r.y)),
        ("w", int(r.w)),
        ("h", int(r.h)),
        ("encoding", rawAudioEncoding(r.encoding)),
        ("sampleRate", int(r.sampleRate)),
      ])
    );

  let layerContent = r =>
    Json.Encode.(
      switch (r) {
      | Webcam => object_([("type", string("webcam"))])
      | Slitscan(s) =>
        object_([
          ("type", string("slitscan")),
          ("options", EncodeCameraOptions.cameraOptions(s)),
        ])
      | Text(s) =>
        object_([("type", string("text")), ("text", string(s))])
      | Image(url) =>
        object_([("type", string("image")), ("url", string(url))])
      | Video(url) =>
        object_([("type", string("video")), ("url", string(url))])
      | Analysis(opts) =>
        object_([
          ("type", string("analysis")),
          ("opts", EncodeAnalysisOptions.analysisOptions(opts)),
        ])

      | PitchClasses(classes) =>
        object_([
          ("type", string("pitchClasses")),
          ("pc", list(int, PitchSet.elements(classes))),
        ])
      | Fill(style) =>
        object_([("type", string("fill")), ("style", string(style))])

      | Draw(cmds) =>
        object_([
          ("type", string("draw")),
          ("cmds", list(DrawCommand.EncodeDrawCommand.command, cmds)),
        ])
      | DrawGlobal(cmds) =>
        object_([
          ("type", string("draw-global")),
          ("cmds", list(DrawCommand.EncodeDrawCommand.command, cmds)),
        ])
      | MIDIKeyboard => object_([("type", string("midi-keyboard"))])
      | KeycodeWriter(fmt) =>
        object_([
          ("type", string("keycode-writer")),
          ("fmt", EncodeKeycodeFormat.keycodeFormat(fmt)),
        ])
      | KeycodeReader(fmt) =>
        object_([
          ("type", string("keycode-reader")),
          ("fmt", EncodeKeycodeFormat.keycodeFormat(fmt)),
        ])
      | Histogram => object_([("type", string("histogram"))])
      | Regl(opts) =>
        object_([
          ("type", string("regl")),
          ("options", EncodeReglOptions.reglOptions(opts)),
        ])
      | RawAudioWriter(fmt) =>
        object_([
          ("type", string("raw-audio-writer")),
          ("format", rawAudioFormat(fmt)),
        ])

      | RawAudioReader(fmt) =>
        object_([
          ("type", string("raw-audio-reader")),
          ("format", rawAudioFormat(fmt)),
        ])
      | HandDrawn => object_([("type", string("hand-drawn"))])
      | Reader(t) =>
        object_([
          ("type", string("reader")),
          ("readerType", EncodeReaderType.readerType(t)),
        ])
      }
    );

  let rotation = Json.Encode.float;

  let layer = r =>
    Json.Encode.(
      object_([
        ("id", nullable(string, r.id)),
        ("content", layerContent(r.content)),
        ("enabled", bool(r.enabled)),
        ("alpha", float(r.alpha)),
        (
          "compositeOperation",
          string(string_of_compositeOperation(r.compositeOperation)),
        ),
        ("transformMatrix", transformMatrix(r.transformMatrix)),
        ("rotation", rotation(r.rotation)),
        ("filters", string(r.filters)),
        ("tickPeriod", int(r.tickPeriod)),
        ("tickPhase", int(r.tickPhase)),
        ("filters", string(r.filters)),
      ])
    );
};

let getLayerKey: layer => string =
  layer =>
    Belt.Option.getWithDefault(
      layer.id,
      Js.Json.stringify(EncodeLayer.layerContent(layer.content)),
    );
