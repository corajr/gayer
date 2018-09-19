open Audio.AudioInput;
open CameraOptions;
open Canvas;
open MIDICanvas;
open Music;
open RawAudio;
open ReaderType;

type layerContent =
  | Fill(string)
  | Draw(list(DrawCommand.command))
  | HandDrawn
  | Webcam
  | Slitscan(cameraOptions)
  | Image(string)
  | Video(string)
  | Analysis(audioInputSetting)
  | PitchClasses(PitchSet.t)
  | MIDIKeyboard
  | KeycodeReader
  | KeycodeWriter
  | RawAudioWriter(rawAudioFormat)
  | RawAudioReader(rawAudioFormat)
  | Histogram
  | Regl
  | Reader(readerType);

type layer = {
  content: layerContent,
  alpha: float,
  compositeOperation,
  rotation,
  transformMatrix,
  filters: string,
  id: option(string),
};

let defaultLayer = {
  content: Fill("black"),
  alpha: 1.0,
  compositeOperation: SourceOver,
  transformMatrix: defaultTransform,
  rotation: 0.0,
  filters: "none",
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
      | "keycode-writer" => KeycodeWriter
      | "keycode-reader" => KeycodeReader
      | "hand-drawn" => HandDrawn
      | "regl" => Regl
      | "webcam" => Webcam
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
             s => Analysis(s),
             field("source", DecodeAudioInput.audioInputSetting),
           )

      | "fill" => json |> map(s => Fill(s), field("style", string))
      | "draw" =>
        json
        |> map(
             xs => Draw(xs),
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
      alpha: json |> field("alpha", float),
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
      | Image(url) =>
        object_([("type", string("image")), ("url", string(url))])
      | Video(url) =>
        object_([("type", string("video")), ("url", string(url))])
      | Analysis(source) =>
        object_([
          ("type", string("analysis")),
          ("source", EncodeAudioInput.audioInputSetting(source)),
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
      | MIDIKeyboard => object_([("type", string("midi-keyboard"))])
      | KeycodeWriter => object_([("type", string("keycode-writer"))])
      | KeycodeReader => object_([("type", string("keycode-reader"))])
      | Histogram => object_([("type", string("histogram"))])
      | Regl => object_([("type", string("regl"))])
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
        ("alpha", float(r.alpha)),
        (
          "compositeOperation",
          string(string_of_compositeOperation(r.compositeOperation)),
        ),
        ("transformMatrix", transformMatrix(r.transformMatrix)),
        ("rotation", rotation(r.rotation)),
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

let renderLayerPreview =
    (~layer, ~changeLayer, ~setRef, ~saveTick, ~onUnmount, ~layerRefs) => {
  let layerKey = getLayerKey(layer);
  let savePreviewRef = aRef =>
    switch (Js.Nullable.toOption(aRef)) {
    | Some(previewCanvas) =>
      saveTick(onUnmount, layerKey ++ "preview", () =>
        switch (Belt.Map.String.get(layerRefs^, layerKey)) {
        | Some(layer) =>
          let ctx = getContext(getFromReact(previewCanvas));
          let src = getElementAsImageSource(layer);
          Ctx.drawImageDestRect(ctx, src, 0, 0, 120, 120);
        | _ => ()
        }
      )
    | None => ()
    };
  <div style=(ReactDOMRe.Style.make(~display="flex", ()))>
    <div> <canvas ref=savePreviewRef width="120" height="120" /> </div>
    <div>
      (
        switch (layer.content) {
        | PitchClasses(xs) =>
          <div>
            <MaterialUi.Typography>
              (ReasonReact.string("Pitch classes:"))
            </MaterialUi.Typography>
            <PitchSetSelector
              pitchSet=xs
              onChangeSetting=(
                newPitches =>
                  changeLayer(
                    layer,
                    {...layer, content: PitchClasses(newPitches)},
                  )
              )
            />
          </div>
        | Reader(readerType) =>
          <div>
            <MaterialUi.Typography>
              (ReasonReact.string("Reader:"))
            </MaterialUi.Typography>
            <ReaderType
              readerType
              onChangeSetting=(
                newReaderType =>
                  changeLayer(
                    layer,
                    {...layer, content: Reader(newReaderType)},
                  )
              )
            />
          </div>
        | _ =>
          <MaterialUi.Typography>
            (
              ReasonReact.string(
                Js.Json.stringifyWithSpace(
                  EncodeLayer.layerContent(layer.content),
                  2,
                ),
              )
            )
          </MaterialUi.Typography>
        }
      )
    </div>
  </div>;
};

let component = ReasonReact.statelessComponent("Layer");

let make =
    (
      ~layer,
      ~layerRefs,
      ~onSetRef,
      ~saveTick,
      ~changeLayer,
      ~width,
      ~height,
      _children,
    ) => {
  ...component,
  render: self =>
    MaterialUi.(
      <MaterialUi.Card
        style=(
          ReactDOMRe.Style.make(
            ~display="flex",
            ~justifyContent="space-between",
            (),
          )
        )>
        <CardMedia src="dummy">
          (
            renderLayerPreview(
              ~layer,
              ~saveTick,
              ~onUnmount=self.onUnmount,
              ~setRef=onSetRef(layer),
              ~changeLayer,
              ~layerRefs,
            )
          )
        </CardMedia>
        <CardContent style=(ReactDOMRe.Style.make(~height="100%", ()))>
          <FloatSlider
            value=layer.alpha
            label="Alpha"
            onChange=(value => changeLayer(layer, {...layer, alpha: value}))
          />
          <FloatSlider
            min=((-0.5) *. tau)
            max=(0.5 *. tau)
            value=layer.rotation
            label="Rotation"
            step=0.01
            onChange=(
              value => changeLayer(layer, {...layer, rotation: value})
            )
          />
          /* <NumericTextField */
          /*   label=(ReasonReact.string("Rotation")) */
          /*   value=(`Float(layer.rotation)) */
          /*   onChange=( */
          /*     value => changeLayer(layer, {...layer, rotation: value}) */
          /*   ) */
          /* /> */
          <div>
            <CompositeOperationSelect
              compositeOperation=layer.compositeOperation
              onChange=(
                newOperation =>
                  changeLayer(
                    layer,
                    {...layer, compositeOperation: newOperation},
                  )
              )
            />
          </div>
        </CardContent>
      </MaterialUi.Card>
    ),
};
