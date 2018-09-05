open Audio.AudioInput;
open Canvas;
open MIDICanvas;
open Music;

type slitscanOptions =
  | ReadPosX
  | ReadPosY
  | StaticX(int)
  | StaticY(int);

type cameraOptions = {slitscan: option(slitscanOptions)};

module DecodeCameraOptions = {
  let slitscanOptions = json =>
    Json.Decode.(
      json
      |> (
        field("type", string)
        |> andThen((type_, json) =>
             switch (type_) {
             | "readPosX" => ReadPosX
             | "readPosY" => ReadPosY
             | "staticX" => json |> map(i => StaticX(i), field("x", int))
             | "staticY" => json |> map(i => StaticY(i), field("y", int))
             | _ => StaticX(320)
             }
           )
      )
    );

  let cameraOptions = json =>
    Json.Decode.{
      slitscan: json |> optional(field("slitscan", slitscanOptions)),
    };
};

module EncodeCameraOptions = {
  let slitscanOptions: slitscanOptions => Js.Json.t =
    Json.Encode.(
      fun
      | ReadPosX => object_([("type", string("readPosX"))])
      | ReadPosY => object_([("type", string("readPosY"))])
      | StaticX(i) =>
        object_([("type", string("staticX")), ("x", int(i))])
      | StaticY(i) =>
        object_([("type", string("staticY")), ("y", int(i))])
    );

  let cameraOptions = r =>
    Json.Encode.(
      object_([("slitscan", nullable(slitscanOptions, r.slitscan))])
    );
};

type layerContent =
  | Fill(string)
  | Draw(list(DrawCommand.command))
  | HandDrawn
  | Webcam(cameraOptions)
  | Image(string)
  | Video(string)
  | Analysis(audioInputSetting)
  | PitchClasses(PitchSet.t)
  | MIDIKeyboard
  | HistogramReader
  | Reader(channel);

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

  let layerByType = (type_, json) =>
    Json.Decode.(
      switch (type_) {
      | "midi-keyboard" => MIDIKeyboard
      | "hand-drawn" => HandDrawn
      | "webcam" =>
        json
        |> map(
             s => Webcam(s),
             field("options", DecodeCameraOptions.cameraOptions),
           )
      | "image" => json |> map(s => Image(s), field("url", string))
      | "video" => json |> map(s => Video(s), field("url", string))
      | "histogram-reader" => HistogramReader
      | "reader" =>
        json
        |> map(i => Reader(i), map(channel_of_int, field("channel", int)))
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

  let layerContent = r =>
    Json.Encode.(
      switch (r) {
      | Webcam(s) =>
        object_([
          ("type", string("webcam")),
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
      | HistogramReader => object_([("type", string("histogram-reader"))])
      | HandDrawn => object_([("type", string("hand-drawn"))])
      | Reader(channel) =>
        object_([
          ("type", string("reader")),
          ("channel", int(int_of_channel(channel))),
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

let renderLayerContent = (~layerContent, ~setRef, ~saveTick, ~layerRefs) => {
  let layerKey = Js.Json.stringify(EncodeLayer.layerContent(layerContent));

  let savePreviewRef = aRef =>
    switch (Js.Nullable.toOption(aRef)) {
    | Some(previewCanvas) =>
      saveTick(layerKey ++ "preview", () =>
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
      <MaterialUi.Typography>
        (
          ReasonReact.string(
            Js.Json.stringifyWithSpace(
              EncodeLayer.layerContent(layerContent),
              2,
            ),
          )
        )
      </MaterialUi.Typography>
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
            renderLayerContent(
              ~layerContent=layer.content,
              ~saveTick,
              ~setRef=onSetRef(layer),
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
