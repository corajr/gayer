open Audio.AudioInput;
open Canvas;
open Music;

type slitscanOptions = {x: int};

type cameraOptions = {slitscan: option(slitscanOptions)};

module DecodeCameraOptions = {
  let slitscanOptions = json => Json.Decode.{x: json |> field("x", int)};
  let cameraOptions = json =>
    Json.Decode.{
      slitscan: json |> optional(field("slitscan", slitscanOptions)),
    };
};

module EncodeCameraOptions = {
  let slitscanOptions: slitscanOptions => Js.Json.t =
    r => Json.Encode.(object_([("x", int(r.x))]));

  let cameraOptions = r =>
    Json.Encode.(
      object_([("slitscan", nullable(slitscanOptions, r.slitscan))])
    );
};

type layerContent =
  | Fill(string)
  | Webcam(cameraOptions)
  | Image(string)
  | Analysis(audioInputSetting)
  | PitchClasses(PitchSet.t)
  | Reader(channel);

type layer = {
  content: layerContent,
  alpha: float,
  compositeOperation,
};

module DecodeLayer = {
  let layerByType = (type_, json) =>
    Json.Decode.(
      switch (type_) {
      | "webcam" =>
        json
        |> map(
             s => Webcam(s),
             field("options", DecodeCameraOptions.cameraOptions),
           )
      | "image" => json |> map(s => Image(s), field("url", string))
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
      content: json |> field("content", layerContent),
      alpha: json |> field("alpha", float),
      compositeOperation:
        json
        |> map(
             compositeOperation_of_string,
             field("compositeOperation", string),
           ),
    };
};

module EncodeLayer = {
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

      | Reader(channel) =>
        object_([
          ("type", string("reader")),
          ("channel", int(int_of_channel(channel))),
        ])
      }
    );

  let layer = r =>
    Json.Encode.(
      object_([
        ("content", layerContent(r.content)),
        ("alpha", float(r.alpha)),
        (
          "compositeOperation",
          string(string_of_compositeOperation(r.compositeOperation)),
        ),
      ])
    );
};

let renderLayerContent = (layerContent, changeLayer, getAudio, setRef) =>
  <div style=(ReactDOMRe.Style.make(~display="flex", ()))>
    <div>
      (
        switch (layerContent) {
        | Webcam(_) =>
          <video
            ref=setRef
            autoPlay=true
            muted=true
            width="120"
            height="120"
          />
        | Image(url) => <img ref=setRef src=url width="120" height="120" />
        | Analysis(source) =>
          let (audioCtx, input) = getAudio(source);
          <AnalysisCanvas size=120 audioCtx input saveRef=setRef />;
        | PitchClasses(_)
        | Fill(_)
        | Reader(_) => ReasonReact.null
        }
      )
    </div>
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

let component = ReasonReact.statelessComponent("Layer");

let make = (~layer, ~changeLayer, ~setRef=_ => (), ~getAudio, _children) => {
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
          (renderLayerContent(layer.content, changeLayer, getAudio, setRef))
        </CardMedia>
        <CardContent style=(ReactDOMRe.Style.make(~height="100%", ()))>
          <div>
            <Typography> (ReasonReact.string("Alpha")) </Typography>
            <Slider
              min=0.0
              max=1.0
              step=0.1
              value=layer.alpha
              onChange=(
                (_evt, value) =>
                  changeLayer(layer, {...layer, alpha: value})
              )
            />
          </div>
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
