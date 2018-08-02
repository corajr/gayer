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
  let slitscanOptions = r => Json.Encode.(object_([("x", int(r.x))]));

  let cameraOptions = r =>
    Json.Encode.(
      switch (r.slitscan) {
      | None => object_([])
      | Some(slitscan) =>
        object_([("slitscan", slitscanOptions(slitscan))])
      }
    );
};

type layerContent =
  | Fill(string)
  | Webcam(cameraOptions)
  | Image(string)
  | Analysis
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
        json |> map(s => Webcam(s), DecodeCameraOptions.cameraOptions)
      | "image" => json |> map(s => Image(s), field("url", string))
      | "reader" =>
        json
        |> map(i => Reader(i), map(channel_of_int, field("channel", int)))
      | "analysis" => Analysis
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
      | Analysis => object_([("type", string("analysis"))])
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

let renderLayerContent = (layerContent, setRef) =>
  switch (layerContent) {
  | Webcam(_) =>
    <video ref=setRef autoPlay=true muted=true width="120" height="120" />
  | Image(url) => <img ref=setRef src=url width="120" height="120" />
  | Analysis => ReasonReact.string("analysis")
  | PitchClasses(pc) => ReasonReact.string("pc")
  | Fill(s) => ReasonReact.string("fill: " ++ s)
  | Reader(channel) => ReasonReact.string("reader")
  };

let component = ReasonReact.statelessComponent("Layer");

let make = (~layer, ~setRef=_ => (), _children) => {
  ...component,
  render: self =>
    <div>
      (renderLayerContent(layer.content, setRef))
      <div>
        (ReasonReact.string("Alpha: " ++ Js.Float.toString(layer.alpha)))
      </div>
      <div>
        (
          ReasonReact.string(
            "Composite operation: "
            ++ string_of_compositeOperation(layer.compositeOperation),
          )
        )
      </div>
    </div>,
};
