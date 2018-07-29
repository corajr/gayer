open Canvas;
open Music;

type layerContent =
  | Webcam
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
      | "webcam" => Webcam
      | "image" => json |> map(s => Image(s), field("url", string))
      | "reader" =>
        json
        |> map(i => Reader(i), map(channel_of_int, field("channel", int)))
      | "analysis" => Analysis
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
      | Webcam => object_([("type", string("webcam"))])
      | Image(url) =>
        object_([("type", string("image")), ("url", string(url))])
      | Analysis => object_([("type", string("analysis"))])
      | PitchClasses(classes) =>
        object_([
          ("type", string("pitchClasses")),
          ("pc", list(int, PitchSet.elements(classes))),
        ])
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
