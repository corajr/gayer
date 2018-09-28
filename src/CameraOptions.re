open Canvas.DrawCommand;

type cameraOptions = {
  sourceLayerKey: string,
  sourceRect: rect,
  destRect: rect,
  sourceXDelta: length,
  sourceYDelta: length,
  destXDelta: length,
  destYDelta: length,
};

let slitscanDefaults = {
  sourceLayerKey: "webcam",
  sourceRect: {
    x: Divide(Width, Constant(2)),
    y: Pixels(0),
    w: Pixels(1),
    h: Height,
  },
  destRect: {
    x: Pixels(0),
    y: Pixels(0),
    w: Pixels(1),
    h: Height,
  },
  sourceXDelta: Pixels(0),
  sourceYDelta: Pixels(0),
  destXDelta: Pixels(1),
  destYDelta: Pixels(0),
};

module DecodeCameraOptions = {
  let cameraOptions = json =>
    Json.Decode.{
      sourceLayerKey: json |> field("source", string),
      sourceRect: json |> field("sourceRect", DecodeDrawCommand.rect),
      destRect: json |> field("destRect", DecodeDrawCommand.rect),
      sourceXDelta: json |> field("sourceXDelta", DecodeDrawCommand.length),
      sourceYDelta: json |> field("sourceYDelta", DecodeDrawCommand.length),
      destXDelta: json |> field("destXDelta", DecodeDrawCommand.length),
      destYDelta: json |> field("destYDelta", DecodeDrawCommand.length),
    };
};

module EncodeCameraOptions = {
  let cameraOptions = r =>
    Json.Encode.(
      object_([
        ("source", string(r.sourceLayerKey)),
        ("sourceRect", EncodeDrawCommand.rect(r.sourceRect)),
        ("destRect", EncodeDrawCommand.rect(r.destRect)),
        ("sourceXDelta", EncodeDrawCommand.length(r.sourceXDelta)),
        ("sourceYDelta", EncodeDrawCommand.length(r.sourceYDelta)),
        ("destXDelta", EncodeDrawCommand.length(r.destXDelta)),
        ("destYDelta", EncodeDrawCommand.length(r.destYDelta)),
      ])
    );
};
