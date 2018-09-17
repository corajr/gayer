type slitscanOptions =
  | ReadPosX
  | ReadPosY
  | StaticX(int)
  | StaticY(int);

type cameraOptions = {
  sourceLayerKey: string,
  slitscan: slitscanOptions,
};

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
      sourceLayerKey: json |> field("source", string),
      slitscan: json |> field("slitscan", slitscanOptions),
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
      object_([
        ("source", string(r.sourceLayerKey)),
        ("slitscan", slitscanOptions(r.slitscan)),
      ])
    );
};
