open Jest;
open Expect;
open Layer;
open Presets;

let slitscan = StaticX(320);

let cameraOptExamples = [{slitscan: None}, {slitscan: Some(slitscan)}];

describe("EncodeCameraOptions <=> DecodeCameraOptions", () =>
  testAll("decode inverts encode", cameraOptExamples, cameraOptions =>
    expect(
      DecodeCameraOptions.cameraOptions(
        EncodeCameraOptions.cameraOptions(cameraOptions),
      ),
    )
    |> toEqual(cameraOptions)
  )
);

describe("EncodeLayer <=> DecodeLayer", () =>
  testAll("decode inverts encode", allLayerTypes, layer =>
    expect(DecodeLayer.layer(EncodeLayer.layer(layer))) |> toEqual(layer)
  )
);
