open Jest;
open Expect;
open Layer;
open LayerGenerator;
open Presets;
open CameraOptions;

let cameraOptExamples = [slitscanDefaults];

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
  testAll(
    "decode inverts encode",
    Array.to_list(Array.map(snd, allLayerTypes)),
    layer =>
    expect(DecodeLayer.layer(EncodeLayer.layer(layer))) |> toEqual(layer)
  )
);
