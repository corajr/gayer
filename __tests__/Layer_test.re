open Jest;
open Expect;
open Layer;
open Presets;

describe("EncodeLayer <=> DecodeLayer", () =>
  testAll("decode inverts encode", allLayerTypes, layer =>
    expect(DecodeLayer.layer(EncodeLayer.layer(layer))) |> toEqual(layer)
  )
);
