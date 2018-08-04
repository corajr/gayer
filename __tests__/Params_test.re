open Jest;
open Expect;
open Params;
open Presets;

describe("EncodeParams <=> DecodeParams", () =>
  testAll("decode inverts encode", List.map(snd, presets), preset =>
    expect(DecodeParams.params(EncodeParams.params(preset)))
    |> toEqual(preset)
  )
);
