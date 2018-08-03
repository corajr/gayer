open Jest;
open Expect;
open Params;

describe("EncodeParams <=> DecodeParams", () => {
    test("decode inverts encode", () =>
      expect(DecodeParams.params(EncodeParams.params(defaultParams)))
      |> toBe(defaultParams)
    );
  }
);
