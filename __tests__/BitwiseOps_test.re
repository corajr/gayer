open Jest;

open Expect;

open BitwiseOps;

let allFalse = Array.make(12, false);

describe("readAllBitsOfInt", () => {
  test("converts 0 to [|false|] * 12", () =>
    expect(readAllBitsOfInt(0)) |> toEqual(allFalse)
  );
/*
  test("converts 1 to [|false, false, ...true|]", () =>
    expect(readAllBitsOfInt(1)) |> toEqual(allFalse)
  );

  test("converts 2 to [true, false]", () =>
    expect(readAllBitsOfInt(2)) |> toEqual([true, false])
  );

  test("converts 3 to [true, true]", () =>
    expect(readAllBitsOfInt(3)) |> toEqual([true, true])
  );
  */
});
