open Jest;

open Expect;

open Color;

describe("rgbToHsvFloat", () => {
  test("converts red to hue == 0", () =>
    expect(rgbToHsvFloat(1.0, 0.0, 0.0)) |> toEqual((0.0, 1.0, 1.0))
  );

  test("converts green to hue == 1/3", () =>
    expect(rgbToHsvFloat(0.0, 1.0, 0.0)) |> toEqual((1.0 /. 3.0, 1.0, 1.0))
  );

  test("converts blue to hue == 2/3", () =>
    expect(rgbToHsvFloat(0.0, 0.0, 1.0)) |> toEqual((2.0 /. 3.0, 1.0, 1.0))
  );
});
