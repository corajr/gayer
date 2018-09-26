open Color;

type uint8 = int;
type color = (uint8, uint8, uint8, uint8);
type t = array(color);

let grayscale = Array.init(256, i => (i, i, i, 0xff));

let alpha = Array.init(256, i => (0xff, 0xff, 0xff, i));

let red = Array.init(256, i => (i, 0, 0, 0xff));
let green = Array.init(256, i => (0, i, 0, 0xff));
let blue = Array.init(256, i => (0, 0, i, 0xff));

let lightnessRainbow =
  Array.init(
    256,
    i => {
      let h = float_of_int(i mod 16) /. 16.0;
      let s = 1.0;
      let l = float_of_int(i) /. 255.0;
      let (r, g, b) = hslToRgb(h, s, l);
      (r, g, b, 0xff);
    },
  );

let saturationRainbow =
  Array.init(
    256,
    i => {
      let h = float_of_int(i mod 16) /. 16.0;
      let s = float_of_int(i) /. 255.0;
      let l = 0.5;
      let (r, g, b) = hslToRgb(h, s, l);
      (r, g, b, 0xff);
    },
  );
