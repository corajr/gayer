type hsvFloat = (float, float, float);
type hslFloat = (float, float, float);

/* Adapted from MIT licensed  TinyColor v1.4.1 */
/* https://github.com/bgrins/TinyColor/blob/96592a5cacdbf4d4d16cd7d39d4d6dd28da9bd5f/tinycolor.js#L446 */

let rgbToHsvFloat: (float, float, float) => hsvFloat = [%bs.raw
  (r, g, b) => {|
  var max = Math.max(r, g, b), min = Math.min(r, g, b);
  var h, s, v = max;

  var d = max - min;
  s = max === 0 ? 0 : d / max;

  if(max == min) {
  h = 0; // achromatic
}
    else {
  switch(max) {
    case r: h = (g - b) / d + (g < b ? 6 : 0); break;
    case g: h = (b - r) / d + 2; break;
    case b: h = (r - g) / d + 4; break;
  }
    h /= 6;
}
    return [h, s, v];
     |}
];
let rgbToHslFloat: (float, float, float) => hslFloat = [%bs.raw
  (r, g, b) => {|

     var max = Math.max(r, g, b), min = Math.min(r, g, b);
     var h, s, l = (max + min) / 2;

     if(max == min) {
     h = s = 0; // achromatic
     }
     else {
     var d = max - min;
     s = l > 0.5 ? d / (2 - max - min) : d / (max + min);
     switch(max) {
     case r: h = (g - b) / d + (g < b ? 6 : 0); break;
     case g: h = (b - r) / d + 2; break;
     case b: h = (r - g) / d + 4; break;
     }

     h /= 6;
     }

     return [h, s, l];
|}
];

let hslToRgb = (h: float, s: float, l: float) : (int, int, int) => [%bs.raw
  (h, s, l) => {|
     var r, g, b;

     function hue2rgb(p, q, t) {
     if(t < 0) t += 1;
     if(t > 1) t -= 1;
     if(t < 1/6) return p + (q - p) * 6 * t;
     if(t < 1/2) return q;
     if(t < 2/3) return p + (q - p) * (2/3 - t) * 6;
     return p;
     }

     if(s === 0) {
     r = g = b = l; // achromatic
     }
     else {
     var q = l < 0.5 ? l * (1 + s) : l + s - l * s;
     var p = 2 * l - q;
     r = hue2rgb(p, q, h + 1/3);
     g = hue2rgb(p, q, h);
     b = hue2rgb(p, q, h - 1/3);
     }

     return [r * 255, g * 255, b * 255];
|}
];

/* Color strings for Ctx.setFillStyle */
let hsl = (~a: float=1.0, h: float, s: float, l: float) : string =>
  "hsl("
  ++ Js.Float.toString(h)
  ++ "turn,"
  ++ Js.Float.toString(s *. 100.0)
  ++ "%,"
  ++ Js.Float.toString(l *. 100.0)
  ++ "%,"
  ++ Js.Float.toString(a)
  ++ ")";

let gray = (l: float) : string => {
  let color = Js.Int.toStringWithRadix(int_of_float(255.0 *. l), 16);

  "#" ++ color ++ color ++ color;
};
