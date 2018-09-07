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
