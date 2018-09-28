// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Color$Gayer from "./Color.bs.js";
import * as HsluvJs from "hsluv/hsluv.js";

var grayscale = $$Array.init(256, (function (i) {
        return /* tuple */[
                i,
                i,
                i,
                255
              ];
      }));

var alpha = $$Array.init(256, (function (i) {
        return /* tuple */[
                255,
                255,
                255,
                255 - i | 0
              ];
      }));

var red = $$Array.init(256, (function (i) {
        return /* tuple */[
                i,
                0,
                0,
                255
              ];
      }));

var green = $$Array.init(256, (function (i) {
        return /* tuple */[
                0,
                i,
                0,
                255
              ];
      }));

var blue = $$Array.init(256, (function (i) {
        return /* tuple */[
                0,
                0,
                i,
                255
              ];
      }));

var rainbow = $$Array.init(256, (function (i) {
        var h = i / 255.0 * 360.0;
        var match = HsluvJs.hsluvToRgb(/* tuple */[
              h,
              100.0,
              50.0
            ]);
        return /* tuple */[
                match[0] * 255.0 | 0,
                match[1] * 255.0 | 0,
                match[2] * 255.0 | 0,
                255
              ];
      }));

var lightnessRainbow = $$Array.init(256, (function (i) {
        var h = i % 16 / 16.0;
        var l = i / 255.0;
        var match = Color$Gayer.hslToRgb(h, 1.0, l);
        return /* tuple */[
                match[0],
                match[1],
                match[2],
                255
              ];
      }));

var saturationRainbowOld = $$Array.init(256, (function (i) {
        var h = i % 16 / 16.0;
        var s = i / 255.0;
        var match = Color$Gayer.hslToRgb(h, s, 0.5);
        return /* tuple */[
                match[0],
                match[1],
                match[2],
                255
              ];
      }));

var saturationRainbow = $$Array.init(256, (function (i) {
        var h = i % 16 / 16.0 * 360.0;
        var s = i / 2.5;
        var match = HsluvJs.hsluvToRgb(/* tuple */[
              h,
              s,
              50.0
            ]);
        return /* tuple */[
                match[0] * 255.0 | 0,
                match[1] * 255.0 | 0,
                match[2] * 255.0 | 0,
                255
              ];
      }));

export {
  grayscale ,
  alpha ,
  red ,
  green ,
  blue ,
  rainbow ,
  lightnessRainbow ,
  saturationRainbowOld ,
  saturationRainbow ,
  
}
/* grayscale Not a pure module */