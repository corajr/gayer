// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as Pervasives from "bs-platform/lib/es6/pervasives.js";
import * as Caml_primitive from "bs-platform/lib/es6/caml_primitive.js";
import * as RList$Rationale from "rationale/src/RList.js";

function string_of_canvasPatternRepeat(param) {
  switch (param) {
    case 0 : 
        return "repeat";
    case 1 : 
        return "repeat-x";
    case 2 : 
        return "repeat-y";
    case 3 : 
        return "no-repeat";
    
  }
}

function int_of_channel(channel) {
  return channel;
}

function channel_of_int($$int) {
  if ($$int > 3 || $$int < 0) {
    return /* R */0;
  } else {
    return $$int;
  }
}

function string_of_channel(param) {
  switch (param) {
    case 0 : 
        return "R";
    case 1 : 
        return "G";
    case 2 : 
        return "B";
    case 3 : 
        return "A";
    
  }
}

var tau = Math.PI * 2.0;

var one_over_tau = 1.0 / tau;

function degreesToRadians(degrees) {
  return degrees * (tau / 360.0);
}

function radiansToDegrees(radians) {
  return radians * (360.0 * one_over_tau);
}

function string_of_compositeOperation(param) {
  switch (param) {
    case 0 : 
        return "source-over";
    case 1 : 
        return "source-in";
    case 2 : 
        return "source-out";
    case 3 : 
        return "source-atop";
    case 4 : 
        return "destination-over";
    case 5 : 
        return "destination-in";
    case 6 : 
        return "destination-out";
    case 7 : 
        return "destination-atop";
    case 8 : 
        return "lighter";
    case 9 : 
        return "copy";
    case 10 : 
        return "xor";
    case 11 : 
        return "multiply";
    case 12 : 
        return "screen";
    case 13 : 
        return "overlay";
    case 14 : 
        return "darken";
    case 15 : 
        return "lighten";
    case 16 : 
        return "color-dodge";
    case 17 : 
        return "color-burn";
    case 18 : 
        return "hard-light";
    case 19 : 
        return "soft-light";
    case 20 : 
        return "difference";
    case 21 : 
        return "exclusion";
    case 22 : 
        return "hue";
    case 23 : 
        return "saturation";
    case 24 : 
        return "color";
    case 25 : 
        return "luminosity";
    
  }
}

function compositeOperation_of_string(param) {
  switch (param) {
    case "color" : 
        return /* Color */24;
    case "color-burn" : 
        return /* ColorBurn */17;
    case "color-dodge" : 
        return /* ColorDodge */16;
    case "copy" : 
        return /* Copy */9;
    case "darken" : 
        return /* Darken */14;
    case "destination-atop" : 
        return /* DestinationAtop */7;
    case "destination-in" : 
        return /* DestinationIn */5;
    case "destination-out" : 
        return /* DestinationOut */6;
    case "destination-over" : 
        return /* DestinationOver */4;
    case "difference" : 
        return /* Difference */20;
    case "exclusion" : 
        return /* Exclusion */21;
    case "hard-light" : 
        return /* HardLight */18;
    case "hue" : 
        return /* Hue */22;
    case "lighten" : 
        return /* Lighten */15;
    case "lighter" : 
        return /* Lighter */8;
    case "luminosity" : 
        return /* Luminosity */25;
    case "multiply" : 
        return /* Multiply */11;
    case "overlay" : 
        return /* Overlay */13;
    case "saturation" : 
        return /* Saturation */23;
    case "screen" : 
        return /* Screen */12;
    case "soft-light" : 
        return /* SoftLight */19;
    case "source-atop" : 
        return /* SourceAtop */3;
    case "source-in" : 
        return /* SourceIn */1;
    case "source-out" : 
        return /* SourceOut */2;
    case "source-over" : 
        return /* SourceOver */0;
    case "xor" : 
        return /* Xor */10;
    default:
      return /* SourceOver */0;
  }
}

function rgba(r, g, b, a) {
  return "rgba(" + (r.toString() + ("," + (g.toString() + ("," + (b.toString() + ("," + (a.toString() + ")")))))));
}

function string_of_filter(param) {
  if (typeof param === "number") {
    return "none";
  } else {
    switch (param.tag | 0) {
      case 0 : 
          return "url(" + (param[0] + ")");
      case 1 : 
          return "blur(" + (param[0] + ")");
      case 2 : 
          return "brightness(" + (param[0].toString() + "%)");
      case 3 : 
          return "contrast(" + (param[0].toString() + "%)");
      case 4 : 
          return "drop-shadow(" + (RList$Rationale.join(" ")(/* :: */[
                        param[0],
                        /* :: */[
                          param[1],
                          /* :: */[
                            param[2],
                            /* :: */[
                              param[3],
                              /* [] */0
                            ]
                          ]
                        ]
                      ]) + ")");
      case 5 : 
          return "grayscale(" + (param[0].toString() + "%)");
      case 6 : 
          return "hue-rotate(" + (param[0] + ")");
      case 7 : 
          return "invert(" + (param[0].toString() + "%)");
      case 8 : 
          return "opacity(" + (param[0].toString() + "%)");
      case 9 : 
          return "saturate(" + (param[0].toString() + "%)");
      case 10 : 
          return "sepia(" + (param[0].toString() + "%)");
      
    }
  }
}

function setGlobalCompositeOperation(ctx, compositeOperation) {
  ctx.globalCompositeOperation = string_of_compositeOperation(compositeOperation);
  return /* () */0;
}

function createPattern(ctx, src, repeat) {
  return ctx.createPattern(src, string_of_canvasPatternRepeat(repeat));
}

function setFilter(ctx, filters) {
  ctx.filter = RList$Rationale.join(" ")(List.map(string_of_filter, filters));
  return /* () */0;
}

function transform(ctx, param) {
  ctx.transform(param[/* horizontalScaling */0], param[/* horizontalSkewing */1], param[/* verticalSkewing */2], param[/* verticalScaling */3], param[/* horizontalMoving */4], param[/* verticalMoving */5]);
  return /* () */0;
}

function setTransform(ctx, param) {
  ctx.setTransform(param[/* horizontalScaling */0], param[/* horizontalSkewing */1], param[/* verticalSkewing */2], param[/* verticalScaling */3], param[/* horizontalMoving */4], param[/* verticalMoving */5]);
  return /* () */0;
}

function circle(ctx, x, y, r) {
  ctx.ellipse(x, y, r, r, 0.0, 0.0, tau);
  return /* () */0;
}

function moveToPos(ctx, param) {
  ctx.moveTo(param[0], param[1]);
  return /* () */0;
}

function lineToPos(ctx, param) {
  ctx.lineTo(param[0], param[1]);
  return /* () */0;
}

function line(ctx, param, param$1) {
  ctx.beginPath();
  ctx.moveTo(param[0], param[1]);
  ctx.lineTo(param$1[0], param$1[1]);
  ctx.stroke();
  return /* () */0;
}

var Ctx = /* module */[
  /* setGlobalCompositeOperation */setGlobalCompositeOperation,
  /* createPattern */createPattern,
  /* setFilter */setFilter,
  /* transform */transform,
  /* setTransform */setTransform,
  /* circle */circle,
  /* moveToPos */moveToPos,
  /* lineToPos */lineToPos,
  /* line */line
];

function simpleDrawImage(ctx, image, compositeOperation, alphaValue) {
  setGlobalCompositeOperation(ctx, compositeOperation);
  ctx.globalAlpha = alphaValue;
  ctx.drawImage(image, 0, 0);
  return /* () */0;
}

var loadImage = function (src,onLoad){
     var img = new Image;

     img.crossOrigin = "Anonymous";

     img.onload = function() {
       onLoad(img);
     }

     img.src = src;
     // make sure the load event fires for cached images too
     if ( img.complete || img.complete === undefined ) {
     img.src = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw==";
     img.src = src;
}
     };

function clamp($staropt$star, $staropt$star$1, i) {
  var minVal = $staropt$star !== undefined ? $staropt$star : 0;
  var maxVal = $staropt$star$1 !== undefined ? $staropt$star$1 : 255;
  return Caml_primitive.caml_int_min(maxVal, minVal > i ? minVal : i);
}

function wrapCoord(index, delta, size) {
  var newCoord = index + delta | 0;
  if (newCoord >= 0 && newCoord < size) {
    return newCoord;
  } else if (newCoord >= 0) {
    return Caml_int32.mod_(newCoord, size);
  } else {
    return size - Pervasives.abs(Caml_int32.mod_(newCoord, size)) | 0;
  }
}

function binsPerSemitone(height) {
  return height / 120 | 0;
}

var defaultSize = 240;

var defaultTransform = /* record */[
  /* horizontalScaling */1.0,
  /* horizontalSkewing */0.0,
  /* verticalSkewing */0.0,
  /* verticalScaling */1.0,
  /* horizontalMoving */0.0,
  /* verticalMoving */0.0
];

export {
  string_of_canvasPatternRepeat ,
  defaultSize ,
  int_of_channel ,
  channel_of_int ,
  string_of_channel ,
  tau ,
  one_over_tau ,
  degreesToRadians ,
  radiansToDegrees ,
  string_of_compositeOperation ,
  compositeOperation_of_string ,
  defaultTransform ,
  rgba ,
  string_of_filter ,
  Ctx ,
  simpleDrawImage ,
  loadImage ,
  clamp ,
  wrapCoord ,
  binsPerSemitone ,
  
}
/* tau Not a pure module */
