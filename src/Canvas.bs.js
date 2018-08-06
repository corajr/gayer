// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as List from "bs-platform/lib/es6/list.js";
import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Caml_int32 from "bs-platform/lib/es6/caml_int32.js";
import * as Pervasives from "bs-platform/lib/es6/pervasives.js";
import * as RList$Rationale from "rationale/src/RList.js";

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

function setFilter(ctx, filters) {
  ctx.filter = RList$Rationale.join(" ")(List.map(string_of_filter, filters));
  return /* () */0;
}

function setTransform(ctx, param) {
  ctx.setTransform(param[/* horizontalScaling */0], param[/* horizontalSkewing */1], param[/* verticalSkewing */2], param[/* verticalScaling */3], param[/* horizontalMoving */4], param[/* verticalMoving */5]);
  return /* () */0;
}

function circle(ctx, x, y, r) {
  ctx.ellipse(x, y, r, r, 0.0, 0.0, 2.0 * (Math.PI));
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
  /* setFilter */setFilter,
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

function mapRawData(rawData, f) {
  var n = rawData.length / 4 | 0;
  return $$Array.init(n, (function (i) {
                var offset = (i << 2);
                return Curry._2(f, rawData, offset);
              }));
}

function mapImageData(imageData, f) {
  return mapRawData(imageData.data, f);
}

function rawDataToPixel(rawData, offset) {
  return /* record */[
          /* r */Caml_array.caml_array_get(rawData, offset + /* R */0 | 0) / 255.0,
          /* g */Caml_array.caml_array_get(rawData, offset + /* G */1 | 0) / 255.0,
          /* b */Caml_array.caml_array_get(rawData, offset + /* B */2 | 0) / 255.0,
          /* a */Caml_array.caml_array_get(rawData, offset + /* A */3 | 0) / 255.0
        ];
}

function imageDataToPixels(imageData) {
  return mapRawData(imageData.data, rawDataToPixel);
}

function rawDataToFloatArray(channel, invert) {
  return (function (rawData, offset) {
      var v = Caml_array.caml_array_get(rawData, offset + channel | 0) / 255.0;
      if (invert) {
        return 1.0 - v;
      } else {
        return v;
      }
    });
}

function imageDataToFloatArray(imageData, channel) {
  var f = rawDataToFloatArray(channel, channel === /* A */3);
  return mapRawData(imageData.data, f);
}

var makeUint8ClampedArray = function (len){return new Uint8ClampedArray(len)};

function makeImageData(cqtLine) {
  var len = cqtLine.length;
  var n = len / 4 | 0;
  var output = makeUint8ClampedArray(len);
  for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
    var offset = (i << 2);
    var cqtVal = Caml_array.caml_array_get(cqtLine, (((n - i | 0) - 1 | 0) << 2));
    Caml_array.caml_array_set(output, offset + /* R */0 | 0, cqtVal);
    Caml_array.caml_array_set(output, offset + /* G */1 | 0, cqtVal);
    Caml_array.caml_array_set(output, offset + /* B */2 | 0, cqtVal);
    Caml_array.caml_array_set(output, offset + /* A */3 | 0, 255);
  }
  return new ImageData(output, 1, n);
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

var defaultTransform = /* record */[
  /* horizontalScaling */1.0,
  /* horizontalSkewing */0.0,
  /* verticalSkewing */0.0,
  /* verticalScaling */1.0,
  /* horizontalMoving */0.0,
  /* verticalMoving */0.0
];

export {
  int_of_channel ,
  channel_of_int ,
  string_of_compositeOperation ,
  compositeOperation_of_string ,
  defaultTransform ,
  string_of_filter ,
  Ctx ,
  simpleDrawImage ,
  mapRawData ,
  mapImageData ,
  rawDataToPixel ,
  imageDataToPixels ,
  rawDataToFloatArray ,
  imageDataToFloatArray ,
  makeUint8ClampedArray ,
  makeImageData ,
  loadImage ,
  wrapCoord ,
  
}
/* RList-Rationale Not a pure module */
