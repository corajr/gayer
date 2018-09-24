// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Canvas$Gayer from "./Canvas.bs.js";
import * as Caml_primitive from "bs-platform/lib/es6/caml_primitive.js";

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
          /* r */Caml_array.caml_array_get(rawData, offset + Canvas$Gayer.int_of_channel(/* R */0) | 0) / 255.0,
          /* g */Caml_array.caml_array_get(rawData, offset + Canvas$Gayer.int_of_channel(/* G */1) | 0) / 255.0,
          /* b */Caml_array.caml_array_get(rawData, offset + Canvas$Gayer.int_of_channel(/* B */2) | 0) / 255.0,
          /* a */Caml_array.caml_array_get(rawData, offset + Canvas$Gayer.int_of_channel(/* A */3) | 0) / 255.0
        ];
}

function imageDataToPixels(imageData) {
  return mapRawData(imageData.data, rawDataToPixel);
}

function rawDataToFloatArray(channel, invert) {
  var channelOffset = Canvas$Gayer.int_of_channel(channel);
  return (function (rawData, offset) {
      var v = Caml_array.caml_array_get(rawData, offset + channelOffset | 0) / 255.0;
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

function imageDataToFloat32Array(imageData, channel) {
  var channelOffset = Canvas$Gayer.int_of_channel(channel);
  var rawData = imageData.data;
  var n = rawData.length / 4 | 0;
  var output = new Float32Array(n);
  for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
    var v = Caml_array.caml_array_get(rawData, (i << 2) + channelOffset | 0) / 255.0;
    Caml_array.caml_array_set(output, i, 2.0 * v - 1.0);
  }
  return output;
}

function imageDataToStereo(imageData, channelL, channelR) {
  var rawData = imageData.data;
  var n = rawData.length / 4 | 0;
  var arrayL = Caml_array.caml_make_vect(n, 0.0);
  var arrayR = Caml_array.caml_make_vect(n, 0.0);
  var offsetL = Canvas$Gayer.int_of_channel(channelL);
  var offsetR = Canvas$Gayer.int_of_channel(channelR);
  for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
    var offset = (i << 2);
    Caml_array.caml_array_set(arrayL, i, Caml_array.caml_array_get(rawData, offset + offsetL | 0) / 255.0);
    Caml_array.caml_array_set(arrayR, i, Caml_array.caml_array_get(rawData, offset + offsetR | 0) / 255.0);
  }
  return /* tuple */[
          arrayL,
          arrayR
        ];
}

function imageDataToHistogram(binCount, binFn, $staropt$star, imageData) {
  var divideBy = $staropt$star !== undefined ? $staropt$star : 1.0;
  var output = Caml_array.caml_make_vect(binCount, 0.0);
  var outputMax = /* record */[/* contents */0.0];
  $$Array.iter((function (pixel) {
          var match = Curry._1(binFn, pixel);
          var i = match[0];
          Caml_array.caml_array_set(output, i, Caml_array.caml_array_get(output, i) + match[1]);
          if (Caml_array.caml_array_get(output, i) > outputMax[0]) {
            outputMax[0] = Caml_array.caml_array_get(output, i);
            return /* () */0;
          } else {
            return 0;
          }
        }), mapRawData(imageData.data, rawDataToPixel));
  var divideByFinal = Caml_primitive.caml_float_max(outputMax[0], divideBy);
  var match = divideByFinal === 0.0;
  if (match) {
    return output;
  } else {
    return $$Array.map((function (x) {
                  return x / divideByFinal;
                }), output);
  }
}

function imageDataToFilterValues(imageData, _, _$1) {
  imageData.data;
  return /* () */0;
}

var makeUint8ClampedArray = function (len){return new Uint8ClampedArray(len)};

function makeImageData(cqtLine) {
  var len = cqtLine.length;
  var n = len / 4 | 0;
  var output = makeUint8ClampedArray(len);
  for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
    var offset = (i << 2);
    var cqtOffset = (((n - i | 0) - 1 | 0) << 2);
    Caml_array.caml_array_set(output, offset, Caml_array.caml_array_get(cqtLine, cqtOffset));
    Caml_array.caml_array_set(output, offset + 1 | 0, Caml_array.caml_array_get(cqtLine, cqtOffset + 1 | 0));
    Caml_array.caml_array_set(output, offset + 2 | 0, Caml_array.caml_array_get(cqtLine, cqtOffset + 2 | 0));
    Caml_array.caml_array_set(output, offset + 3 | 0, 255);
  }
  return new ImageData(output, 1, n);
}

function makeImageDataFromFloats(input, w, h) {
  var n = input.length;
  var len = (n << 2);
  var output = makeUint8ClampedArray(len);
  for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
    var offset = (i << 2);
    var v = Caml_array.caml_array_get(input, (n - i | 0) - 1 | 0) * 255.0 | 0;
    Caml_array.caml_array_set(output, offset + Canvas$Gayer.int_of_channel(/* R */0) | 0, v);
    Caml_array.caml_array_set(output, offset + Canvas$Gayer.int_of_channel(/* G */1) | 0, v);
    Caml_array.caml_array_set(output, offset + Canvas$Gayer.int_of_channel(/* B */2) | 0, v);
    Caml_array.caml_array_set(output, offset + Canvas$Gayer.int_of_channel(/* A */3) | 0, 255);
  }
  return new ImageData(output, w, h);
}

export {
  mapRawData ,
  mapImageData ,
  rawDataToPixel ,
  imageDataToPixels ,
  rawDataToFloatArray ,
  imageDataToFloatArray ,
  imageDataToFloat32Array ,
  imageDataToStereo ,
  imageDataToHistogram ,
  imageDataToFilterValues ,
  makeUint8ClampedArray ,
  makeImageData ,
  makeImageDataFromFloats ,
  
}
/* Canvas-Gayer Not a pure module */