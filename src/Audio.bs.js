// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE

import * as $$Array from "bs-platform/lib/es6/array.js";
import * as Block from "bs-platform/lib/es6/block.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as Caml_array from "bs-platform/lib/es6/caml_array.js";
import * as Json_decode from "@glennsl/bs-json/src/Json_decode.bs.js";
import * as Json_encode from "@glennsl/bs-json/src/Json_encode.bs.js";
import * as Js_primitive from "bs-platform/lib/es6/js_primitive.js";
import * as UserMedia$Gayer from "./UserMedia.bs.js";

var makeDefaultAudioCtx = function (){return new (window.AudioContext || window.webkitAudioContext)()};

function string_of_oscillatorType(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          return "sine";
      case 1 : 
          return "square";
      case 2 : 
          return "sawtooth";
      case 3 : 
          return "triangle";
      
    }
  } else {
    return "custom";
  }
}

function noteToFrequency(note) {
  return 440.0 * Math.pow(2.0, (note - 69.0) / 12.0);
}

function yToFrequency(binsPerSemitone, offset, height) {
  var fBinsPerSemitone = binsPerSemitone;
  var offset$1 = offset;
  return (function (y) {
      return noteToFrequency((height - y | 0) / fBinsPerSemitone + offset$1);
    });
}

function qForBinsPerOctave(binsPerOctave) {
  return 1.0 / (Math.pow(2.0, 1.0 / binsPerOctave) - 1.0);
}

var defaultQ = qForBinsPerOctave(48);

var defaultCompressorValues = /* record */[
  /* threshold */-12.0,
  /* knee */0.0,
  /* ratio */20.0,
  /* attack */0.01,
  /* release */0.05
];

function makeCompressor(audioCtx, $staropt$star) {
  var paramValues = $staropt$star !== undefined ? $staropt$star : defaultCompressorValues;
  var compressor = audioCtx.createDynamicsCompressor();
  var t = audioCtx.currentTime;
  compressor.threshold.setValueAtTime(paramValues[/* threshold */0], t);
  compressor.knee.setValueAtTime(paramValues[/* knee */1], t);
  compressor.attack.setValueAtTime(paramValues[/* attack */3], t);
  compressor.release.setValueAtTime(paramValues[/* release */4], t);
  return compressor;
}

var pinkNoiseFull = function (audioCtx){
     var bufferSize = 4096;
     return (function() {
     var b0, b1, b2, b3, b4, b5, b6;
     b0 = b1 = b2 = b3 = b4 = b5 = b6 = 0.0;
     var node = audioCtx.createScriptProcessor(bufferSize, 1, 1);
     node.onaudioprocess = function(e) {
     var output = e.outputBuffer.getChannelData(0);
     for (var i = 0; i < bufferSize; i++) {
     var white = Math.random() * 2 - 1;
     b0 = 0.99886 * b0 + white * 0.0555179;
     b1 = 0.99332 * b1 + white * 0.0750759;
     b2 = 0.96900 * b2 + white * 0.1538520;
     b3 = 0.86650 * b3 + white * 0.3104856;
     b4 = 0.55000 * b4 + white * 0.5329522;
     b5 = -0.7616 * b5 - white * 0.0168980;
     output[i] = b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362;
     output[i] *= 0.11; // (roughly) compensate for gain
     b6 = white * 0.115926;
     }
     }
     return node;
     })();
     };

var cheaperPinkNoise = function (audioCtx){
     var bufferSize = 4096;
     return (function() {
     var b0, b1, b2;
     b0 = b1 = b2 = 0.0;
     var node = audioCtx.createScriptProcessor(bufferSize, 1, 1);
     node.onaudioprocess = function(e) {
     var output = e.outputBuffer.getChannelData(0);
     for (var i = 0; i < bufferSize; i++) {
     var white = Math.random() * 2 - 1;
     b0 = 0.99765 * b0 + white * 0.0990460;
     b1 = 0.96300 * b1 + white * 0.2965164;
     b2 = 0.57000 * b2 + white * 1.0526913;
     output[i] = b0 + b1 + b2 + white * 0.1848;
     }
     }
     return node;
     })();
     };

function makeAnalyser(audioContext, $staropt$star, $staropt$star$1, $staropt$star$2, $staropt$star$3, _) {
  var fftSize = $staropt$star !== undefined ? $staropt$star : 2048;
  var minDecibels = $staropt$star$1 !== undefined ? $staropt$star$1 : -100.0;
  var maxDecibels = $staropt$star$2 !== undefined ? $staropt$star$2 : -30.0;
  var smoothingTimeConstant = $staropt$star$3 !== undefined ? $staropt$star$3 : 0.8;
  var analyser = audioContext.createAnalyser();
  analyser.fftSize = fftSize;
  analyser.minDecibels = minDecibels;
  analyser.maxDecibels = maxDecibels;
  analyser.smoothingTimeConstant = smoothingTimeConstant;
  return analyser;
}

function setOscillatorType(audioCtx, oscillator, type_) {
  oscillator.type = string_of_oscillatorType(type_);
  if (typeof type_ === "number") {
    return /* () */0;
  } else {
    var match = type_[0];
    var periodicWave = audioCtx.createPeriodicWave(match[/* real */0], match[/* imag */1]);
    oscillator.setPeriodicWave(periodicWave);
    return /* () */0;
  }
}

function makeOscillator(audioCtx, $staropt$star, $staropt$star$1) {
  var frequency = $staropt$star !== undefined ? $staropt$star : 440.0;
  var type_ = $staropt$star$1 !== undefined ? $staropt$star$1 : /* Sine */0;
  var oscillator = audioCtx.createOscillator();
  var t = audioCtx.currentTime;
  oscillator.frequency.setValueAtTime(frequency, t);
  setOscillatorType(audioCtx, oscillator, type_);
  return oscillator;
}

function string_of_filterType(filterType) {
  switch (filterType.tag | 0) {
    case 0 : 
        return "lowpass";
    case 1 : 
        return "highpass";
    case 2 : 
        return "bandpass";
    case 3 : 
        return "lowshelf";
    case 4 : 
        return "highshelf";
    case 5 : 
        return "peaking";
    case 6 : 
        return "notch";
    case 7 : 
        return "allpass";
    
  }
}

function makeFilter(audioCtx, filterType) {
  var filter = audioCtx.createBiquadFilter();
  filter.type = string_of_filterType(filterType);
  var t = audioCtx.currentTime;
  var exit = 0;
  switch (filterType.tag | 0) {
    case 3 : 
    case 4 : 
        exit = 1;
        break;
    case 5 : 
        filter.frequency.setValueAtTime(filterType[0], t);
        filter.Q.setValueAtTime(filterType[1], t);
        filter.gain.setValueAtTime(filterType[2], t);
        break;
    default:
      filter.frequency.setValueAtTime(filterType[0], t);
      filter.Q.setValueAtTime(filterType[1], t);
  }
  if (exit === 1) {
    filter.frequency.setValueAtTime(filterType[0], t);
    filter.gain.setValueAtTime(filterType[1], t);
  }
  return filter;
}

function makeBankOf(audioCtx, n, hasInput, f) {
  var input = hasInput ? Js_primitive.some(audioCtx.createGain()) : undefined;
  var output = audioCtx.createGain();
  var t = audioCtx.currentTime;
  var createNode = input !== undefined ? (function (i) {
        var node = Curry._2(f, audioCtx, i);
        input.connect(node);
        return node;
      }) : Curry._1(f, audioCtx);
  var nodes = $$Array.init(n, createNode);
  var gains = $$Array.map((function (node) {
          var gainNode = audioCtx.createGain();
          gainNode.gain.setValueAtTime(0.0, t);
          node.connect(gainNode);
          gainNode.connect(output);
          return gainNode;
        }), nodes);
  return /* record */[
          /* input */input,
          /* nodes */nodes,
          /* gains */gains,
          /* output */output,
          /* audioCtx */audioCtx
        ];
}

function makeFilterBank(audioCtx, filterN, q, freqFunc) {
  var createNode = function (audioCtx, i) {
    return makeFilter(audioCtx, /* BandPass */Block.__(2, [
                  Curry._1(freqFunc, filterN - i | 0),
                  q
                ]));
  };
  return makeBankOf(audioCtx, filterN, true, createNode);
}

function makeOscillatorBank(audioCtx, n, type_, freqFunc) {
  var createNode = function (audioCtx, i) {
    return makeOscillator(audioCtx, Curry._1(freqFunc, n - i | 0), type_);
  };
  var bank = makeBankOf(audioCtx, n, false, createNode);
  var t = audioCtx.currentTime;
  bank[/* output */3].gain.setValueAtTime(0.007, t);
  return bank;
}

function getAudioSource(ctx) {
  var match = UserMedia$Gayer.getAudioStream(/* () */0);
  if (match !== undefined) {
    return Js_primitive.valFromOption(match).then((function (mediaStream) {
                    return Promise.resolve(ctx.createMediaStreamSource(mediaStream));
                  })).catch((function (err) {
                  console.log(err);
                  return Promise.resolve(undefined);
                }));
  } else {
    return Promise.resolve(undefined);
  }
}

function connectFilterBank(noise, filterBank, compressor) {
  var match = filterBank[/* input */0];
  if (match !== undefined) {
    noise.connect(Js_primitive.valFromOption(match));
  }
  filterBank[/* output */3].connect(compressor);
  return /* () */0;
}

function disconnectFilterBank(noise, filterBank, compressor) {
  var match = filterBank[/* input */0];
  if (match !== undefined) {
    noise.disconnect(Js_primitive.valFromOption(match));
  }
  filterBank[/* output */3].disconnect(compressor);
  return /* () */0;
}

function updateBankGains(bank, gainValues) {
  var t = bank[/* audioCtx */4].currentTime;
  var n = gainValues.length;
  for(var i = 0 ,i_finish = n - 1 | 0; i <= i_finish; ++i){
    var gainI = (n - i | 0) - 1 | 0;
    Caml_array.caml_array_get(bank[/* gains */2], gainI).gain.setValueAtTime(Caml_array.caml_array_get(gainValues, i), t);
  }
  return /* () */0;
}

function updateFilterBank($staropt$star, $staropt$star$1, filterBank, filterValues) {
  var inputGain = $staropt$star !== undefined ? $staropt$star : 1.0;
  var outputGain = $staropt$star$1 !== undefined ? $staropt$star$1 : 0.1;
  var currentTime = filterBank[/* audioCtx */4].currentTime;
  var match = filterBank[/* input */0];
  if (match !== undefined) {
    Js_primitive.valFromOption(match).gain.setValueAtTime(inputGain, currentTime);
  }
  filterBank[/* output */3].gain.setValueAtTime(outputGain, currentTime);
  return updateBankGains(filterBank, filterValues);
}

function updateFilterBankDefinition(filterBank, freqFunc, q) {
  console.log("updating filter bank definitions (costly!)");
  var currentTime = filterBank[/* audioCtx */4].currentTime;
  var n = filterBank[/* nodes */1].length;
  return $$Array.iteri((function (i, filter) {
                filter.Q.setValueAtTime(q, currentTime);
                filter.frequency.setValueAtTime(Curry._1(freqFunc, (n - i | 0) - 1 | 0), currentTime);
                return /* () */0;
              }), filterBank[/* nodes */1]);
}

function audioInputSetting(r) {
  if (typeof r === "number") {
    if (r === 0) {
      return Json_encode.object_(/* :: */[
                  /* tuple */[
                    "type",
                    "pink-noise"
                  ],
                  /* [] */0
                ]);
    } else {
      return Json_encode.object_(/* :: */[
                  /* tuple */[
                    "type",
                    "mic"
                  ],
                  /* [] */0
                ]);
    }
  } else if (r.tag) {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "video"
                ],
                /* :: */[
                  /* tuple */[
                    "url",
                    r[0]
                  ],
                  /* [] */0
                ]
              ]);
  } else {
    return Json_encode.object_(/* :: */[
                /* tuple */[
                  "type",
                  "file"
                ],
                /* :: */[
                  /* tuple */[
                    "url",
                    r[0]
                  ],
                  /* [] */0
                ]
              ]);
  }
}

var EncodeAudioInput = /* module */[/* audioInputSetting */audioInputSetting];

function audioInputSetting$1(json) {
  var param = Json_decode.field("type", Json_decode.string, json);
  switch (param) {
    case "file" : 
        return Json_decode.map((function (url) {
                      return /* AudioFile */Block.__(0, [url]);
                    }), (function (param) {
                      return Json_decode.field("url", Json_decode.string, param);
                    }), json);
    case "mic" : 
        return /* Mic */1;
    case "pink-noise" : 
        return /* PinkNoise */0;
    case "video" : 
        return Json_decode.map((function (url) {
                      return /* AudioFromVideo */Block.__(1, [url]);
                    }), (function (param) {
                      return Json_decode.field("url", Json_decode.string, param);
                    }), json);
    default:
      return /* PinkNoise */0;
  }
}

var DecodeAudioInput = /* module */[/* audioInputSetting */audioInputSetting$1];

var AudioInput = /* module */[
  /* EncodeAudioInput */EncodeAudioInput,
  /* DecodeAudioInput */DecodeAudioInput
];

var midiNoteA440Hz = 69.0;

var pinkNoise = pinkNoiseFull;

export {
  makeDefaultAudioCtx ,
  string_of_oscillatorType ,
  midiNoteA440Hz ,
  noteToFrequency ,
  yToFrequency ,
  qForBinsPerOctave ,
  defaultQ ,
  defaultCompressorValues ,
  makeCompressor ,
  pinkNoiseFull ,
  cheaperPinkNoise ,
  pinkNoise ,
  makeAnalyser ,
  setOscillatorType ,
  makeOscillator ,
  string_of_filterType ,
  makeFilter ,
  makeBankOf ,
  makeFilterBank ,
  makeOscillatorBank ,
  getAudioSource ,
  connectFilterBank ,
  disconnectFilterBank ,
  updateBankGains ,
  updateFilterBank ,
  updateFilterBankDefinition ,
  AudioInput ,
  
}
/* defaultQ Not a pure module */
