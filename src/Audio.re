open UserMedia;

type audioTime = float; /* time in milliseconds */

[@bs.deriving abstract]
type audioContext = {
  currentTime: audioTime,
  sampleRate: float,
};

let makeDefaultAudioCtx: unit => audioContext = [%bs.raw
  () => {|return new (window.AudioContext || window.webkitAudioContext)()|}
];

type audioParam;

type frequency = float;
type qFactor = float;
type filterGain = float;
type freqFunc = int => frequency;

type float32array = array(float);

type filterTypeField;

type filterType =
  | LowPass(frequency, qFactor)
  | HighPass(frequency, qFactor)
  | BandPass(frequency, qFactor)
  | LowShelf(frequency, filterGain)
  | HighShelf(frequency, filterGain)
  | Peaking(frequency, qFactor, filterGain)
  | Notch(frequency, qFactor)
  | AllPass(frequency, qFactor);

[@bs.deriving abstract]
type biquadFilter =
  pri {
    [@bs.as "type"]
    mutable type_: string,
    frequency: audioParam,
    gain: audioParam,
    [@bs.as "Q"]
    qualityFactor: audioParam,
  };

[@bs.deriving abstract]
type gainNode =
  pri {
    [@bs.as "gain"]
    gain_: audioParam,
  };

[@bs.deriving abstract]
type analyser =
  pri {
    mutable fftSize: int,
    frequencyBinCount: int,
    mutable minDecibels: float,
    mutable maxDecibels: float,
    mutable smoothingTimeConstant: float,
  };

[@bs.deriving abstract]
type compressor = {
  threshold: audioParam,
  knee: audioParam,
  ratio: audioParam,
  attack: audioParam,
  release: audioParam,
};

type compressorParamValues = {
  threshold: float,
  knee: float,
  ratio: float,
  attack: float,
  release: float,
};

type audioNode =
  | BiquadFilter(biquadFilter)
  | Gain(gainNode)
  | Compressor(compressor)
  | Analyser(analyser)
  | Node;

external unwrapAnalyser : analyser => audioNode = "%identity";
external unwrapFilter : biquadFilter => audioNode = "%identity";
external unwrapGain : gainNode => audioNode = "%identity";
external unwrapCompressor : compressor => audioNode = "%identity";

type filterBank = {
  input: gainNode,
  filters: array(biquadFilter),
  gains: array(gainNode),
  output: gainNode,
  audioCtx: audioContext,
};

/**
   https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode/getFrequencyResponse
   getFrequencyResponse(frequencyArray, magResponseOutput, phaseResponseOutput);

   magResponseOutput and phaseResponseOutput must be pre-initialized to have the
   same length as frequencyArray. */
[@bs.send]
external getFrequencyResponse :
  (biquadFilter, float32array, float32array, float32array) => audioParam =
  "";

[@bs.send] external createAnalyser : audioContext => analyser = "";
[@bs.send] external createBiquadFilter : audioContext => biquadFilter = "";
[@bs.send] external createGain : audioContext => gainNode = "";
[@bs.send] external createDynamicsCompressor : audioContext => compressor = "";
[@bs.send]
external createMediaStreamSource : (audioContext, mediaStream) => audioNode =
  "";

[@bs.send]
external createMediaElementSource : (audioContext, Dom.element) => audioNode =
  "";

[@bs.send] external setValueAtTime : (audioParam, float, float) => unit = "";

[@bs.send]
external connectFilterToGain : (biquadFilter, gainNode) => unit = "connect";
[@bs.send]
external connectGainToFilter : (gainNode, biquadFilter) => unit = "connect";
[@bs.send]
external connectFilterToFilter : (biquadFilter, biquadFilter) => unit =
  "connect";
[@bs.send]
external connectGainToGain : (gainNode, gainNode) => unit = "connect";
[@bs.send]
external connectGainToCompressor : (gainNode, compressor) => unit = "connect";
[@bs.send]
external connectNodeToGain : (audioNode, gainNode) => unit = "connect";
[@bs.send]
external connectNodeToAnalyser : (audioNode, analyser) => unit = "connect";
[@bs.send]
external connectGainToNode : (gainNode, audioNode) => unit = "connect";
[@bs.send]
external connectCompressorToNode : (compressor, audioNode) => unit = "connect";

[@bs.send] external disconnect : ('a, 'b) => unit = "disconnect";

let midiNoteA440Hz = 69.0;

let noteToFrequency: float => float =
  note => 440.0 *. Js.Math.pow_float(2.0, (note -. midiNoteA440Hz) /. 12.0);

let yToFrequency: (int, int, int, int) => float =
  (binsPerSemitone, offset, height) => {
    let fBinsPerSemitone = float_of_int(binsPerSemitone);
    let offset = float_of_int(offset);

    y => {
      let note = float_of_int(height - y) /. fBinsPerSemitone +. offset;
      noteToFrequency(note);
    };
  };

let qForBinsPerOctave: int => float =
  binsPerOctave =>
    1.0
    /. (Js.Math.pow_float(2.0, 1.0 /. float_of_int(binsPerOctave)) -. 1.0);

/* In principle, Q should be 1 / (2^(1/12) - 1) = 16.817 */
/* but a higher Q sounds better. */

let defaultQ = qForBinsPerOctave(48);

let defaultCompressorValues: compressorParamValues = {
  threshold: (-12.0),
  knee: 0.0,
  ratio: 20.0,
  attack: 0.01,
  release: 0.05,
};

let makeCompressor =
    (
      ~audioCtx: audioContext,
      ~paramValues: compressorParamValues=defaultCompressorValues,
    )
    : compressor => {
  let compressor = createDynamicsCompressor(audioCtx);
  let t = currentTime(audioCtx);
  setValueAtTime(compressor |. threshold, paramValues.threshold, t);
  setValueAtTime(compressor |. knee, paramValues.knee, t);
  setValueAtTime(compressor |. attack, paramValues.attack, t);
  setValueAtTime(compressor |. release, paramValues.release, t);
  compressor;
};

let pinkNoise: audioContext => audioNode = [%bs.raw
  audioCtx => {|
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
     |}
];

[@bs.send]
external getFloatFrequencyData : (analyser, array(float)) => unit = "";
[@bs.send]
external getByteFrequencyData : (analyser, array(int)) => unit = "";
[@bs.send]
external getFloatTimeDomainData : (analyser, array(float)) => unit = "";
[@bs.send]
external getByteTimeDomainData : (analyser, array(int)) => unit = "";

let makeAnalyser =
    (
      ~audioContext: audioContext,
      ~fftSize: int=2048,
      ~minDecibels: float=(-100.0),
      ~maxDecibels: float=(-30.0),
      ~smoothingTimeConstant: float=0.8,
      (),
    )
    : analyser => {
  let analyser = createAnalyser(audioContext);
  fftSizeSet(analyser, fftSize);
  minDecibelsSet(analyser, minDecibels);
  maxDecibelsSet(analyser, maxDecibels);
  smoothingTimeConstantSet(analyser, smoothingTimeConstant);
  analyser;
};

let string_of_filterType = filterType =>
  switch (filterType) {
  | LowPass(_, _) => "lowpass"
  | HighPass(_, _) => "highpass"
  | BandPass(_, _) => "bandpass"
  | LowShelf(_, _) => "lowshelf"
  | HighShelf(_, _) => "highshelf"
  | Peaking(_, _, _) => "peaking"
  | Notch(_, _) => "notch"
  | AllPass(_, _) => "allpass"
  };

let makeFilter =
    (~audioCtx: audioContext, ~filterType: filterType)
    : biquadFilter => {
  let filter = createBiquadFilter(audioCtx);
  type_Set(filter, string_of_filterType(filterType));

  let t = currentTime(audioCtx);

  switch (filterType) {
  | BandPass(f, q)
  | LowPass(f, q)
  | HighPass(f, q)
  | BandPass(f, q)
  | Notch(f, q)
  | AllPass(f, q) =>
    setValueAtTime(frequency(filter), f, t);
    setValueAtTime(qualityFactor(filter), q, t);
  | LowShelf(f, g)
  | HighShelf(f, g) =>
    setValueAtTime(frequency(filter), f, t);
    setValueAtTime(gain(filter), g, t);
  | Peaking(f, q, g) =>
    setValueAtTime(frequency(filter), f, t);
    setValueAtTime(qualityFactor(filter), q, t);
    setValueAtTime(gain(filter), g, t);
  };
  filter;
};

let makeFilterBank =
    (~audioCtx: audioContext, ~filterN: int, ~q: float, ~freqFunc: freqFunc)
    : filterBank => {
  let input = createGain(audioCtx);
  let output = createGain(audioCtx);
  let t = currentTime(audioCtx);
  let filters =
    Array.init(
      filterN,
      i => {
        let filter =
          makeFilter(audioCtx, BandPass(freqFunc(filterN - i), q));
        connectGainToFilter(input, filter);
        filter;
      },
    );
  let gains =
    Array.init(
      filterN,
      i => {
        let filter = filters[i];
        let gainNode = createGain(audioCtx);
        setValueAtTime(gainNode |. gain_, 0.0, t);
        connectFilterToGain(filter, gainNode);
        connectGainToGain(gainNode, output);
        gainNode;
      },
    );
  {filters, gains, input, output, audioCtx};
};

let getAudioSource: audioContext => Js.Promise.t(option(audioNode)) =
  ctx =>
    switch (getAudioStream()) {
    | None => Js.Promise.resolve(None)
    | Some(streamPromise) =>
      streamPromise
      |> Js.Promise.then_(mediaStream =>
           Js.Promise.resolve(
             Some(createMediaStreamSource(ctx, mediaStream)),
           )
         )
      |> Js.Promise.catch(err => {
           Js.log(err);
           Js.Promise.resolve(None);
         })
    };

[@bs.get] external defaultSink : audioContext => audioNode = "destination";

let connectFilterBank = (noise, filterBank, compressor) => {
  connectNodeToGain(noise, filterBank.input);
  connectGainToNode(filterBank.output, unwrapCompressor(compressor));
};

let disconnectFilterBank = (noise, filterBank, compressor) => {
  disconnect(noise, filterBank.input);
  disconnect(filterBank.output, compressor);
};

let updateFilterBank =
    (
      ~inputGain: float=1.0,
      ~outputGain: float=0.1,
      ~filterBank: filterBank,
      ~filterValues: array(float),
    ) => {
  let currentTime = currentTime(filterBank.audioCtx);
  setValueAtTime(filterBank.input |. gain_, inputGain, currentTime);
  setValueAtTime(filterBank.output |. gain_, outputGain, currentTime);
  let n = Array.length(filterValues);
  for (i in 0 to n - 1) {
    let filterbankI = n - i - 1;
    setValueAtTime(
      filterBank.gains[filterbankI] |. gain_,
      filterValues[i],
      currentTime,
    );
  };
};

let updateFilterBankDefinition =
    (~filterBank: filterBank, ~freqFunc: int => float, ~q: float) => {
  Js.log("updating filter bank definitions (costly!)");
  let currentTime = currentTime(filterBank.audioCtx);
  let n = Array.length(filterBank.filters);
  Array.iteri(
    (i, filter) => {
      setValueAtTime(filter |. qualityFactor, q, currentTime);
      setValueAtTime(filter |. frequency, freqFunc(n - i - 1), currentTime);
    },
    filterBank.filters,
  );
};

module AudioInput = {
  type audioInputSetting =
    | AudioFile(string)
    | PinkNoise
    | Mic;

  module EncodeAudioInput = {
    let audioInputSetting = r =>
      Json.Encode.(
        switch (r) {
        | AudioFile(url) =>
          object_([("type", string("file")), ("url", string(url))])
        | PinkNoise => object_([("type", string("pink-noise"))])
        | Mic => object_([("type", string("mic"))])
        }
      );
  };

  module DecodeAudioInput = {
    let audioInputSetting = json =>
      Json.Decode.(
        json
        |> field("type", string)
        |> (
          fun
          | "pink-noise" => PinkNoise
          | "mic" => Mic
          | "file" =>
            json |> map(url => AudioFile(url), field("url", string))
          | _ => PinkNoise
        )
      );
  };
};
