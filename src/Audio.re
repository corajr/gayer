open UserMedia;

type audioTime = float; /* time in milliseconds */
[@bs.deriving abstract]
type audioContext = {currentTime: audioTime};

let defaultAudioCtx: audioContext = [%bs.raw
  {|new (window.AudioContext || window.webkitAudioContext)()|}
];

type audioParam;

type frequency = float;
type qFactor = float;
type filterGain = float;
type freqFunc = int => frequency;

type float32array;

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
  | Node;

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

[@bs.send] external createBiquadFilter : audioContext => biquadFilter = "";
[@bs.send] external createGain : audioContext => gainNode = "";
[@bs.send] external createDynamicsCompressor : audioContext => compressor = "";
[@bs.send]
external createMediaStreamSource : (audioContext, mediaStream) => audioNode =
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
external connectGainToNode : (gainNode, audioNode) => unit = "connect";
[@bs.send]
external connectCompressorToNode : (compressor, audioNode) => unit = "connect";

[@bs.send] external disconnect : ('a, 'b) => unit = "disconnect";

let midiNoteA440Hz = 69;
let frequencyFromNoteNumber: int => float =
  note =>
    440.0
    *. Js.Math.pow_float(2.0, float_of_int(note - midiNoteA440Hz) /. 12.0);

/* In principle, Q should be 1 / (2^(1/12) - 1) = 16.817 */
/* but a higher Q sounds better. */

let defaultQ = 34.127;

let defaultCompressorValues: compressorParamValues = {
  threshold: (-18.0),
  knee: 3.0,
  ratio: 10.0,
  attack: 0.01,
  release: 0.05,
};

let makeCompressor =
    (~audioCtx: audioContext, ~paramValues: compressorParamValues)
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

let defaultNoise = pinkNoise(defaultAudioCtx);

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
  %bs.raw
  "filter.type = 'bandpass'";
  /* filter |. (type_ = string_of_filterType(filterType)); */

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
        let filter = makeFilter(audioCtx, BandPass(freqFunc(i), q));
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

/*
 WebMidi.enable(function () {

     // Viewing available inputs and outputs
     console.log(WebMidi.inputs);
     console.log(WebMidi.outputs);

     // Retrieve an input by name, id or index
     var input = WebMidi.getInputByName("microKEY-37 MIDI 1");
     // OR...
     // input = WebMidi.getInputById("1809568182");
     // input = WebMidi.inputs[0];

     // Listen for a 'note on' message on all channels
     input.addListener('noteon', 'all',
         function (e) {
             const gain = e.velocity * 2;
             filterBank.setGainForNote(e.note.number, gain);
             // filterBank.filters[bin].Q.linearRampToValueAtTime(Q, defaultAudioCtx.currentTime + delay);
         }
     );
     input.addListener('noteoff', 'all',
         function (e) {
             filterBank.setGainForNote(e.note.number, 0);
            // filterBank.filters[bin].Q.linearRampToValueAtTime(1, defaultAudioCtx.currentTime + delay);
         }
     );
 });
 */

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

let defaultCompressor = {
  let compressor =
    makeCompressor(
      ~audioCtx=defaultAudioCtx,
      ~paramValues=defaultCompressorValues,
    );
  connectCompressorToNode(compressor, defaultSink(defaultAudioCtx));
  compressor;
};

let defaultFilterBank =
    (~ctx=defaultAudioCtx, ~n=120, ~q=defaultQ)
    : filterBank =>
  makeFilterBank(
    ~audioCtx=ctx,
    ~filterN=n,
    ~q,
    ~freqFunc=frequencyFromNoteNumber,
  );

let connectFilterBank = (noise, filterBank) => {
  connectNodeToGain(noise, filterBank.input);
  connectGainToNode(filterBank.output, unwrapCompressor(defaultCompressor));
};

let disconnectFilterBank = (noise, filterBank) => {
  disconnect(noise, filterBank.input);
  disconnect(filterBank.output, defaultCompressor);
};

let updateFilterBank =
    (
      ~filterBank: filterBank,
      ~filterValues: array(float),
      ~inputGain: float,
      ~outputGain: float,
      ~q=defaultQ,
    ) => {
  let currentTime = currentTime(filterBank.audioCtx);
  setValueAtTime(filterBank.input |. gain_, inputGain, currentTime);
  setValueAtTime(filterBank.output |. gain_, outputGain, currentTime);
  let n = Array.length(filterValues);
  for (i in 0 to n - 1) {
    setValueAtTime(
      filterBank.gains[n - i - 1] |. gain_,
      filterValues[i],
      currentTime,
    );

    setValueAtTime(filterBank.filters[i] |. qualityFactor, q, currentTime);
  };
};
