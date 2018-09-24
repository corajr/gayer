open Audio;
open AudioGraph;
open Canvas;
open RawAudio;
open TypedArray;
open Timing;

type state = {
  analyser: ref(analyser),
  audioDataFloat: ref(float32Array),
  audioDataByte: ref(uint8Array),
  audioDataByteForImage: ref(uint8ClampedArray),
  canvasRef: ref(option(Dom.element)),
  timerId: ref(option(Js.Global.intervalId)),
};

let drawRawAudioFloat = (layerRefs, state, x, y, width, height) => {
  getFloatTimeDomainData(
    state.analyser^,
    floatArrayAsArray(state.audioDataFloat^),
  );

  let outputImageData =
    createImageData(
      float32toUint8ClampedArray(state.audioDataFloat^),
      width,
      height,
    );

  /* switch (state.canvasRef^) { */
  /* | Some(canvas) => */
  /*   let ctx = getContext(getFromReact(canvas)); */
  /*   Ctx.putImageData(ctx, outputImageData, 0, 0); */
  /* | None => () */
  /* }; */

  switch (Belt.Map.String.get(layerRefs^, "root")) {
  | Some(canvas) =>
    let ctx = getContext(getFromReact(canvas));
    Ctx.putImageData(ctx, outputImageData, x, y);
  | None => ()
  };
};

let drawRawAudioUint8 = (layerRefs, channel, state, x, y, width, height) => {
  let audioDataByte = intArrayAsArray(state.audioDataByte^);
  let n = Array.length(audioDataByte);
  let audioDataByteForImage =
    uint8ClampedArrayAsArray(state.audioDataByteForImage^);
  getByteTimeDomainData(state.analyser^, audioDataByte);

  let channelOffset = int_of_channel(channel);
  for (i in 0 to n - 1) {
    audioDataByteForImage[i * 4 + channelOffset] = audioDataByte[i];
    audioDataByteForImage[i * 4 + 3] = 255;
  };
  /* Js.log(audioDataByteForImage); */

  let outputImageData = createImageData(audioDataByteForImage, width, height);

  switch (state.canvasRef^) {
  | Some(canvas) =>
    let ctx = getContext(getFromReact(canvas));
    Ctx.putImageData(ctx, outputImageData, 0, 0);
  | None => ()
  };
};

let component = ReasonReact.reducerComponent("AnalysisCanvas");

let make =
    (
      ~samples,
      ~width,
      ~height,
      ~saveTick,
      ~layerKey,
      ~layerRefs,
      ~audioCtx,
      ~encoding,
      ~audioGraph,
      ~setRef,
      ~x,
      ~y,
      _children,
    ) => {
  let setCanvasRef =
      (theRef, {ReasonReact.state, ReasonReact.send, ReasonReact.onUnmount}) => {
    state.canvasRef := Js.Nullable.toOption(theRef);
    setRef(theRef);
    saveTick(onUnmount, layerKey, _t =>
      switch (encoding) {
      | Float => drawRawAudioFloat(layerRefs, state, x, y, width, height)
      | Int8(c) => drawRawAudioUint8(layerRefs, c, state, x, y, width, height)
      }
    );
  };

  {
    ...component,
    initialState: () => {
      Js.log(
        "Raw audio canvas initialized. Using FFT of size "
        ++ Js.Int.toString(samples),
      );

      let analyser =
        makeAnalyser(~audioContext=audioCtx, ~fftSize=samples, ());

      {
        analyser: ref(analyser),
        audioDataFloat: ref(createFloat32Array(samples)),
        audioDataByte: ref(createUint8Array(samples)),
        audioDataByteForImage: ref(createUint8ClampedArray(samples * 4)),
        canvasRef: ref(None),
        timerId: ref(None),
      };
    },
    didMount: self => {
      audioGraph :=
        audioGraph^
        |> addNode((layerKey, unwrapAnalyser(self.state.analyser^)))
        |> addEdge((layerKey ++ "input", layerKey, 0, 0))
        |> updateConnections;

      self.onUnmount(() =>
        audioGraph :=
          audioGraph^
          |> removeNode(layerKey)
          |> removeAllEdgesInvolvingNode(layerKey)
          |> updateConnections
      );
      /* setTimer( */
      /*   self.state.timerId, */
      /*   () => drawRawAudioFloat(layerRefs, self.state, x, y, width, height), */
      /*   samples * 1000 / 44100, */
      /* ); */
      /* self.onUnmount(() => maybeClearTimer(self.state.timerId)); */
    },
    reducer: ((), _state) => ReasonReact.NoUpdate,
    render: self =>
      <canvas
        ref=(self.handle(setCanvasRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
        style=(
          ReactDOMRe.Style.make(
            ~position="absolute",
            ~visibility="hidden",
            (),
          )
        )
      />,
  };
};
