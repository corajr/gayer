open Audio;
open AudioGraph;
open Canvas;
open TypedArray;
open Timing;

type state = {
  analyser: ref(analyser),
  audioData: ref(float32Array),
  canvasRef: ref(option(Dom.element)),
  timerId: ref(option(Js.Global.intervalId)),
};

let drawRawAudio = (layerRefs, state, x, y, width, height) => {
  getFloatTimeDomainData(
    state.analyser^,
    floatArrayAsArray(state.audioData^),
  );

  let outputImageData =
    createImageData(
      float32toUint8ClampedArray(state.audioData^),
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
      ~audioGraph,
      ~setRef,
      ~x,
      ~y,
      _children,
    ) => {
  let setCanvasRef = (theRef, {ReasonReact.state, ReasonReact.send}) => {
    state.canvasRef := Js.Nullable.toOption(theRef);
    setRef(theRef);
    /* saveTick(layerKey, () => drawRawAudio(state, width, height)); */
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
        audioData: ref(createFloat32Array(samples)),
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
      setTimer(
        self.state.timerId,
        () => drawRawAudio(layerRefs, self.state, x, y, width, height),
        samples * 1000 / 44100,
      );
      self.onUnmount(() => maybeClearTimer(self.state.timerId));
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
