open AnalysisOptions;
open Audio;
open AudioGraph;
open Canvas;
open ImageDataUtil;
open ReaderType;
open Timing;

type state = {
  analyserL: ref(analyser),
  analyserR: ref(analyser),
  channelSplitter: ref(channelSplitter),
  stereoPanner: ref(stereoPanner),
  cqt: ref(CQT.t),
  canvasRef: ref(option(Dom.element)),
  timerId: ref(option(Js.Global.intervalId)),
};

let drawCQTBar = (ctx, state, options, width, height) => {
  let audioDataL = CQT.getInputArray(state.cqt^, 0);
  let audioDataR = CQT.getInputArray(state.cqt^, 1);
  getFloatTimeDomainData(state.analyserL^, audioDataL);
  getFloatTimeDomainData(state.analyserR^, audioDataR);
  CQT.calc(state.cqt^);
  CQT.renderLine(state.cqt^, 1);
  let cqtLine = CQT.getOutputArray(state.cqt^);
  switch (options.readerType) {
  | Channel(_) =>
    let outputImageData = makeImageData(~cqtLine);
    Ctx.putImageData(ctx, outputImageData, width - 1, 0);
  | Saturation =>
    let outputImageData =
      makeImageDataWithPalette(~cqtLine, ~palette=Palette.saturationRainbow);
    Ctx.putImageData(ctx, outputImageData, width - 1, 0);
  };
};

let component = ReasonReact.reducerComponent("AnalysisCanvas");

let make =
    (
      ~width,
      ~height,
      ~layerKey,
      ~audioCtx,
      ~audioGraph,
      ~options,
      ~millisPerTick,
      ~saveRef,
      ~saveTick,
      _children,
    ) => {
  let setCanvasRef = (theRef, {ReasonReact.state, ReasonReact.send}) => {
    state.canvasRef := Js.Nullable.toOption(theRef);
    saveRef(theRef);
  };

  {
    ...component,
    initialState: () => {
      let cqt =
        CQT.createShowCQTBar({
          ...CQT.defaultCqtBarParams,
          rate: audioCtx |. sampleRate,
          width: height,
        });

      let fftSize = cqt |. CQT.fftSizeGet;
      Js.log(
        "Constant-Q transform initialized. Using FFT of size "
        ++ Js.Int.toString(fftSize),
      );

      let analyserL = makeAnalyser(~audioContext=audioCtx, ~fftSize, ());

      let analyserR = makeAnalyser(~audioContext=audioCtx, ~fftSize, ());

      let stereoPanner = createStereoPanner(audioCtx);
      let channelSplitter = createChannelSplitter(audioCtx);
      connect(stereoPanner, channelSplitter);
      connectWithOutputIndex(channelSplitter, analyserL, 0);
      connectWithOutputIndex(channelSplitter, analyserR, 1);

      {
        analyserL: ref(analyserL),
        analyserR: ref(analyserR),
        channelSplitter: ref(channelSplitter),
        stereoPanner: ref(stereoPanner),
        cqt: ref(cqt),
        canvasRef: ref(None),
        timerId: ref(None),
      };
    },
    didMount: self => {
      audioGraph :=
        audioGraph^
        |> addNode((layerKey, unwrapStereoPanner(self.state.stereoPanner^)))
        |> addEdge((layerKey ++ "input", layerKey, 0, 0))
        |> updateConnections;

      self.onUnmount(() =>
        audioGraph :=
          audioGraph^
          |> removeNode(layerKey)
          |> removeAllEdgesInvolvingNode(layerKey)
          |> updateConnections
      );

      switch (options.analysisSize) {
      | WithHistory(_) =>
        saveTick(self.onUnmount, layerKey, _t =>
          switch (self.state.canvasRef^) {
          | Some(canvas) =>
            let canvasElement = getFromReact(canvas);
            let ctx = getContext(canvasElement);

            Ctx.drawImage(ctx, getCanvasAsSource(canvasElement), -1, 0);
          | None => ()
          }
        )
      | _ => ()
      };

      setTimer(
        self.state.timerId,
        () =>
          switch (self.state.canvasRef^) {
          | Some(canvas) =>
            let canvasElement = getFromReact(canvas);
            let ctx = getContext(canvasElement);
            drawCQTBar(ctx, self.state, options, width, height);
          | None => ()
          },
        millisPerTick,
      );
      self.onUnmount(() => maybeClearTimer(self.state.timerId));
    },
    willUpdate: ({oldSelf, newSelf}) => {
      maybeClearTimer(oldSelf.state.timerId);
      setTimer(
        newSelf.state.timerId,
        () =>
          switch (newSelf.state.canvasRef^) {
          | Some(canvas) =>
            let canvasElement = getFromReact(canvas);
            let ctx = getContext(canvasElement);
            drawCQTBar(ctx, newSelf.state, options, width, height);
          | None => ()
          },
        millisPerTick,
      );
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
