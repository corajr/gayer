open Audio;
open Canvas;
open Timing;

type state = {
  analyserL: analyser,
  analyserR: analyser,
  channelSplitter,
  stereoPanner,
  cqt: CQT.t,
  canvasRef: ref(option(Dom.element)),
  timerId: ref(option(Js.Global.intervalId)),
};

type action =
  | Draw;

let drawCQTBar = (canvasRenderingContext2D, state) => {
  let audioDataL = CQT.getInputArray(state.cqt, 0);
  let audioDataR = CQT.getInputArray(state.cqt, 1);
  getFloatTimeDomainData(state.analyserL, audioDataL);
  getFloatTimeDomainData(state.analyserR, audioDataR);
  CQT.calc(state.cqt);
  CQT.renderLine(state.cqt, 1);
  let cqtLine = CQT.getOutputArray(state.cqt);
  let outputImageData = makeImageData(~cqtLine);

  Ctx.putImageData(canvasRenderingContext2D, outputImageData, 0, 0);
};

let component = ReasonReact.reducerComponent("AnalysisCanvas");

let make =
    (~size, ~audioCtx, ~input, ~millisPerTick, ~saveRef, ~saveTick, _children) => {
  let setCanvasRef = (theRef, {ReasonReact.state, ReasonReact.send}) => {
    state.canvasRef := Js.Nullable.toOption(theRef);
    saveRef(theRef);
    /* saveTick(() => send(Draw)); */
  };

  {
    ...component,
    initialState: () => {
      let cqt =
        CQT.createShowCQTBar({
          ...CQT.defaultCqtBarParams,
          rate: audioCtx |. sampleRate,
          width: size,
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
        analyserL,
        analyserR,
        channelSplitter,
        stereoPanner,
        cqt,
        canvasRef: ref(None),
        timerId: ref(None),
      };
    },
    didMount: self => {
      switch (input) {
      | None => ()
      | Some(inputNode) => connect(inputNode, self.state.stereoPanner)
      };
      self.onUnmount(() =>
        switch (input) {
        | None => ()
        | Some(inputNode) => disconnect(inputNode, self.state.stereoPanner)
        }
      );
      setTimer(self.state.timerId, () => self.send(Draw), millisPerTick);
      self.onUnmount(() => maybeClearTimer(self.state.timerId));
    },
    reducer: (action, state) =>
      switch (action) {
      | Draw =>
        switch (state.canvasRef^) {
        | None => ReasonReact.NoUpdate
        | Some(canvas) =>
          ReasonReact.SideEffects(
            (
              _self => {
                let canvasElement = getFromReact(canvas);
                let ctx = getContext(canvasElement);
                drawCQTBar(ctx, state);
              }
            ),
          )
        }
      },
    render: self =>
      <canvas
        ref=(self.handle(setCanvasRef))
        width="1"
        height=(Js.Int.toString(size))
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
