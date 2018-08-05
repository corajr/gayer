open Audio;
open Canvas;

type state = {
  analyser,
  cqt: CQT.t,
  canvasRef: ref(option(Dom.element)),
  timerId: ref(option(Js.Global.intervalId)),
};

type action =
  | Draw;

let drawCQTBar = (canvasRenderingContext2D, state) => {
  let audioData = CQT.getInputArray(state.cqt, 0);
  getFloatTimeDomainData(state.analyser, audioData);
  CQT.calc(state.cqt);
  CQT.renderLine(state.cqt, 1);
  let cqtLine = CQT.getOutputArray(state.cqt);
  let outputImageData = makeImageData(~cqtLine);

  Ctx.putImageData(canvasRenderingContext2D, outputImageData, 0, 0);
};

let component = ReasonReact.reducerComponent("AnalysisCanvas");

let make = (~size, ~audioCtx, ~input, ~saveRef, _children) => {
  let setCanvasRef = (theRef, {ReasonReact.state}) => {
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
          width: size,
        });

      let analyser =
        makeAnalyser(~audioContext=audioCtx, ~fftSize=cqt |. CQT.fftSize, ());

      {analyser, cqt, canvasRef: ref(None), timerId: ref(None)};
    },
    didMount: self => {
      switch (input) {
      | None => ()
      | Some(inputNode) =>
        connectNodeToAnalyser(inputNode, self.state.analyser)
      };
      self.onUnmount(() =>
        switch (input) {
        | None => ()
        | Some(inputNode) => disconnect(inputNode, self.state.analyser)
        }
      );

      let timerId = Js.Global.setInterval(() => self.send(Draw), 20);
      self.state.timerId := Some(timerId);
      self.onUnmount(() =>
        switch (self.state.timerId^) {
        | None => ()
        | Some(timerId) => Js.Global.clearInterval(timerId)
        }
      );
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
