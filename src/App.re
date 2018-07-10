open Audio;
open Music;

open Canvas;
open UserMedia;
open Video;

type filterInput = audioNode;

type visualInput = option(canvasImageSource);

type state = {
  xIndex: int,
  xDelta: int,
  inputGain: float,
  outputGain: float,
  q: float,
  filterInput,
  visualInput,
  micInput: option(audioNode),
  cameraInput: option(canvasImageSource),
  shouldClear: bool,
  channelToRead: channel,
  alpha: float,
  compositeOperation,
  allowedPitchClasses: PitchSet.t,
  filterBank: option(filterBank),
  canvasRef: ref(option(Dom.element)),
  timerId: ref(option(Js.Global.intervalId)),
};

let defaultState: state = {
  xIndex: 0,
  xDelta: 1,
  inputGain: 1.0,
  outputGain: 0.1,
  q: defaultQ,
  filterInput: defaultNoise,
  visualInput: None,
  micInput: None,
  cameraInput: None,
  shouldClear: true,
  alpha: 1.0,
  compositeOperation: SourceOver,
  channelToRead: R,
  allowedPitchClasses: PitchSet.of_list([0, 2, 5, 7, 9]),
  filterBank: None,
  canvasRef: ref(None),
  timerId: ref(None),
};

type action =
  | Clear
  | Tick
  | SetFilterInput(audioNode)
  | SetVisualInput(option(canvasImageSource))
  | SetMicInput(audioNode)
  | SetCameraInput(option(canvasImageSource))
  | SetFilterBank(filterBank)
  | SetXIndex(int)
  | SetXDelta(int);

let setCanvasRef = (theRef, {ReasonReact.state}) =>
  state.canvasRef := Js.Nullable.toOption(theRef);

let component = ReasonReact.reducerComponent("App");

let maybeUpdateCanvas:
  (ref(option(Dom.element)), canvasElement => unit) => unit =
  (maybeEl, f) =>
    switch (maybeEl^) {
    | None => ()
    | Some(canvas) => f(getFromReact(canvas))
    };

/* why can't I just use Js.Option.map here? */
let maybeMapFilterBank: (filterBank => unit, option(filterBank)) => unit =
  (f, maybeFilterBank) =>
    switch (maybeFilterBank) {
    | None => ()
    | Some(filterBank) => f(filterBank)
    };

let clearCanvas = (canvasElement, width, height) => {
  let ctx = getContext(canvasElement);
  Ctx.clearRect(ctx, 0, 0, width, height);
};

let drawCanvas = (canvasElement, width, height, state) => {
  if (state.shouldClear) {
    clearCanvas(canvasElement, width, height);
  };
  let ctx = getContext(canvasElement);
  Ctx.setGlobalAlpha(ctx, state.alpha);
  Ctx.setGlobalCompositeOperation(ctx, state.compositeOperation);

  switch (state.visualInput) {
  | None => ()
  | Some(input) => Ctx.drawImageDestRect(ctx, input, 0, 0, width, height)
  };

  let slice = Ctx.getImageData(ctx, state.xIndex, 0, 1, height);
  let values = imageDataToFloatArray(slice, state.channelToRead);

  Ctx.setGlobalAlpha(ctx, 1.0);
  Ctx.setGlobalCompositeOperation(ctx, SourceOver);
  Ctx.setFillStyle(ctx, "white");
  Ctx.fillRect(ctx, state.xIndex, 0, 1, height);
  values;
};

type domHighResTimeStamp;

[@bs.val] external window : Dom.window = "window";

[@bs.send]
external requestAnimationFrame :
  (Dom.window, domHighResTimeStamp => unit) => unit =
  "";

let make = (~width=120, ~height=120, _children) => {
  ...component,
  initialState: () => defaultState,
  reducer: (action, state) =>
    switch (action) {
    | SetXIndex(idx) => ReasonReact.Update({...state, xIndex: idx mod width})
    | SetXDelta(delta) => ReasonReact.Update({...state, xDelta: delta})
    | SetMicInput(mic) => ReasonReact.Update({...state, micInput: Some(mic)})
    | SetCameraInput(camera) =>
      ReasonReact.Update({...state, cameraInput: camera})
    | SetVisualInput(visualInput) =>
      ReasonReact.Update({...state, visualInput})
    | SetFilterInput(filterInput) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterInput},
        (
          self =>
            maybeMapFilterBank(
              connectFilterBank(self.state.filterInput),
              self.state.filterBank,
            )
        ),
      )
    | SetFilterBank(filterBank) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterBank: Some(filterBank)},
        (
          self =>
            maybeMapFilterBank(
              connectFilterBank(self.state.filterInput),
              self.state.filterBank,
            )
        ),
      )
    | Tick =>
      ReasonReact.UpdateWithSideEffects(
        {...state, xIndex: (state.xIndex + state.xDelta) mod width},
        (
          self =>
            maybeUpdateCanvas(
              self.state.canvasRef,
              canvas => {
                let rawFilterValues =
                  drawCanvas(canvas, width, height, self.state);
                let filterValues =
                  filterByPitchSet(
                    ~pitchClasses=self.state.allowedPitchClasses,
                    ~filterValues=rawFilterValues,
                  );
                maybeMapFilterBank(
                  filterBank =>
                    updateFilterBank(
                      ~filterBank,
                      ~filterValues,
                      ~inputGain=self.state.inputGain,
                      ~outputGain=self.state.outputGain,
                      ~q=self.state.q,
                    ),
                  self.state.filterBank,
                );
              },
            )
        ),
      )
    | Clear =>
      ReasonReact.SideEffects(
        (
          self =>
            maybeUpdateCanvas(self.state.canvasRef, canvas =>
              clearCanvas(canvas, width, height)
            )
        ),
      )
    },
  didMount: self => {
    let filterBank =
      defaultFilterBank(~ctx=defaultAudioCtx, ~n=height, ~q=defaultQ);
    connectFilterBank(self.state.filterInput, filterBank);

    (
      switch (getAudioVisualStream()) {
      | None => ()
      | Some(streamPromise) =>
        streamPromise
        |> Js.Promise.then_(stream => {
             let audio = createMediaStreamSource(defaultAudioCtx, stream);
             let video = attachVideoStream(stream);
             self.send(SetMicInput(audio));
             self.send(SetCameraInput(Some(video)));
             self.send(SetFilterInput(audio));
             self.send(SetVisualInput(Some(video)));
             Js.Promise.resolve();
           })
        |> ignore
      }
    )
    |> ignore;

    self.send(SetFilterBank(filterBank));
    self.send(Clear);

    self.state.timerId :=
      Some(Js.Global.setInterval(() => self.send(Tick), 33));
  },
  didUpdate: ({oldSelf, newSelf}) => {
    if (oldSelf.state.filterInput !== newSelf.state.filterInput) {
      maybeMapFilterBank(
        disconnectFilterBank(oldSelf.state.filterInput),
        oldSelf.state.filterBank,
      );
    };

    if (oldSelf.state.filterBank !== newSelf.state.filterBank) {
      maybeMapFilterBank(
        disconnectFilterBank(oldSelf.state.filterInput),
        oldSelf.state.filterBank,
      );
    };
  },
  willUnmount: self =>
    maybeMapFilterBank(
      disconnectFilterBank(self.state.filterInput),
      self.state.filterBank,
    ),
  render: self =>
    <div
      onClick=(_event => self.send(Tick))
      style=(
        ReactDOMRe.Style.make(
          ~display="flex",
          ~flexDirection="row",
          ~justifyContent="space-between",
          (),
        )
      )>
      <div style=(ReactDOMRe.Style.make(~margin="10px", ()))>
        <h1> (ReasonReact.string("GAYER")) </h1>
        <a href="https://github.com/corajr/gayer">
          (ReasonReact.string("source"))
        </a>
      </div>
      <canvas
        ref=(self.handle(setCanvasRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
        style=(
          ReactDOMRe.Style.make(
            ~transform="scale(4)",
            ~transformOrigin="top right",
            (),
          )
        )
      />
    </div>,
};
