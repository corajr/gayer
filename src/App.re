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
  analysisCanvasRef: ref(option(Dom.element)),
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
  shouldClear: false,
  alpha: 0.25,
  compositeOperation: Overlay,
  channelToRead: R,
  allowedPitchClasses: pentatonic,
  filterBank: None,
  analysisCanvasRef: ref(None),
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

let setAnalysisCanvasRef = (theRef, {ReasonReact.state}) =>
  state.analysisCanvasRef := Js.Nullable.toOption(theRef);

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

let connectInputs: state => unit =
  state =>
    maybeMapFilterBank(
      connectFilterBank(state.filterInput),
      state.filterBank,
    );

let disconnectInputs: state => unit =
  state =>
    maybeMapFilterBank(
      disconnectFilterBank(state.filterInput),
      state.filterBank,
    );

let clearCanvas = (canvasElement, width, height) => {
  let ctx = getContext(canvasElement);
  Ctx.clearRect(ctx, 0, 0, width, height);
};

let drawCanvas = (canvasElement, width, height, state) => {
  if (state.shouldClear) {
    clearCanvas(canvasElement, width, height);
  };
  let ctx = getContext(canvasElement);

  switch (state.analysisCanvasRef^) {
  | None => ()
  | Some(analysisCanvas) =>
    let canvasElt = getFromReact(analysisCanvas);
    let canvasAsSource = getCanvasAsSource(canvasElt);
    Ctx.setGlobalAlpha(ctx, 1.0);
    Ctx.setGlobalCompositeOperation(ctx, SourceOver);
    Ctx.drawImage(ctx, canvasAsSource, state.xIndex, 0);
  };

  Ctx.setGlobalAlpha(ctx, state.alpha);
  Ctx.setGlobalCompositeOperation(ctx, state.compositeOperation);

  switch (state.visualInput) {
  | None => ()
  | Some(input) => Ctx.drawImageDestRect(ctx, input, 0, 0, width, height)
  };

  let slice = Ctx.getImageData(ctx, state.xIndex, 0, 1, height);
  let values = imageDataToFloatArray(slice, state.channelToRead);

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
        (self => connectInputs(self.state)),
      )
    | SetFilterBank(filterBank) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterBank: Some(filterBank)},
        (self => connectInputs(self.state)),
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
      makeFilterBank(
        ~audioCtx=defaultAudioCtx,
        ~filterN=height,
        ~q=defaultQ,
        ~freqFunc=frequencyFromNoteNumber(15),
      );
    self.send(SetFilterBank(filterBank));

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
             /* self.send(SetFilterInput(audio)); */
             self.send(SetVisualInput(Some(video)));
             Js.Promise.resolve();
           })
        |> ignore
      }
    )
    |> ignore;

    self.send(Clear);

    self.state.timerId :=
      Some(Js.Global.setInterval(() => self.send(Tick), 33));
  },
  didUpdate: ({oldSelf, newSelf}) =>
    if (oldSelf.state.filterInput != newSelf.state.filterInput
        || oldSelf.state.filterBank != newSelf.state.filterBank) {
      disconnectInputs(oldSelf.state);
    },
  willUnmount: self => disconnectInputs(self.state),
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
        <div>
          (
            ReasonReact.string(
              "UI forthcoming; for now, please download and edit...",
            )
          )
        </div>
        <a href="https://github.com/corajr/gayer">
          (ReasonReact.string("source"))
        </a>
        <div>
          (
            ReasonReact.string(
              "Allowed pitch classes: "
              ++ (
                switch (
                  Js.Json.stringifyAny(
                    PitchSet.elements(self.state.allowedPitchClasses),
                  )
                ) {
                | None => ""
                | Some(s) => s
                }
              ),
            )
          )
        </div>
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
      <AnalysisCanvas
        size=height
        audioCtx=defaultAudioCtx
        input=self.state.micInput
        saveRef=(self.handle(setAnalysisCanvasRef))
      />
    </div>,
};
