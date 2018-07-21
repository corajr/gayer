open Audio;
open Music;

open Canvas;
open UserMedia;
open Video;

type filterInput = audioNode;

type visualInput = option(canvasImageSource);

type params = {
  xDelta: int,
  inputGain: float,
  outputGain: float,
  q: float,
  transpose: int,
  shouldClear: bool,
  channelToRead: channel,
  alpha: float,
  compositeOperation,
  useVisual: bool,
  useAnalysis: bool,
  allowedPitchClasses: PitchSet.t,
};

let defaultParams: params = {
  xDelta: 1,
  inputGain: 1.0,
  outputGain: 0.2,
  q: defaultQ,
  transpose: 0,
  shouldClear: false,
  useVisual: true,
  useAnalysis: true,
  alpha: 0.25,
  compositeOperation: Overlay,
  channelToRead: R,
  allowedPitchClasses: cMajor,
};

module DecodeParams = {
  let params = json =>
    Json.Decode.{
      xDelta: json |> field("xDelta", int),
      inputGain: json |> field("inputGain", float),
      outputGain: json |> field("outputGain", float),
      q: json |> field("q", float),
      transpose: json |> field("transpose", int),
      shouldClear: json |> field("shouldClear", bool),
      useVisual: json |> field("useVisual", bool),
      useAnalysis: json |> field("useAnalysis", bool),
      alpha: json |> field("alpha", float),
      compositeOperation:
        json
        |> map(
             compositeOperation_of_string,
             field("compositeOperation", string),
           ),
      channelToRead:
        json |> map(channel_of_int, field("channelToRead", int)),
      allowedPitchClasses:
        json
        |> map(PitchSet.of_list, field("allowedPitchClasses", list(int))),
    };
};

module EncodeParams = {
  let params = r =>
    Json.Encode.(
      object_([
        ("xDelta", int(r.xDelta)),
        ("inputGain", float(r.inputGain)),
        ("outputGain", float(r.outputGain)),
        ("q", float(r.q)),
        ("transpose", int(r.transpose)),
        ("shouldClear", bool(r.shouldClear)),
        ("useVisual", bool(r.useVisual)),
        ("useAnalysis", bool(r.useAnalysis)),
        ("alpha", float(r.alpha)),
        (
          "compositeOperation",
          string(string_of_compositeOperation(r.compositeOperation)),
        ),
        ("channelToRead", int(int_of_channel(r.channelToRead))),
        (
          "allowedPitchClasses",
          list(int, PitchSet.elements(r.allowedPitchClasses)),
        ),
      ])
    );
};

type state = {
  xIndex: int,
  filterInput,
  visualInput,
  params,
  micInput: option(audioNode),
  cameraInput: option(canvasImageSource),
  filterBank: option(filterBank),
  analysisCanvasRef: ref(option(Dom.element)),
  canvasRef: ref(option(Dom.element)),
  timerId: ref(option(Js.Global.intervalId)),
};

let defaultState: state = {
  xIndex: 0,
  filterInput: defaultNoise,
  visualInput: None,
  micInput: None,
  cameraInput: None,
  params: defaultParams,
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
  | SetParams(params);

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
  if (state.params.shouldClear) {
    clearCanvas(canvasElement, width, height);
  };
  let ctx = getContext(canvasElement);

  if (state.params.useAnalysis) {
    switch (state.analysisCanvasRef^) {
    | None => ()
    | Some(analysisCanvas) =>
      let canvasElt = getFromReact(analysisCanvas);
      let canvasAsSource = getCanvasAsSource(canvasElt);
      Ctx.setGlobalAlpha(ctx, 1.0);
      Ctx.setGlobalCompositeOperation(ctx, SourceOver);
      Ctx.drawImage(ctx, canvasAsSource, state.xIndex, 0);
    };
  };

  Ctx.setGlobalAlpha(ctx, state.params.alpha);
  Ctx.setGlobalCompositeOperation(ctx, state.params.compositeOperation);

  if (state.params.useVisual) {
    switch (state.visualInput) {
    | None => ()
    | Some(input) => Ctx.drawImageDestRect(ctx, input, 0, 0, width, height)
    };
  };

  let slice = Ctx.getImageData(ctx, state.xIndex, 0, 1, height);
  let values = imageDataToFloatArray(slice, state.params.channelToRead);

  values;
};

type domHighResTimeStamp;

[@bs.val] external window : Dom.window = "window";

[@bs.send]
external requestAnimationFrame :
  (Dom.window, domHighResTimeStamp => unit) => unit =
  "";

[@bs.val] external decodeURIComponent : string => string = "";

let make = (~width=120, ~height=120, _children) => {
  ...component,
  initialState: () => defaultState,
  reducer: (action, state) =>
    switch (action) {
    | SetParams(params) => ReasonReact.Update({...state, params})
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
        {...state, xIndex: (state.xIndex + state.params.xDelta) mod width},
        (
          self =>
            maybeUpdateCanvas(
              self.state.canvasRef,
              canvas => {
                let rawFilterValues =
                  drawCanvas(canvas, width, height, self.state);
                let filterValues =
                  filterByPitchSet(
                    ~pitchClasses=self.state.params.allowedPitchClasses,
                    ~filterValues=rawFilterValues,
                  );
                maybeMapFilterBank(
                  filterBank =>
                    updateFilterBank(
                      ~filterBank,
                      ~filterValues,
                      ~inputGain=self.state.params.inputGain,
                      ~outputGain=self.state.params.outputGain,
                      ~freqFunc=
                        frequencyFromNoteNumber(
                          16 + self.state.params.transpose,
                        ),
                      ~q=self.state.params.q,
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
        ~freqFunc=frequencyFromNoteNumber(16),
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
      Some(Js.Global.setInterval(() => self.send(Tick), 20));

    let watcherID =
      ReasonReact.Router.watchUrl(url => {
        let hash = decodeURIComponent(url.hash);

        switch (Json.parse(hash)) {
        | Some(x) =>
          switch (Json.Decode.optional(DecodeParams.params, x)) {
          | None => ()
          | Some(params) => self.send(SetParams(params))
          }
        | None => ()
        };
      });
    self.onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));
    let startingParams =
      Js.Json.stringify(EncodeParams.params(self.state.params));
    ReasonReact.Router.push("#" ++ startingParams);
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
          ~minHeight=Js.Int.toString(height * 4),
          (),
        )
      )>
      <div style=(ReactDOMRe.Style.make(~margin="10px", ~width="50%", ()))>
        <h1> (ReasonReact.string("GAYER")) </h1>
        <div>
          (
            ReasonReact.string(
              "UI forthcoming; for now, please edit params in URL",
            )
          )
        </div>
        <a href="https://github.com/corajr/gayer">
          (ReasonReact.string("source"))
        </a>
        <br />
        (ReasonReact.string("params:"))
        <br />
        <div>
          (
            ReasonReact.string(
              Js.Json.stringifyWithSpace(
                EncodeParams.params(self.state.params),
                2,
              ),
            )
          )
        </div>
      </div>
      <div
        style=(
          ReactDOMRe.Style.make(
            ~margin="0",
            ~width="50%",
            ~minHeight=Js.Int.toString(height * 4),
            ~display="flex",
            ~justifyContent="flex-end",
            ~alignItems="flex-start",
            (),
          )
        )>
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
      </div>
    </div>,
};
