open Audio;
open Music;

open Canvas;
open Video;

type filterInput = audioNode;

type visualInput = option(canvasImageSource);

type state = {
  xIndex: int,
  xDelta: int,
  inputGain: float,
  outputGain: float,
  filterInput,
  visualInput,
  channelToRead: channel,
  allowedPitchClasses: PitchSet.t,
  filterBank: option(filterBank),
  canvasRef: ref(option(Dom.element)),
  timerId: ref(option(Js.Global.intervalId)),
};

type action =
  | Clear
  | Tick
  | SetFilterInput(audioNode)
  | SetVisualInput(option(canvasImageSource))
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

let drawCanvas =
    (canvasElement, width, height, xIndex, channelToRead, maybeVisualInput) => {
  let ctx = getContext(canvasElement);
  Ctx.setFillStyle(ctx, "black");
  Ctx.fillRect(ctx, 0, 0, width, height);

  switch (maybeVisualInput) {
  | None => ()
  | Some(input) => Ctx.drawImageDestRect(ctx, input, 0, 0, width, height)
  };

  let slice = Ctx.getImageData(ctx, xIndex, 0, 1, height);
  let values = imageDataToFloatArray(slice, channelToRead);

  Ctx.setStrokeStyle(ctx, "white");
  Ctx.line(ctx, (xIndex, 0), (xIndex, height));
  values;
};

let make = (~width=640, ~height=120, _children) => {
  ...component,
  initialState: () => {
    xIndex: 0,
    xDelta: 1,
    inputGain: 1.0,
    outputGain: 0.05,
    filterInput: defaultNoise,
    visualInput: None,
    channelToRead: A,
    allowedPitchClasses: PitchSet.of_list([0, 2, 5, 7, 9]),
    filterBank: None,
    canvasRef: ref(None),
    timerId: ref(None),
  },
  reducer: (action, state) =>
    switch (action) {
    | SetXIndex(idx) => ReasonReact.Update({...state, xIndex: idx mod width})
    | SetXDelta(delta) => ReasonReact.Update({...state, xDelta: delta})
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
    | Clear =>
      ReasonReact.SideEffects(
        (
          self =>
            maybeUpdateCanvas(self.state.canvasRef, canvas =>
              clearCanvas(canvas, width, height)
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
                  drawCanvas(
                    canvas,
                    width,
                    height,
                    self.state.xIndex,
                    self.state.channelToRead,
                    self.state.visualInput,
                  );
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
                    ),
                  self.state.filterBank,
                );
              },
            )
        ),
      )
    },
  didMount: self => {
    let filterBank =
      defaultFilterBank(~ctx=defaultAudioCtx, ~n=height, ~q=defaultQ);
    connectFilterBank(self.state.filterInput, filterBank);
    turnOnVideo()
    |> Js.Promise.then_(video => {
         self.send(SetVisualInput(video));
         Js.Promise.resolve();
       })
    |> ignore;
    self.send(SetFilterBank(filterBank));
    self.send(Clear);
    self.state.timerId :=
      Some(Js.Global.setInterval(() => self.send(Tick), 300));
    getAudioSource(defaultAudioCtx)
    |> Js.Promise.then_(maybeSource => {
         switch (maybeSource) {
         | None => ()
         | Some(source) => self.send(SetFilterInput(source))
         };
         Js.Promise.resolve();
       })
    |> ignore;
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
    <div onClick=(_event => self.send(Tick))>
      <h1> (ReasonReact.string("GAYER")) </h1>
      <canvas
        ref=(self.handle(setCanvasRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />
    </div>,
};
