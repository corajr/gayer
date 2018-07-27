open Audio;
open Music;

open Canvas;
open UserMedia;
open Video;

type filterInput = audioNode;

type visualInput = option(canvasImageSource);

type layerContent =
  | Webcam
  | Analysis
  | PitchClasses(PitchSet.t)
  | Reader(channel);

type layer = {
  content: layerContent,
  alpha: float,
  compositeOperation,
};

type params = {
  xDelta: int,
  inputGain: float,
  outputGain: float,
  q: float,
  transpose: int,
  shouldClear: bool,
  layers: list(layer),
};

let defaultParams: params = {
  xDelta: 1,
  inputGain: 1.0,
  outputGain: 0.2,
  q: defaultQ,
  transpose: 0,
  shouldClear: false,
  layers: [
    {content: Analysis, alpha: 1.0, compositeOperation: SourceOver},
    {content: Webcam, alpha: 0.25, compositeOperation: Overlay},
    {
      content: PitchClasses(cMajor),
      alpha: 1.0,
      compositeOperation: SourceOver,
    },
    {content: Reader(R), alpha: 0.0, compositeOperation: SourceOver},
  ],
};

module DecodeParams = {
  let layerByType = (type_, json) =>
    Json.Decode.(
      switch (type_) {
      | "webcam" => Webcam
      | "reader" =>
        json
        |> map(i => Reader(i), map(channel_of_int, field("channel", int)))
      | "analysis" => Analysis
      | "pitchClasses" =>
        json
        |> map(
             xs => PitchClasses(PitchSet.of_list(xs)),
             field("pc", list(int)),
           )
      | _ =>
        raise @@
        DecodeError(
          "Expected layer content, got " ++ Js.Json.stringify(json),
        )
      }
    );
  let layerContent = json =>
    Json.Decode.(json |> (field("type", string) |> andThen(layerByType)));

  let layer = json =>
    Json.Decode.{
      content: json |> field("content", layerContent),
      alpha: json |> field("alpha", float),
      compositeOperation:
        json
        |> map(
             compositeOperation_of_string,
             field("compositeOperation", string),
           ),
    };

  let params = json =>
    Json.Decode.{
      xDelta: json |> field("xDelta", int),
      inputGain: json |> field("inputGain", float),
      outputGain: json |> field("outputGain", float),
      q: json |> field("q", float),
      transpose: json |> field("transpose", int),
      shouldClear: json |> field("shouldClear", bool),
      layers: json |> field("layers", list(layer)),
    };
};

module EncodeParams = {
  let layerContent = r =>
    Json.Encode.(
      switch (r) {
      | Webcam => object_([("type", string("webcam"))])
      | Analysis => object_([("type", string("analysis"))])
      | PitchClasses(classes) =>
        object_([
          ("type", string("pitchClasses")),
          ("pc", list(int, PitchSet.elements(classes))),
        ])
      | Reader(channel) =>
        object_([
          ("type", string("reader")),
          ("channel", int(int_of_channel(channel))),
        ])
      }
    );

  let layer = r =>
    Json.Encode.(
      object_([
        ("content", layerContent(r.content)),
        ("alpha", float(r.alpha)),
        (
          "compositeOperation",
          string(string_of_compositeOperation(r.compositeOperation)),
        ),
      ])
    );
  let params = r =>
    Json.Encode.(
      object_([
        ("xDelta", int(r.xDelta)),
        ("inputGain", float(r.inputGain)),
        ("outputGain", float(r.outputGain)),
        ("q", float(r.q)),
        ("transpose", int(r.transpose)),
        ("shouldClear", bool(r.shouldClear)),
        ("layers", list(layer, r.layers)),
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

let drawLayer: (ctx, int, int, state, layer) => option(array(float)) =
  (ctx, width, height, state, layer) => {
    Ctx.setGlobalAlpha(ctx, layer.alpha);
    Ctx.setGlobalCompositeOperation(ctx, layer.compositeOperation);

    switch (layer.content) {
    | Analysis =>
      switch (state.analysisCanvasRef^) {
      | None => ()
      | Some(analysisCanvas) =>
        let canvasElt = getFromReact(analysisCanvas);
        let canvasAsSource = getCanvasAsSource(canvasElt);
        Ctx.drawImage(ctx, canvasAsSource, state.xIndex, 0);
      };
      None;
    | Webcam =>
      switch (state.cameraInput) {
      | None => ()
      | Some(input) => Ctx.drawImageDestRect(ctx, input, 0, 0, width, height)
      };
      None;
    | PitchClasses(classes) =>
      let classList = PitchSet.elements(PitchSet.diff(allPitches, classes));

      Ctx.setFillStyle(ctx, "black");
      for (i in 0 to height / 12) {
        List.iter(
          j => {
            let y = i * 12 + j;
            Ctx.fillRect(ctx, 0, y, width, 1);
          },
          classList,
        );
      };
      None;
    | Reader(channel) =>
      let slice = Ctx.getImageData(ctx, state.xIndex, 0, 1, height);
      Ctx.setFillStyle(
        ctx,
        switch (channel) {
        | R => "red"
        | G => "green"
        | B => "blue"
        | A => "white"
        },
      );
      Ctx.fillRect(ctx, state.xIndex, 0, 1, height);
      Some(imageDataToFloatArray(slice, channel));
    };
  };

let drawCanvas = (canvasElement, width, height, state) => {
  if (state.params.shouldClear) {
    clearCanvas(canvasElement, width, height);
  };
  let ctx = getContext(canvasElement);

  let values =
    List.fold_left(
      (values, layer) => {
        let newMaybeValues = drawLayer(ctx, width, height, state, layer);
        switch (newMaybeValues) {
        | None => values
        | Some(newValues) => newValues
        };
      },
      Array.make(height, 0.0),
      state.params.layers,
    );

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
                let filterValues =
                  drawCanvas(canvas, width, height, self.state);
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
