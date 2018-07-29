open Audio;
open Music;

open Canvas;
open Layer;
open Params;
open UserMedia;
open Video;

module RList = Rationale.RList;

type filterInput = audioNode;

type visualInput = option(canvasImageSource);

type state = {
  readPos: int,
  writePos: int,
  filterInput,
  visualInput,
  params,
  mediaStream: option(mediaStream),
  micInput: option(audioNode),
  cameraInput: ref(option(canvasImageSource)),
  filterBank: option(filterBank),
  analysisCanvasRef: ref(option(Dom.element)),
  loadedImages: ref(Belt.Map.String.t(canvasImageSource)),
  canvasRef: ref(option(Dom.element)),
  timerId: ref(option(Js.Global.intervalId)),
};

let defaultState: state = {
  readPos: 0,
  writePos: 0,
  filterInput: defaultNoise,
  visualInput: None,
  mediaStream: None,
  micInput: None,
  cameraInput: ref(None),
  params: defaultParams,
  filterBank: None,
  loadedImages: ref(Belt.Map.String.empty),
  analysisCanvasRef: ref(None),
  canvasRef: ref(None),
  timerId: ref(None),
};

type action =
  | Clear
  | Tick
  | MoveLayer(int, int)
  | SetFilterInput(audioNode)
  | SetMicInput(audioNode)
  | SetMediaStream(mediaStream)
  | SetFilterBank(filterBank)
  | SetParams(params);

let setCanvasRef = (theRef, {ReasonReact.state}) =>
  state.canvasRef := Js.Nullable.toOption(theRef);

let setAnalysisCanvasRef = (theRef, {ReasonReact.state}) =>
  state.analysisCanvasRef := Js.Nullable.toOption(theRef);

let setLayerRef = ((layer, theRef), {ReasonReact.send, ReasonReact.state}) => {
  let maybeRef = Js.Nullable.toOption(theRef);
  switch (layer.content, maybeRef) {
  | (Webcam, Some(aRef)) =>
    switch (state.mediaStream, state.cameraInput^) {
    | (Some(stream), None) =>
      let video = attachVideoStream(aRef, stream);
      state.cameraInput := Some(video);
    | _ => ()
    }
  | _ => ()
  };
};

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

let maybeLoadImage: (state, string) => unit =
  (state, url) =>
    switch (Belt.Map.String.get(state.loadedImages^, url)) {
    | None =>
      Js.Console.log(url);
      loadImage(url, img =>
        state.loadedImages :=
          Belt.Map.String.set(state.loadedImages^, url, img)
      );
    | Some(_) => ()
    };

let maybeLoadImages: state => unit =
  state =>
    List.iter(
      layer =>
        switch (layer.content) {
        | Image(url) => maybeLoadImage(state, url)
        | _ => ()
        },
      state.params.layers,
    );

let pushParamsState = newParams => {
  let newParamsJson = Js.Json.stringify(EncodeParams.params(newParams));
  ReasonReact.Router.push("#" ++ newParamsJson);
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
        let x =
          wrapCoord(state.writePos + state.params.writePosOffset, 0, width);
        Ctx.drawImage(ctx, canvasAsSource, x, 0);
      };
      None;
    | Image(url) =>
      switch (Belt.Map.String.get(state.loadedImages^, url)) {
      | None => ()
      | Some(img) => Ctx.drawImageDestRect(ctx, img, 0, 0, width, height)
      };
      None;
    | Webcam =>
      switch (state.cameraInput^) {
      | None => ()
      | Some(input) => Ctx.drawImageDestRect(ctx, input, 0, 0, width, height)
      };
      None;
    | PitchClasses(classes) =>
      let classList = PitchSet.elements(PitchSet.diff(allPitches, classes));

      Ctx.setFillStyle(ctx, "black");
      let binsPerSemitone = height / 120;
      for (i in 0 to height / 10) {
        List.iter(
          j => {
            let y = (i * 12 + j) * binsPerSemitone;
            Ctx.fillRect(ctx, 0, y, width, binsPerSemitone);
          },
          classList,
        );
      };
      None;
    | Reader(channel) =>
      let slice = Ctx.getImageData(ctx, state.readPos, 0, 1, height);
      Ctx.setFillStyle(
        ctx,
        switch (channel) {
        | R => "red"
        | G => "green"
        | B => "blue"
        | A => "white"
        },
      );
      Ctx.fillRect(ctx, state.readPos, 0, 1, height);
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
    | SetMediaStream(stream) =>
      ReasonReact.Update({...state, mediaStream: Some(stream)})
    | SetFilterInput(filterInput) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterInput},
        (self => connectInputs(self.state)),
      )
    | MoveLayer(dragIndex, hoverIndex) =>
      ReasonReact.SideEffects(
        (
          _self => {
            let layers = state.params.layers;
            let layer = List.nth(layers, dragIndex);
            let updatedLayers =
              layers
              |> RList.remove(dragIndex, 1)
              |> RList.insert(hoverIndex, layer);
            pushParamsState({...state.params, layers: updatedLayers});
          }
        ),
      )
    | SetFilterBank(filterBank) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterBank: Some(filterBank)},
        (self => connectInputs(self.state)),
      )
    | Tick =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          readPos: wrapCoord(state.readPos, state.params.readPosDelta, width),
          writePos:
            wrapCoord(state.writePos, state.params.writePosDelta, width),
        },
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
                        yToFrequency(
                          height / 120,
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
        ~freqFunc=
          yToFrequency(height / 120, 16 + self.state.params.transpose),
      );
    self.send(SetFilterBank(filterBank));

    (
      switch (getAudioVisualStream()) {
      | None => ()
      | Some(streamPromise) =>
        streamPromise
        |> Js.Promise.then_(stream => {
             self.send(SetMediaStream(stream));
             let audio = createMediaStreamSource(defaultAudioCtx, stream);
             self.send(SetMicInput(audio));
             /* let video = attachVideoStream(stream); */
             /* self.send(SetCameraInput(Some(video))); */
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
    let url = ReasonReact.Router.dangerouslyGetInitialUrl();
    if (url.hash === "") {
      let startingParams =
        Js.Json.stringify(EncodeParams.params(self.state.params));
      ReasonReact.Router.push("#" ++ startingParams);
    } else {
      ReasonReact.Router.push("#" ++ url.hash);
    };
    maybeLoadImages(self.state);
  },
  didUpdate: ({oldSelf, newSelf}) => {
    if (oldSelf.state.filterInput != newSelf.state.filterInput
        || oldSelf.state.filterBank != newSelf.state.filterBank) {
      disconnectInputs(oldSelf.state);
    };

    if (oldSelf.state.params.layers != newSelf.state.params.layers) {
      newSelf.state.cameraInput := None;
      maybeLoadImages(newSelf.state);
    };
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
        <a href="https://github.com/corajr/gayer">
          (ReasonReact.string("source"))
        </a>
        <br />
        <Params
          params=self.state.params
          onMoveCard=((i, j) => self.send(MoveLayer(i, j)))
          onSetRef=(
            (layer, theRef) => self.handle(setLayerRef, (layer, theRef))
          )
        />
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
