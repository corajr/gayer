open Audio;
open Audio.AudioInput;
open Music;

open Canvas;
open Layer;
open Params;
open Presets;
open UserMedia;
open Video;

module RList = Rationale.RList;

type filterInput = audioNode;

type state = {
  readPos: ref(int),
  writePos: ref(int),
  filterInput: option(audioNode),
  params,
  presetDrawerOpen: bool,
  mediaStream: option(mediaStream),
  micInput: option(audioNode),
  cameraInput: ref(option(canvasImageSource)),
  filterBank: option(filterBank),
  compressor: ref(option(compressor)),
  analysisCanvasRef: ref(option(Dom.element)),
  savedImages: list(string),
  loadedImages: ref(Belt.Map.String.t(canvasImageSource)),
  loadedAudio: ref(Belt.Map.String.t(audioNode)),
  canvasRef: ref(option(Dom.element)),
  scaleCanvas: option(int),
  timerId: ref(option(Js.Global.intervalId)),
};

let defaultState: state = {
  readPos: ref(0),
  writePos: ref(0),
  presetDrawerOpen: false,
  filterInput: None,
  mediaStream: None,
  micInput: None,
  cameraInput: ref(None),
  params: snd(List.nth(presets, 0)),
  filterBank: None,
  compressor: ref(None),
  savedImages: [],
  loadedImages: ref(Belt.Map.String.empty),
  loadedAudio: ref(Belt.Map.String.empty),
  analysisCanvasRef: ref(None),
  canvasRef: ref(None),
  scaleCanvas: Some(2),
  timerId: ref(None),
};

type action =
  | Clear
  | Tick
  | TogglePresetDrawer
  | LoadAudioFile(string)
  | SaveImage
  | AddSavedImage(string)
  | SetFilterInput(audioNode)
  | SetMicInput(audioNode)
  | SetMediaStream(mediaStream)
  | SetFilterBank(filterBank)
  | SetParams(params);

[@bs.val] external decodeURIComponent : string => string = "";
[@bs.val] external encodeURIComponent : string => string = "";

let setCanvasRef = (theRef, {ReasonReact.state}) =>
  state.canvasRef := Js.Nullable.toOption(theRef);

let setAnalysisCanvasRef = (theRef, {ReasonReact.state}) =>
  state.analysisCanvasRef := Js.Nullable.toOption(theRef);

let setLayerRef = ((layer, theRef), {ReasonReact.send, ReasonReact.state}) => {
  let maybeRef = Js.Nullable.toOption(theRef);
  switch (layer.content, maybeRef) {
  | (Analysis(source), Some(_)) =>
    state.analysisCanvasRef := maybeRef;
    switch (source) {
    | AudioFile(url) =>
      if (! Belt.Map.String.has(state.loadedAudio^, url)) {
        send(LoadAudioFile(url));
      }
    | _ => ()
    };
  | (Webcam(_), Some(aRef)) =>
    switch (state.mediaStream) {
    | Some(stream) =>
      let video = attachVideoStream(aRef, stream);
      state.cameraInput := Some(video);
    | None => ()
    }
  | (Image(url), Some(aRef)) =>
    let img = getElementAsImageSource(aRef);
    state.loadedImages := Belt.Map.String.set(state.loadedImages^, url, img);
  | _ => ()
  };
};

let changeLayer = (oldLayer, newLayer, layers) =>
  switch (RList.indexOf(oldLayer, layers)) {
  | None => layers
  | Some(index) => RList.update(newLayer, index, layers)
  };

[%mui.withStyles
  "SizedDrawer"({
    paper: ReactDOMRe.Style.make(~position="relative", ~width="240px", ()),
  })
];

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
    switch (state.filterBank, state.filterInput, state.compressor^) {
    | (Some(filterBank), Some(filterInput), Some(compressor)) =>
      connectFilterBank(filterInput, filterBank, compressor)
    | _ => Js.log("could not connect inputs")
    };

let disconnectInputs: state => unit =
  state =>
    switch (state.filterBank, state.filterInput, state.compressor^) {
    | (Some(filterBank), Some(filterInput), Some(compressor)) =>
      disconnectFilterBank(filterInput, filterBank, compressor)
    | _ => Js.log("could not disconnect inputs")
    };

let clearCanvas = (canvasElement, width, height) => {
  let ctx = getContext(canvasElement);
  Ctx.clearRect(ctx, 0, 0, width, height);
};

let pushParamsState = newParams => {
  let newParamsJson =
    encodeURIComponent(Js.Json.stringify(EncodeParams.params(newParams)));
  ReasonReact.Router.push("#" ++ newParamsJson);
};

let setLayers = (params, newLayers) =>
  pushParamsState({...params, layers: newLayers});

let drawLayer: (ctx, int, int, state, layer) => option(array(float)) =
  (ctx, width, height, state, layer) => {
    Ctx.setGlobalAlpha(ctx, layer.alpha);
    Ctx.setGlobalCompositeOperation(ctx, layer.compositeOperation);
    Ctx.setTransform(ctx, layer.transformMatrix);
    Ctx._setFilter(ctx, layer.filters);

    switch (layer.content) {
    | Draw(cmds) =>
      DrawCommand.drawCommands(ctx, cmds);
      None;
    | Fill(s) =>
      Ctx.setFillStyle(ctx, s);
      Ctx.fillRect(ctx, 0, 0, width, height);
      None;
    | Analysis(_) =>
      switch (state.analysisCanvasRef^) {
      | None => ()
      | Some(analysisCanvas) =>
        let canvasElt = getFromReact(analysisCanvas);
        let canvasAsSource = getCanvasAsSource(canvasElt);
        let x =
          wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width);
        Ctx.drawImage(ctx, canvasAsSource, x, 0);
      };
      None;
    | Image(url) =>
      switch (Belt.Map.String.get(state.loadedImages^, url)) {
      | None => ()
      | Some(img) => Ctx.drawImageDestRect(ctx, img, 0, 0, width, height)
      };
      None;
    | Webcam(opts) =>
      switch (state.cameraInput^, opts.slitscan) {
      | (None, _) => ()
      | (Some(input), None) =>
        Ctx.drawImageDestRect(ctx, input, 0, 0, width, height)
      | (Some(input), Some({x: xToRead})) =>
        let xToWrite =
          wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width);
        Ctx.drawImageSourceRectDestRect(
          ctx,
          input,
          xToRead,
          0,
          1,
          480,
          xToWrite,
          0,
          1,
          height,
        );
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
      let xToRead =
        wrapCoord(state.readPos^ + state.params.readPosOffset, 0, width);
      let slice = Ctx.getImageData(ctx, xToRead, 0, 1, height);
      Ctx.setFillStyle(
        ctx,
        switch (channel) {
        | R => "red"
        | G => "green"
        | B => "blue"
        | A => "white"
        },
      );
      Ctx.fillRect(ctx, xToRead, 0, 1, height);
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

let getAnalysisInput:
  (audioContext, state, audioInputSetting) =>
  (audioContext, option(audioNode)) =
  (audioCtx, state, audioInput) =>
    switch (audioInput) {
    | AudioFile(s) => (audioCtx, Belt.Map.String.get(state.loadedAudio^, s))
    | PinkNoise => (audioCtx, Some(pinkNoise(audioCtx)))
    | Mic => (audioCtx, state.micInput)
    };

let setTimer = ({ReasonReact.send, ReasonReact.state}) => {
  switch (state.timerId^) {
  | None => ()
  | Some(id) => Js.Global.clearInterval(id)
  };

  state.timerId :=
    Some(
      Js.Global.setInterval(() => send(Tick), state.params.millisPerTick),
    );
};

type domHighResTimeStamp;

[@bs.val] external window : Dom.window = "window";

[@bs.send]
external requestAnimationFrame :
  (Dom.window, domHighResTimeStamp => unit) => unit =
  "";

let makeAudioElt: string => Dom.element = [%bs.raw
  url => {|
     var audio = document.createElement("audio");
     audio.id = "audio-elt";
     audio.src = url;
     audio.loop = true;
     audio.autoplay = true;
     console.log(audio);

     document.body.appendChild(audio);
     return audio;
     |}
];

let make =
    (~width=120, ~height=120, ~audioCtx=makeDefaultAudioCtx(), _children) => {
  ...component,
  initialState: () => defaultState,
  reducer: (action, state) =>
    switch (action) {
    | AddSavedImage(url) =>
      ReasonReact.Update({
        ...state,
        savedImages: [url, ...state.savedImages],
      })
    | SaveImage =>
      ReasonReact.SideEffects(
        (
          self =>
            switch (state.canvasRef^) {
            | None => ()
            | Some(canvas) =>
              let url = toDataURL(getFromReact(canvas));
              self.send(AddSavedImage(url));
            }
        ),
      )
    | TogglePresetDrawer =>
      ReasonReact.Update({
        ...state,
        presetDrawerOpen: ! state.presetDrawerOpen,
      })
    | LoadAudioFile(url) =>
      ReasonReact.SideEffects(
        (
          _self => {
            let elt = makeAudioElt(url);
            let mediaElementSource = createMediaElementSource(audioCtx, elt);
            state.loadedAudio :=
              Belt.Map.String.set(state.loadedAudio^, url, mediaElementSource);
          }
        ),
      )
    | SetParams(params) => ReasonReact.Update({...state, params})
    | SetMicInput(mic) => ReasonReact.Update({...state, micInput: Some(mic)})
    | SetMediaStream(stream) =>
      ReasonReact.Update({...state, mediaStream: Some(stream)})
    | SetFilterInput(filterInput) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterInput: Some(filterInput)},
        (self => connectInputs(self.state)),
      )
    | SetFilterBank(filterBank) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterBank: Some(filterBank)},
        (self => connectInputs(self.state)),
      )
    | Tick =>
      ReasonReact.SideEffects(
        (
          self => {
            self.state.readPos :=
              wrapCoord(state.readPos^, state.params.readPosDelta, width);

            self.state.writePos :=
              wrapCoord(state.writePos^, state.params.writePosDelta, width);
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
            );
          }
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
    let compressor =
      makeCompressor(~audioCtx, ~paramValues=defaultCompressorValues);
    connectCompressorToNode(compressor, defaultSink(audioCtx));
    self.state.compressor := Some(compressor);

    let noise = pinkNoise(audioCtx);
    self.send(SetFilterInput(noise));

    let filterBank =
      makeFilterBank(
        ~audioCtx,
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
             let audio = createMediaStreamSource(audioCtx, stream);
             self.send(SetMicInput(audio));
             Js.Promise.resolve();
           })
        |> ignore
      }
    )
    |> ignore;

    self.send(Clear);

    setTimer(self);

    let watcherID =
      ReasonReact.Router.watchUrl(url => {
        let hash = decodeURIComponent(url.hash);

        switch (Json.parse(hash)) {
        | Some(x) =>
          switch (Json.Decode.optional(DecodeParams.params, x)) {
          | None => Js.log("unable to decode params")
          | Some(params) => self.send(SetParams(params))
          }
        | None => Js.log("Could not parse json")
        };
      });
    self.onUnmount(() => ReasonReact.Router.unwatchUrl(watcherID));
    let url = ReasonReact.Router.dangerouslyGetInitialUrl();
    if (url.hash === "") {
      pushParamsState(snd(List.nth(presets, 0)));
    } else {
      ReasonReact.Router.push("#" ++ url.hash);
    };
  },
  didUpdate: ({oldSelf, newSelf}) => {
    if (oldSelf.state.filterInput != newSelf.state.filterInput
        || oldSelf.state.filterBank != newSelf.state.filterBank) {
      disconnectInputs(oldSelf.state);
    };

    if (oldSelf.state.params.audioInputSetting
        != newSelf.state.params.audioInputSetting) {
      let (_, audio) =
        getAnalysisInput(
          audioCtx,
          newSelf.state,
          newSelf.state.params.audioInputSetting,
        );
      switch (audio) {
      | None => ()
      | Some(audio) => newSelf.send(SetFilterInput(audio))
      };
    };

    /* If we change the read or write position offsets, immediately reset the read or write head to that position. */
    if (oldSelf.state.params.readPosOffset
        != newSelf.state.params.readPosOffset) {
      newSelf.state.readPos := newSelf.state.params.readPosOffset;
    };

    if (oldSelf.state.params.writePosOffset
        != newSelf.state.params.writePosOffset) {
      newSelf.state.writePos := newSelf.state.params.writePosOffset;
    };

    if (oldSelf.state.params.millisPerTick
        != newSelf.state.params.millisPerTick) {
      setTimer(newSelf);
    };
  },
  willUnmount: self => disconnectInputs(self.state),
  render: self =>
    MaterialUi.(
      <div>
        <CssBaseline />
        <AppBar position=`Sticky>
          <Toolbar>
            <IconButton
              color=`Inherit onClick=(_evt => self.send(TogglePresetDrawer))>
              <MaterialUIIcons.Menu />
            </IconButton>
            <Typography variant=`Title color=`Inherit>
              (ReasonReact.string("GAYER"))
            </Typography>
          </Toolbar>
        </AppBar>
        <div style=(ReactDOMRe.Style.make(~padding="12px", ()))>
          <SizedDrawer
            render=(
              classes =>
                <Drawer
                  variant=`Temporary
                  open_=self.state.presetDrawerOpen
                  classes=[Paper(classes.paper)]>
                  <div
                    style=(
                      ReactDOMRe.Style.make(
                        ~display="flex",
                        ~alignItems="center",
                        ~justifyContent="flex-end",
                        ~padding="0 8px",
                        (),
                      )
                    )>
                    <IconButton
                      onClick=(_evt => self.send(TogglePresetDrawer))
                      color=`Inherit>
                      <MaterialUIIcons.ChevronLeft />
                    </IconButton>
                  </div>
                  <Divider />
                  <div
                    tabIndex=0
                    role="button"
                    onClick=(_evt => self.send(TogglePresetDrawer))
                    onKeyDown=(_evt => self.send(TogglePresetDrawer))>
                    <List component=(`String("nav"))>
                      (
                        ReasonReact.array(
                          Array.of_list(presets)
                          |> Array.map(((name, preset)) =>
                               <ListItem
                                 key=name
                                 button=true
                                 onClick=(_evt => pushParamsState(preset))>
                                 <ListItemText>
                                   (ReasonReact.string(name))
                                 </ListItemText>
                               </ListItem>
                             ),
                        )
                      )
                    </List>
                  </div>
                </Drawer>
            )
          />
          <Grid container=true spacing=Grid.V24>
            <Grid item=true xs=Grid.V6>
              <Params
                params=self.state.params
                onMoveCard=(layers => setLayers(self.state.params, layers))
                onChangeLayer=(
                  (oldLayer, newLayer) =>
                    setLayers(
                      self.state.params,
                      changeLayer(
                        oldLayer,
                        newLayer,
                        self.state.params.layers,
                      ),
                    )
                )
                onSetRef=(
                  (layer, theRef) =>
                    self.handle(setLayerRef, (layer, theRef))
                )
                onSetParams=(newParams => pushParamsState(newParams))
                getAudio=(getAnalysisInput(audioCtx, self.state))
              />
            </Grid>
            <Grid item=true xs=Grid.V6>
              <div
                style=(
                  ReactDOMRe.Style.make(
                    ~marginBottom="24px",
                    ~minHeight=
                      Js.Int.toString(
                        switch (self.state.scaleCanvas) {
                        | None => height
                        | Some(i) => height * i
                        },
                      )
                      ++ "px",
                    (),
                  )
                )>
                <canvas
                  ref=(self.handle(setCanvasRef))
                  onClick=(evt => Js.log(evt))
                  width=(Js.Int.toString(width))
                  height=(Js.Int.toString(height))
                  style=(
                    switch (self.state.scaleCanvas) {
                    | None => ReactDOMRe.Style.make()
                    | Some(i) =>
                      ReactDOMRe.Style.make(
                        ~transform="scale(" ++ Js.Int.toString(i) ++ ")",
                        ~transformOrigin="top left",
                        (),
                      )
                    }
                  )
                />
              </div>
              <div>
                <div style=(ReactDOMRe.Style.make(~marginBottom="24px", ()))>
                  <Button
                    variant=`Contained onClick=(evt => self.send(SaveImage))>
                    <MaterialUIIcons.PhotoCamera />
                    (ReasonReact.string("Snapshot"))
                  </Button>
                </div>
                (
                  self.state.savedImages
                  |> Array.of_list
                  |> Array.map(url =>
                       <img
                         key=(Js.Int.toString(Hashtbl.hash(url)))
                         src=url
                       />
                     )
                  |> ReasonReact.array
                )
              </div>
            </Grid>
          </Grid>
        </div>
      </div>
    ),
};
