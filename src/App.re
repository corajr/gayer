open Audio;
open Audio.AudioInput;
open Music;

open Canvas;
open Fscreen;
open Layer;
open Params;
open Presets;
open Timing;
open UserMedia;
open Video;

module RList = Rationale.RList;

type filterInput = audioNode;

type state = {
  animationStartTime: ref(float),
  animationLastUpdated: ref(float),
  readPos: ref(int),
  writePos: ref(int),
  freqFuncParams: ref((int, int)),
  filterInput: option(audioNode),
  params,
  presetDrawerOpen: bool,
  mediaStream: option(mediaStream),
  micInput: option(audioNode),
  cameraInput: ref(option(canvasImageSource)),
  filterBanks: option(filterBanks),
  compressor: ref(option(compressor)),
  merger: ref(option(channelMerger)),
  analysisCanvasRef: ref(option(Dom.element)),
  midiCanvasRef: ref(option(Dom.element)),
  savedImages: list(string),
  loadedImages: ref(Belt.Map.String.t(canvasImageSource)),
  loadedAudio: ref(Belt.Map.String.t(audioNode)),
  canvasRef: ref(option(Dom.element)),
  scaleCanvas: option(float),
  fullscreenCanvas: bool,
  tickFunctions: ref(list(unit => unit)),
  timerId: ref(option(Js.Global.intervalId)),
};

let defaultState: state = {
  animationStartTime: ref(0.0),
  animationLastUpdated: ref(0.0),
  readPos: ref(0),
  writePos: ref(0),
  freqFuncParams: ref((1, 16)),
  presetDrawerOpen: false,
  filterInput: None,
  mediaStream: None,
  micInput: None,
  cameraInput: ref(None),
  params: snd(List.nth(presets, 0)),
  filterBanks: None,
  compressor: ref(None),
  merger: ref(None),
  savedImages: [],
  loadedImages: ref(Belt.Map.String.empty),
  loadedAudio: ref(Belt.Map.String.empty),
  analysisCanvasRef: ref(None),
  midiCanvasRef: ref(None),
  canvasRef: ref(None),
  scaleCanvas: Some(480.0 /. float_of_int(defaultSize)),
  fullscreenCanvas: false,
  tickFunctions: ref([]),
  timerId: ref(None),
};

type action =
  | Clear
  | Tick
  | TogglePresetDrawer
  | LoadAudioFile(string)
  | SaveImage
  | ToggleFullscreen
  | AddSavedImage(string)
  | SetFilterInput(audioNode)
  | SetMicInput(audioNode)
  | SetMediaStream(mediaStream)
  | SetFilterBanks(filterBanks)
  | SetParams(params);

[@bs.val] external decodeURIComponent : string => string = "";
[@bs.val] external encodeURIComponent : string => string = "";

let setCanvasRef = (theRef, {ReasonReact.state}) =>
  state.canvasRef := Js.Nullable.toOption(theRef);

let setAnalysisCanvasRef = (theRef, {ReasonReact.state}) =>
  state.analysisCanvasRef := Js.Nullable.toOption(theRef);

let setLayerRef =
    (audioCtx, (layer, theRef), {ReasonReact.send, ReasonReact.state}) => {
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
  | (MIDIKeyboard, Some(_)) => state.midiCanvasRef := maybeRef
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
  | (Video(url), Some(aRef)) =>
    let vid = getElementAsImageSource(aRef);
    state.loadedImages := Belt.Map.String.set(state.loadedImages^, url, vid);

    let readyState = ReactDOMRe.domElementToObj(aRef)##readyState;
    if (readyState >= 3) {
      switch (Belt.Map.String.get(state.loadedAudio^, url)) {
      | Some(_) => ()
      | None =>
        unmute(aRef);
        let mediaElementSource = createMediaElementSource(audioCtx, aRef);
        Js.log("adding element source");
        Js.log(mediaElementSource);
        state.loadedAudio :=
          Belt.Map.String.set(state.loadedAudio^, url, mediaElementSource);
      };
    };
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
    switch (state.filterBanks, state.filterInput, state.merger^) {
    | (
        Some(StereoBanks(filterBankL, filterBankR)),
        Some(filterInput),
        Some(merger),
      ) =>
      connectFilterBank(filterInput, filterBankL, merger, 0);
      connectFilterBank(filterInput, filterBankR, merger, 1);
    | (Some(MonoBank(filterBank)), Some(filterInput), Some(merger)) =>
      connectFilterBank(filterInput, filterBank, merger, 0);
      connectWithOutputAndInputIndex(filterBank.output, merger, 0, 1);

    | _ => Js.log("could not connect inputs")
    };

let disconnectInputs: state => unit =
  state =>
    switch (state.filterBanks, state.filterInput, state.merger^) {
    | (
        Some(StereoBanks(filterBankL, filterBankR)),
        Some(filterInput),
        Some(merger),
      ) =>
      disconnectFilterBank(filterInput, filterBankL, merger);
      disconnectFilterBank(filterInput, filterBankR, merger);
    | (Some(MonoBank(filterBank)), Some(filterInput), Some(merger)) =>
      disconnectFilterBank(filterInput, filterBank, merger)
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

let drawLayer: (ctx, int, int, state, layer) => option(filterValues) =
  (ctx, width, height, state, layer) => {
    Ctx.setGlobalAlpha(ctx, layer.alpha);
    Ctx.setGlobalCompositeOperation(ctx, layer.compositeOperation);
    Ctx.setTransform(ctx, layer.transformMatrix);

    /* NOTE:  setTransform will return to the identity matrix, then transforms
       translation, scaling, and skew. Rotations will take effect after the
       transform is complete (to allow for a different axis of rotation than the
       origin). */

    Ctx.rotate(ctx, layer.rotation);
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
        Ctx.drawImageDestRect(ctx, canvasAsSource, x, 0, 1, height);
      };
      None;
    | MIDIKeyboard =>
      switch (state.midiCanvasRef^) {
      | None => ()
      | Some(midiCanvas) =>
        let canvasElt = getFromReact(midiCanvas);
        let canvasAsSource = getCanvasAsSource(canvasElt);
        let x =
          wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width);
        Ctx.drawImageDestRect(ctx, canvasAsSource, x, 0, 1, height);
      };
      None;
    | Image(url)
    | Video(url) =>
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
      let pixelsPerSemitone = binsPerSemitone(height);
      for (i in 0 to height / 10) {
        List.iter(
          j => {
            let y = (i * 12 + j) * pixelsPerSemitone;
            Ctx.fillRect(ctx, 0, y, width, pixelsPerSemitone);
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
        | R => rgba(127, 0, 0, 0.5)
        | G => rgba(0, 127, 0, 0.5)
        | B => rgba(0, 0, 127, 0.5)
        | A => rgba(127, 127, 127, 0.5)
        },
      );
      Ctx.fillRect(ctx, xToRead, 0, 1, height);
      let (l, r) = imageDataToStereo(slice, R, B);
      Some(Stereo(l, r));
    /* Some(Mono(imageDataToFloatArray(slice, channel))); */
    /* Some( */
    /*   Stereo( */
    /*     imageDataToFloatArray(slice, R), */
    /*     imageDataToFloatArray(slice, B), */
    /*   ), */
    /* ); */
    };
  };

let drawCanvas = (canvasElement, width, height, state) => {
  if (state.params.shouldClear) {
    clearCanvas(canvasElement, width, height);
  };
  let ctx = getContext(canvasElement);

  let values =
    List.fold_left(
      (acc, layer) => {
        let newMaybeValues = drawLayer(ctx, width, height, state, layer);
        switch (newMaybeValues) {
        | None => acc
        | Some(newValues) => newValues
        };
      },
      Mono(Array.make(height, 0.0)),
      state.params.layers,
    );

  values;
};

let getAnalysisInput:
  (audioContext, state, audioInputSetting) =>
  (audioContext, option(audioNode)) =
  (audioCtx, state, audioInput) =>
    switch (audioInput) {
    | AudioFromVideo(s)
    | AudioFile(s) => (audioCtx, Belt.Map.String.get(state.loadedAudio^, s))
    | PinkNoise => (audioCtx, Some(pinkNoise(audioCtx)))
    | Mic => (audioCtx, state.micInput)
    };

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
    (
      ~width=defaultSize,
      ~height=defaultSize,
      ~audioCtx=makeDefaultAudioCtx(),
      _children,
    ) => {
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
    | ToggleFullscreen =>
      ReasonReact.Update({
        ...state,
        fullscreenCanvas: ! state.fullscreenCanvas,
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
    | SetFilterBanks(filterBanks) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterBanks: Some(filterBanks)},
        (self => connectInputs(self.state)),
      )
    | Tick =>
      ReasonReact.SideEffects(
        (
          self => {
            /* Js.log("heartbeat"); */
            self.state.readPos :=
              wrapCoord(state.readPos^, state.params.readPosDelta, width);

            self.state.writePos :=
              wrapCoord(state.writePos^, state.params.writePosDelta, width);

            maybeUpdateCanvas(
              self.state.canvasRef,
              canvas => {
                let filterValues = drawCanvas(canvas, width, height, state);
                let updateBank = (values, filterBank) =>
                  updateFilterBank(
                    ~filterBank,
                    ~filterValues=values,
                    ~inputGain=state.params.inputGain,
                    ~outputGain=state.params.outputGain,
                  );

                switch (self.state.filterBanks) {
                | None => ()
                | Some(MonoBank(filterBank)) =>
                  switch (filterValues) {
                  | Mono(filterValues) =>
                    updateBank(filterValues, filterBank)
                  | Stereo(filterValuesL, _) =>
                    updateBank(filterValuesL, filterBank)
                  }
                | Some(StereoBanks(filterBankL, filterBankR)) =>
                  switch (filterValues) {
                  | Mono(filterValues) =>
                    updateBank(filterValues, filterBankL);
                    updateBank(filterValues, filterBankR);
                  | Stereo(filterValuesL, filterValuesR) =>
                    updateBank(filterValuesL, filterBankL);
                    updateBank(filterValuesR, filterBankR);
                  }
                };
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
    let merger = createChannelMerger(audioCtx);
    self.state.merger := Some(merger);

    let compressor =
      makeCompressor(~audioCtx, ~paramValues=defaultCompressorValues);
    connect(merger, compressor);
    connectCompressorToNode(compressor, defaultSink(audioCtx));
    self.state.compressor := Some(compressor);

    let noise = pinkNoise(audioCtx);
    self.send(SetFilterInput(noise));

    let pixelsPerSemitone = binsPerSemitone(height);
    let defaultTranspose = 16;
    self.state.freqFuncParams := (pixelsPerSemitone, defaultTranspose);
    let freqFunc =
      yToFrequency(
        pixelsPerSemitone,
        defaultTranspose + self.state.params.transpose,
        height,
      );

    /* let oscillators = */
    /*   makeOscillatorBank(~audioCtx, ~n=height, ~type_=Sine, ~freqFunc); */
    /* Array.iter(startOscillator, oscillators.nodes); */
    /* self.state.oscillatorBank := Some(oscillators); */
    /* self.send(SetFilterInput(unwrapGain(oscillators.output))); */

    let filterBankL =
      makeFilterBank(~audioCtx, ~filterN=height, ~q=defaultQ, ~freqFunc);
    let filterBankR =
      makeFilterBank(~audioCtx, ~filterN=height, ~q=defaultQ, ~freqFunc);
    self.send(SetFilterBanks(MonoBank(filterBankL)));
    /* self.send(SetFilterBanks(StereoBanks(filterBankL, filterBankR))); */

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

    let sendTickFn = () => self.send(Tick);
    self.state.tickFunctions := [sendTickFn, ...self.state.tickFunctions^];

    let rec animationFn = timestamp => {
      /* let lastUpdated = self.state.animationLastUpdated^; */
      /* let timeSinceLastUpdate = timestamp -. lastUpdated; */

      /* self.state.animationLastUpdated := timestamp; */

      /* Time since beginning; Don't need to know yet, but maybe we would? */

      /* if (self.state.animationStartTime^ === 0.0) { */
      /*   self.state.animationStartTime := timestamp; */
      /* }; */
      /* let timeSinceBeginning = timestamp -. self.state.animationStartTime^; */

      /* Js.log(timeSinceLastUpdate); */

      List.iter(f => f(), self.state.tickFunctions^);
      requestAnimationFrame(window, animationFn);
    };

    /* requestAnimationFrame(window, animationFn); */

    setTimer(
      self.state.timerId,
      () => List.iter(f => f(), self.state.tickFunctions^),
      self.state.params.millisPerTick,
    );

    self.onUnmount(() => maybeClearTimer(self.state.timerId));

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
        || oldSelf.state.filterBanks != newSelf.state.filterBanks) {
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

    let layersChanged =
      oldSelf.state.params.layers != newSelf.state.params.layers;

    /* If we changed the layers or read/write position offsets, immediately reset the read or write head to the starting position. */
    if (layersChanged
        ||
        oldSelf.state.params.readPosOffset != newSelf.state.params.
                                                readPosOffset
        ||
        oldSelf.state.params.writePosOffset != newSelf.state.params.
                                                 readPosOffset) {
      newSelf.state.readPos := 0;
      newSelf.state.writePos := 0;
    };

    if (oldSelf.state.params.q != newSelf.state.params.q
        || oldSelf.state.params.transpose != newSelf.state.params.transpose) {
      let (pixelsPerSemitone, defaultTranspose) =
        newSelf.state.freqFuncParams^;
      let freqFunc =
        yToFrequency(
          pixelsPerSemitone,
          defaultTranspose + newSelf.state.params.transpose,
          height,
        );
      let updateFn = filterBank =>
        updateFilterBankDefinition(
          ~freqFunc,
          ~q=newSelf.state.params.q,
          ~filterBank,
        );

      switch (newSelf.state.filterBanks) {
      | None => ()
      | Some(StereoBanks(filterBankL, filterBankR)) =>
        updateFn(filterBankL);
        updateFn(filterBankR);
      | Some(MonoBank(filterBank)) => updateFn(filterBank)
      };
    };

    if (oldSelf.state.params.millisPerTick
        != newSelf.state.params.millisPerTick) {
      setTimer(
        newSelf.state.timerId,
        () => List.iter(f => f(), newSelf.state.tickFunctions^),
        newSelf.state.params.millisPerTick,
      );
    };
    if (oldSelf.state.fullscreenCanvas != newSelf.state.fullscreenCanvas) {
      if (newSelf.state.fullscreenCanvas) {
        switch (newSelf.state.canvasRef^) {
        | Some(canvas) => requestFullscreen(fscreen, canvas)
        | None => ()
        };
      } else {
        exitFullscreen(fscreen);
      };
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
                    self.handle(setLayerRef(audioCtx), (layer, theRef))
                )
                onSetParams=(newParams => pushParamsState(newParams))
                rootWidth=width
                rootHeight=height
                millisPerAudioTick=16
                saveTick=(
                  tickFn =>
                    self.state.tickFunctions :=
                      [tickFn, ...self.state.tickFunctions^]
                )
                getAudio=(getAnalysisInput(audioCtx, self.state))
              />
            </Grid>
            <Grid item=true xs=Grid.V6>
              <div
                style=(
                  ReactDOMRe.Style.make(
                    ~marginBottom="24px",
                    ~minHeight=
                      Js.Float.toString(
                        switch (self.state.scaleCanvas) {
                        | None => float_of_int(height)
                        | Some(i) => float_of_int(height) *. i
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
                        ~transform="scale(" ++ Js.Float.toString(i) ++ ")",
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
                /* <Button */
                /*   variant=`Contained */
                /*   onClick=(evt => self.send(ToggleFullscreen))> */
                /*   ( */
                /*     self.state.fullscreenCanvas ? */
                /*       <MaterialUIIcons.FullscreenExit /> : */
                /*       <MaterialUIIcons.Fullscreen /> */
                /*   ) */
                /*   (ReasonReact.string("Fullscreen")) */
                /* </Button> */
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
