open Audio;
open Audio.AudioInput;
open AudioGraph;
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
  audioGraph: ref(audioGraph),
  micInput: option(audioNode),
  cameraInput: ref(option(canvasImageSource)),
  oscillatorBank: ref(option(bank(oscillator))),
  filterBanks: option(filterBanks),
  compressor: ref(option(compressor)),
  merger: ref(option(channelMerger)),
  layerRefs: ref(Belt.Map.String.t(Dom.element)),
  savedImages: list(string),
  loadedAudio: ref(Belt.Map.String.t(audioNode)),
  canvasRef: ref(option(Dom.element)),
  fullscreenCanvas: bool,
  tickFunctions: ref(Belt.Map.String.t(unit => unit)),
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
  audioGraph: ref(emptyAudioGraph),
  micInput: None,
  cameraInput: ref(None),
  params: snd(List.nth(presets, 0)),
  oscillatorBank: ref(None),
  filterBanks: None,
  compressor: ref(None),
  merger: ref(None),
  savedImages: [],
  layerRefs: ref(Belt.Map.String.empty),
  loadedAudio: ref(Belt.Map.String.empty),
  canvasRef: ref(None),
  fullscreenCanvas: false,
  tickFunctions: ref(Belt.Map.String.empty),
  timerId: ref(None),
};

type action =
  | Clear
  | Tick
  | TogglePresetDrawer
  | SaveImage
  | ToggleFullscreen
  | AddSavedImage(string)
  | SetFilterInput(audioNode)
  | SetMicInput(audioNode)
  | SetMediaStream(mediaStream)
  | SetFilterBanks(filterBanks)
  | ChangeLayer(layer, layer)
  | SetLayers(list(layer))
  | SetParams(params);

[@bs.val] external decodeURIComponent : string => string = "";
[@bs.val] external encodeURIComponent : string => string = "";

let setCanvasRef = (theRef, {ReasonReact.state}) =>
  state.canvasRef := Js.Nullable.toOption(theRef);

let setLayerRef =
    (audioCtx, (layer, theRef), {ReasonReact.send, ReasonReact.state}) => {
  let maybeRef = Js.Nullable.toOption(theRef);
  let layerKey = Js.Json.stringify(EncodeLayer.layerContent(layer.content));

  switch (layer.content, maybeRef) {
  | (Analysis(source), Some(aRef)) =>
    state.layerRefs := Belt.Map.String.set(state.layerRefs^, layerKey, aRef)
  | (MIDIKeyboard, Some(aRef)) =>
    state.layerRefs := Belt.Map.String.set(state.layerRefs^, layerKey, aRef)
  | (HandDrawn, Some(aRef)) =>
    state.layerRefs := Belt.Map.String.set(state.layerRefs^, layerKey, aRef)
  | (RawAudioWriter(_), Some(aRef)) =>
    state.layerRefs := Belt.Map.String.set(state.layerRefs^, layerKey, aRef)
  | (Webcam(_), Some(aRef)) =>
    switch (state.mediaStream) {
    | Some(stream) =>
      let video = attachVideoStream(aRef, stream);
      state.cameraInput := Some(video);
      state.layerRefs := Belt.Map.String.set(state.layerRefs^, layerKey, aRef);
    | None => ()
    }
  | (Image(url), Some(aRef)) =>
    state.layerRefs := Belt.Map.String.set(state.layerRefs^, layerKey, aRef)
  | (Video(url), Some(aRef)) =>
    state.layerRefs := Belt.Map.String.set(state.layerRefs^, layerKey, aRef);

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

    let layerKey =
      Js.Json.stringify(EncodeLayer.layerContent(layer.content));
    let maybeLayerRef = Belt.Map.String.get(state.layerRefs^, layerKey);

    switch (layer.content) {
    | Draw(cmds) =>
      DrawCommand.drawCommands(ctx, cmds);
      None;
    | Fill(s) =>
      Ctx.setFillStyle(ctx, s);
      Ctx.fillRect(ctx, 0, 0, width, height);
      None;
    | Analysis(_) =>
      switch (maybeLayerRef) {
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
      switch (maybeLayerRef) {
      | None => ()
      | Some(midiCanvas) =>
        let canvasElt = getFromReact(midiCanvas);
        let canvasAsSource = getCanvasAsSource(canvasElt);
        let x =
          wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width);
        Ctx.drawImageDestRect(ctx, canvasAsSource, x, 0, 1, height);
      };
      None;
    | RawAudioWriter({x, y, w, h}) =>
      switch (maybeLayerRef) {
      | None => ()
      | Some(canvas) =>
        let otherCtx = getContext(getFromReact(canvas));
        let data = Ctx.getImageData(otherCtx, 0, 0, w, h);
        Ctx.putImageData(ctx, data, x, y);
      };
      None;
    | RawAudioReader({x, y, w, h, sampleRate}) =>
      open TypedArray;

      let imageData = Ctx.getImageData(ctx, x, y, w, h);
      switch (getNode("compressor", state.audioGraph^)) {
      | None => ()
      | Some(sink) =>
        let audioCtx = getAudioContext(sink);
        let buffer = createBuffer(audioCtx, 1, w * h, sampleRate);
        let rawImgData = toFloat32Array(dataGet(imageData));
        copyToChannel(buffer, rawImgData, 0, 0);
        let node = createBufferSource(audioCtx);
        bufferSet(node, buffer);
        connect(node, sink);
        startAudioBufferSourceNode(node);
      };
      None;
    | HandDrawn =>
      switch (maybeLayerRef) {
      | None => ()
      | Some(handDrawn) =>
        let canvasElt = getFromReact(handDrawn);
        let canvasAsSource = getCanvasAsSource(canvasElt);
        Ctx.drawImageDestRect(ctx, canvasAsSource, 0, 0, width, height);
      };
      None;
    | Image(url)
    | Video(url) =>
      switch (maybeLayerRef) {
      | None => ()
      | Some(aRef) =>
        let img = getElementAsImageSource(aRef);
        Ctx.drawImageDestRect(ctx, img, 0, 0, width, height);
      };
      None;
    | Webcam(opts) =>
      let cameraWidth = 640;
      let cameraHeight = 480;

      let cameraWidthToCanvasWidth =
        float_of_int(cameraWidth) /. float_of_int(width);
      let cameraHeightToCanvasHeight =
        float_of_int(cameraHeight) /. float_of_int(height);

      switch (state.cameraInput^, opts.slitscan) {
      | (None, _) => ()
      | (Some(input), None) =>
        Ctx.drawImageDestRect(ctx, input, 0, 0, width, height)
      | (Some(input), Some(StaticX(xToRead))) =>
        let xToWrite =
          wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width);
        Ctx.drawImageSourceRectDestRect(
          ctx,
          input,
          xToRead,
          0,
          1,
          cameraHeight,
          xToWrite,
          0,
          1,
          height,
        );
      | (Some(input), Some(ReadPosX)) =>
        let xToWrite =
          wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width);
        let xToReadCamera =
          int_of_float(float_of_int(xToWrite) *. cameraWidthToCanvasWidth);
        Ctx.drawImageSourceRectDestRect(
          ctx,
          input,
          xToReadCamera,
          0,
          int_of_float(cameraWidthToCanvasWidth),
          cameraHeight,
          xToWrite,
          0,
          1,
          height,
        );
      | (Some(input), Some(ReadPosY)) =>
        let yToWrite =
          wrapCoord(state.writePos^ + state.params.writePosOffset, 0, height);

        let yToRead =
          int_of_float(float_of_int(yToWrite) *. cameraHeightToCanvasHeight);

        Ctx.drawImageSourceRectDestRect(
          ctx,
          input,
          0,
          yToRead,
          cameraWidth,
          int_of_float(cameraHeightToCanvasHeight),
          0,
          yToWrite,
          width,
          1,
        );
      | (Some(input), Some(StaticY(yToRead))) =>
        /* TODO: fix this :( */
        let yToWrite =
          wrapCoord(state.writePos^ + state.params.writePosOffset, 0, height);

        let yToReadCamera =
          int_of_float(float_of_int(yToRead) *. cameraHeightToCanvasHeight);

        Ctx.drawImageSourceRectDestRect(
          ctx,
          input,
          0,
          yToReadCamera,
          cameraWidth,
          int_of_float(cameraHeightToCanvasHeight),
          0,
          yToWrite,
          width,
          1,
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
    | HistogramReader =>
      let xToRead =
        wrapCoord(state.readPos^ + state.params.readPosOffset, 0, width);
      let slice = Ctx.getImageData(ctx, xToRead, 0, 1, height);
      let histogram = imageDataToHistogram(state.params.height, slice);
      let revHistogram =
        Array.init(state.params.height, i =>
          histogram[state.params.height - i - 1]
        );

      let img = makeImageDataFromFloats(revHistogram, 1, state.params.height);
      Ctx.putImageData(ctx, img, xToRead, 0);
      Some(Mono(histogram));
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
      switch (channel) {
      | R
      | G
      | B =>
        let (l, r) = imageDataToStereo(slice, channel, B);
        Some(Stereo(l, r));
      | A => Some(Mono(imageDataToFloatArray(slice, channel)))
      };
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
    | AudioFromVideo(s) => (
        audioCtx,
        Belt.Map.String.get(state.loadedAudio^, s),
      )
    | AudioFile(_) => (audioCtx, None)
    | Oscillator(oType) => (
        audioCtx,
        switch (state.oscillatorBank^) {
        | None => None
        | Some(bank) => Some(unwrapGain(bank.output))
        },
      )
    | PinkNoise => (audioCtx, Some(pinkNoise(audioCtx)))
    | WhiteNoise => (audioCtx, Some(whiteNoise(audioCtx)))
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

let sortLayers = List.sort((a, b) => compare(a.content, b.content));

let generateNewFilterBanks =
    (audioCtx, {ReasonReact.state, ReasonReact.send}) => {
  Js.log("regenerating filter banks (costly!)");

  let pixelsPerSemitone = binsPerSemitone(state.params.height);
  let defaultTranspose = 16;
  state.freqFuncParams := (pixelsPerSemitone, defaultTranspose);
  let freqFunc =
    yToFrequency(
      pixelsPerSemitone,
      defaultTranspose + state.params.transpose,
      state.params.height,
    );

  /* let oscillators = */
  /*   makeOscillatorBank( */
  /*     ~audioCtx, */
  /*     ~n=state.params.height, */
  /*     ~type_=Square, */
  /*     ~freqFunc, */
  /*   ); */
  /* Array.iter(startOscillator, oscillators.nodes); */
  /* state.oscillatorBank := Some(oscillators); */

  /* state.audioGraph := */
  /*   state.audioGraph^ */
  /*   |> addNode(("oscillators", unwrapGain(oscillators.output))) */
  /*   |> addEdge(("oscillators", "compressor", 0, 0)) */
  /*   |> updateConnections; */

  if (state.params.stereo) {
    let filterBankL =
      makeFilterBank(
        ~audioCtx,
        ~filterN=state.params.height,
        ~q=state.params.q,
        ~freqFunc,
      );
    let filterBankR =
      makeFilterBank(
        ~audioCtx,
        ~filterN=state.params.height,
        ~q=state.params.q,
        ~freqFunc,
      );

    send(SetFilterBanks(StereoBanks(filterBankL, filterBankR)));
  } else {
    let filterBank =
      makeFilterBank(
        ~audioCtx,
        ~filterN=state.params.height,
        ~q=state.params.q,
        ~freqFunc,
      );

    send(SetFilterBanks(MonoBank(filterBank)));
  };
};

let make = (~audioCtx=makeDefaultAudioCtx(), _children) => {
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
    | SetParams(params) => ReasonReact.Update({...state, params})
    | SetMicInput(mic) => ReasonReact.Update({...state, micInput: Some(mic)})
    | SetMediaStream(stream) =>
      ReasonReact.Update({...state, mediaStream: Some(stream)})
    | SetFilterInput(filterInput) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterInput: Some(filterInput)},
        (self => connectInputs(self.state)),
      )
    | ChangeLayer(oldLayer, newLayer) =>
      ReasonReact.SideEffects(
        (
          self =>
            pushParamsState({
              ...self.state.params,
              layers:
                changeLayer(oldLayer, newLayer, self.state.params.layers),
            })
        ),
      )

    | SetLayers(layers) =>
      ReasonReact.SideEffects(
        (self => pushParamsState({...self.state.params, layers})),
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
              wrapCoord(
                state.readPos^,
                state.params.readPosDelta,
                self.state.params.width,
              );

            self.state.writePos :=
              wrapCoord(
                state.writePos^,
                state.params.writePosDelta,
                self.state.params.width,
              );

            maybeUpdateCanvas(
              self.state.canvasRef,
              canvas => {
                let filterValues =
                  drawCanvas(
                    canvas,
                    self.state.params.width,
                    self.state.params.height,
                    state,
                  );
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

                switch (self.state.oscillatorBank^) {
                | Some(bank) =>
                  switch (filterValues) {
                  | Mono(filterValues) => updateBankGains(bank, filterValues)
                  | Stereo(filterValuesL, _) =>
                    updateBankGains(bank, filterValuesL)
                  }
                | None => ()
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
              clearCanvas(
                canvas,
                self.state.params.width,
                self.state.params.height,
              )
            )
        ),
      )
    },
  didMount: self => {
    let merger = createChannelMerger(audioCtx);
    self.state.merger := Some(merger);

    let compressor =
      makeCompressor(~audioCtx, ~paramValues=defaultCompressorValues);

    self.state.compressor := Some(compressor);

    let noise = pinkNoise(audioCtx);
    self.send(SetFilterInput(noise));

    self.state.audioGraph :=
      self.state.audioGraph^
      |> addNode(("merger", unwrapChannelMerger(merger)))
      |> addNode(("compressor", unwrapCompressor(compressor)))
      |> addNode(("sink", defaultSink(audioCtx)))
      |> addEdge(("merger", "compressor", 0, 0))
      |> addEdge(("compressor", "sink", 0, 0))
      |> updateConnections;

    generateNewFilterBanks(audioCtx, self);

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
    self.state.tickFunctions :=
      Belt.Map.String.set(self.state.tickFunctions^, "master", sendTickFn);

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

      Array.iter(
        f => f(),
        Belt.Map.String.valuesToArray(self.state.tickFunctions^),
      );
      requestAnimationFrame(window, animationFn);
    };

    /* requestAnimationFrame(window, animationFn); */

    setTimer(
      self.state.timerId,
      () =>
        Array.iter(
          f => f(),
          Belt.Map.String.valuesToArray(self.state.tickFunctions^),
        ),
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
        || oldSelf.state.params.transpose != newSelf.state.params.transpose
        || oldSelf.state.params.stereo != newSelf.state.params.stereo
        || oldSelf.state.params.height != newSelf.state.params.height) {
      generateNewFilterBanks(audioCtx, newSelf);
    };

    if (oldSelf.state.params.millisPerTick
        != newSelf.state.params.millisPerTick) {
      setTimer(
        newSelf.state.timerId,
        () =>
          Array.iter(
            f => f(),
            Belt.Map.String.valuesToArray(newSelf.state.tickFunctions^),
          ),
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
                onMoveCard=(layers => self.send(SetLayers(layers)))
                onChangeLayer=(
                  (oldLayer, newLayer) =>
                    self.send(ChangeLayer(oldLayer, newLayer))
                )
                onSetRef=(
                  (layer, theRef) =>
                    self.handle(setLayerRef(audioCtx), (layer, theRef))
                )
                layerRefs=self.state.layerRefs
                onSetParams=(newParams => pushParamsState(newParams))
                millisPerAudioTick=16
                saveTick=(
                  (key, tickFn) =>
                    self.state.tickFunctions :=
                      Belt.Map.String.set(
                        self.state.tickFunctions^,
                        key,
                        tickFn,
                      )
                )
                getAudio=(getAnalysisInput(audioCtx, self.state))
              />
            </Grid>
            <Grid item=true xs=Grid.V6>
              <div
                id="main-display"
                style=(
                  ReactDOMRe.Style.make(
                    ~marginBottom="24px",
                    ~minHeight="480px",
                    ~position="relative",
                    (),
                  )
                )>
                <MediaProvider
                  audioCtx
                  audioGraph=self.state.audioGraph
                  getAudio=(getAnalysisInput(audioCtx, self.state))
                  onSetRef=(
                    (layer, theRef) =>
                      self.handle(setLayerRef(audioCtx), (layer, theRef))
                  )
                  rootWidth=self.state.params.width
                  rootHeight=self.state.params.height
                  millisPerAudioTick=16
                  saveTick=(
                    (key, tickFn) =>
                      self.state.tickFunctions :=
                        Belt.Map.String.set(
                          self.state.tickFunctions^,
                          key,
                          tickFn,
                        )
                  )
                  layers=(sortLayers(self.state.params.layers))
                />
                <canvas
                  ref=(self.handle(setCanvasRef))
                  width=(Js.Int.toString(self.state.params.width))
                  height=(Js.Int.toString(self.state.params.height))
                  style=(
                    ReactDOMRe.Style.make(
                      ~imageRendering="crisp-edges",
                      ~transform=
                        "scale("
                        ++ Js.Float.toString(
                             480.0 /. float_of_int(self.state.params.height),
                           )
                        ++ ")",
                      ~transformOrigin="top left",
                      (),
                    )
                  )
                />
              </div>
              /* <Button */
              /*   style=( */
              /*     ReactDOMRe.Style.make( */
              /*       ~position="absolute", */
              /*       ~right="0px", */
              /*       ~bottom="0px", */
              /*       (), */
              /*     ) */
              /*   ) */
              /*   variant=`Contained */
              /*   onClick=(evt => self.send(ToggleFullscreen))> */
              /*   ( */
              /*     self.state.fullscreenCanvas ? */
              /*       <MaterialUIIcons.FullscreenExit /> : */
              /*       <MaterialUIIcons.Fullscreen /> */
              /*   ) */
              /*   (ReasonReact.string("Fullscreen")) */
              /* </Button> */
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
