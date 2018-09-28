open Audio;
open AudioGraph;
open Music;

open Canvas;
open Fscreen;
open Layer;
open Params;
open Performance;
open Presets;
open Score;
open Timing;
open UserMedia;
open Video;

module RList = Rationale.RList;

type audioInputSetting = AudioInput.audioInputSetting;
type filterInput = audioNode;

type state = {
  animationStartTime: ref(float),
  animationLastUpdated: ref(float),
  readPos: ref(int),
  writePos: ref(int),
  freqFuncParams: ref((int, int)),
  filterInput: option(audioNode),
  params,
  score: option((score, ref(int))),
  presetDrawerOpen: bool,
  mediaStream: option(mediaStream),
  audioGraph: ref(audioGraph),
  micInput: option(audioNode),
  cameraInput: ref(option(canvasImageSource)),
  oscillatorBank: ref(option(bank(oscillator))),
  filterBanks: option(filterBanks),
  compressor: ref(option(compressor)),
  merger: ref(option(channelMerger)),
  currentFilterValues: ref(option(filterValues)),
  layerRefs: ref(Belt.Map.String.t(Dom.element)),
  savedImages: Belt.Map.String.t(string),
  loadedAudio: ref(Belt.Map.String.t(audioNode)),
  canvasRef: ref(option(Dom.element)),
  drawContext: DrawCommand.drawContext,
  fullscreenCanvas: bool,
  tickFunctions: ref(Belt.Map.String.t(float => unit)),
  tickCounter: ref(int),
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
  score: Some((exampleScore, ref(0))),
  params: snd(List.nth(presets, 0)),
  oscillatorBank: ref(None),
  filterBanks: None,
  currentFilterValues: ref(None),
  compressor: ref(None),
  merger: ref(None),
  savedImages: Belt.Map.String.empty,
  layerRefs: ref(Belt.Map.String.empty),
  loadedAudio: ref(Belt.Map.String.empty),
  canvasRef: ref(None),
  drawContext: {
    maybeCtxRef: ref(None),
    width: 1,
    height: 1,
    variables: Belt.Map.String.empty,
  },
  fullscreenCanvas: false,
  tickCounter: ref(0),
  tickFunctions: ref(Belt.Map.String.empty),
  timerId: ref(None),
};

type action =
  | Clear
  | Tick
  | TogglePresetDrawer
  | SaveImage
  | ToggleFullscreen
  | SetScoreEventIndex(int)
  | AdjustScoreEventIndex(int)
  | AddSavedImage(string)
  | SetFilterInput(audioNode)
  | SetMicInput(audioNode)
  | SetMediaStream(mediaStream)
  | SetFilterBanks(filterBanks)
  | ChangeLayer(layer, option(layer))
  | SetLayers(list(layer))
  | SetParams(params);

[@bs.val] external decodeURIComponent : string => string = "";
[@bs.val] external encodeURIComponent : string => string = "";

let setCanvasRef = (theRef, {ReasonReact.state}) => {
  let maybeRef = Js.Nullable.toOption(theRef);
  state.canvasRef := maybeRef;
  switch (maybeRef) {
  | None => ()
  | Some(aRef) =>
    state.layerRefs := Belt.Map.String.set(state.layerRefs^, "root", aRef);
    state.drawContext.maybeCtxRef := Some(getContext(getFromReact(aRef)));
  };
};

let setLayerRef = (audioCtx, (layer, theRef), {ReasonReact.state}) => {
  let maybeRef = Js.Nullable.toOption(theRef);
  let layerKey = getLayerKey(layer);

  switch (maybeRef) {
  | None => ()
  | Some(aRef) =>
    state.layerRefs := Belt.Map.String.set(state.layerRefs^, layerKey, aRef)
  };

  switch (layer.content, maybeRef) {
  | (Webcam, Some(aRef)) =>
    switch (state.mediaStream) {
    | Some(stream) =>
      let video = attachVideoStream(aRef, stream);
      state.cameraInput := Some(video);
      state.layerRefs := Belt.Map.String.set(state.layerRefs^, "webcam", aRef);
    | None => ()
    }
  | _ => ()
  };
};

let changeLayer =
    (oldLayer: layer, maybeNewLayer: option(layer), layers: list(layer)) =>
  switch (RList.indexOf(oldLayer, layers)) {
  | None => layers
  | Some(index) =>
    switch (maybeNewLayer) {
    | Some(newLayer) => RList.update(newLayer, index, layers)
    | None => RList.remove(index, 1, layers)
    }
  };

[%mui.withStyles
  "SizedDrawer"({
    paper: ReactDOMRe.Style.make(~position="relative", ~width="240px", ()),
  })
];

[%mui.withStyles
  "GrowTitle"({grow: ReactDOMRe.Style.make(~flexGrow="1", ())})
];

let component = ReasonReact.reducerComponent("App");

let maybeUpdateCanvas:
  (ref(option(Dom.element)), canvasElement => unit) => unit =
  (maybeEl, f) =>
    switch (maybeEl^) {
    | None => ()
    | Some(canvas) => f(getFromReact(canvas))
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

let getReadAndWritePos = (f: (int, int) => unit, {ReasonReact.state}) : unit => {
  let width = state.params.width;

  let xToRead =
    wrapCoord(state.readPos^ + state.params.readPosOffset, 0, width);
  let xToWrite =
    wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width);
  f(xToRead, xToWrite);
};

let drawLayer: (ctx, int, int, state, layer) => unit =
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

    let layerKey = getLayerKey(layer);
    switch (Belt.Map.String.get(state.tickFunctions^, layerKey)) {
    | None => ()
    | Some(f) => f(float_of_int(state.tickCounter^))
    };

    let maybeLayerRef = Belt.Map.String.get(state.layerRefs^, layerKey);

    mark(performance, layerKey ++ "start");

    switch (layer.content) {
    | DrawGlobal(cmds) => DrawCommand.drawCommands(state.drawContext, cmds)
    | Fill(s) =>
      Ctx.setFillStyle(ctx, s);
      Ctx.fillRect(ctx, 0, 0, width, height);
    | Analysis({analysisSize}) =>
      switch (maybeLayerRef) {
      | None => ()
      | Some(analysisCanvas) =>
        let canvasElt = getFromReact(analysisCanvas);
        let canvasAsSource = getCanvasAsSource(canvasElt);
        DrawCommand.(
          switch (analysisSize) {
          | WithHistory(_) =>
            Ctx.drawImageDestRect(ctx, canvasAsSource, 0, 0, width, height)
          | DestRect({x, y}) =>
            let analysisX = getLength(state.drawContext, x);
            let analysisY = getLength(state.drawContext, y);
            /* let x = */
            /* wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width); */
            Ctx.drawImage(ctx, canvasAsSource, analysisX, analysisY);
          }
        );
      }
    | MIDIKeyboard =>
      switch (maybeLayerRef) {
      | None => ()
      | Some(midiCanvas) =>
        let canvasElt = getFromReact(midiCanvas);
        let canvasAsSource = getCanvasAsSource(canvasElt);
        Ctx.drawImageDestRect(ctx, canvasAsSource, 0, 0, width, height);
      /* let x = */
      /* wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width); */
      /* Ctx.drawImageDestRect(ctx, canvasAsSource, x, 0, 1, height); */
      }
    | RawAudioWriter({x, y, w, h, encoding}) =>
      switch (encoding, maybeLayerRef) {
      | (Int8(_), Some(canvas)) =>
        let canvasSource = getCanvasAsSource(getFromReact(canvas));
        Ctx.drawImageDestRect(ctx, canvasSource, x, y, w, h);
      | _ => ()
      }
    | RawAudioReader(_) => ()
    | Regl(_) =>
      switch (maybeLayerRef) {
      | None => ()
      | Some(canvas) =>
        let canvasSource = getCanvasAsSource(getFromReact(canvas));
        Ctx.drawImage(ctx, canvasSource, 0, 0);
      }
    | HandDrawn
    | Text(_)
    | Draw(_)
    | Slitscan(_)
    | Image(_)
    | Video(_) =>
      switch (maybeLayerRef) {
      | None => ()
      | Some(aRef) =>
        let img = getElementAsImageSource(aRef);
        Ctx.drawImageDestRect(ctx, img, 0, 0, width, height);
      }
    | Webcam =>
      switch (state.cameraInput^) {
      | None => ()
      | Some(input) => Ctx.drawImageDestRect(ctx, input, 0, 0, width, height)
      }
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
    | KeycodeReader(_)
    | KeycodeWriter(_) =>
      switch (maybeLayerRef) {
      | None => ()
      | Some(canvasEl) =>
        let canvasAsSource = getCanvasAsSource(getFromReact(canvasEl));
        Ctx.drawImageDestRect(ctx, canvasAsSource, 0, 0, width, height);
      }
    | Histogram =>
      switch (maybeLayerRef) {
      | None => ()
      | Some(histogram) =>
        let xToWrite =
          wrapCoord(state.writePos^ + state.params.writePosOffset, 0, width);
        let canvasElt = getFromReact(histogram);
        let canvasAsSource = getCanvasAsSource(canvasElt);
        Ctx.drawImageDestRect(ctx, canvasAsSource, xToWrite, 0, 1, height);
      }
    | Reader(readerType) =>
      let xToRead =
        wrapCoord(state.readPos^ + state.params.readPosOffset, 0, width);
      let slice = Ctx.getImageData(ctx, xToRead, 0, 1, height);
      ImageDataUtil.updateFilterValuesFromImageData(
        slice,
        readerType,
        state.currentFilterValues,
      );

      /* let slice = */
      /*   Ctx.getImageData(ctx, width / 2, height / 2, height / 10, 10); */
      /* Ctx.strokeRect(ctx, width / 2, height / 2, height / 10, 10); */
      switch (readerType) {
      | Channel(channel) =>
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
      | Saturation =>
        Ctx.setFillStyle(ctx, rgba(255, 255, 255, 0.5));
        Ctx.fillRect(ctx, xToRead, 0, 1, height);
      };
    };

    mark(performance, layerKey ++ "end");
    measure(performance, layerKey, layerKey ++ "start", layerKey ++ "end");

    ignore(
      Js.Global.setTimeout(
        () =>
          switch (
            Belt.Map.String.get(state.tickFunctions^, layerKey ++ "preview")
          ) {
          | None => ()
          | Some(f) => f(0.0)
          },
        0,
      ),
    );
  };

let drawCanvas = (canvasElement, width, height, state) => {
  mark(performance, "rootCanvas-start");

  if (state.params.shouldClear) {
    clearCanvas(canvasElement, width, height);
  };
  let ctx = getContext(canvasElement);

  List.iter(
    layer =>
      if (layer.enabled) {
        drawLayer(ctx, width, height, state, layer);
      },
    state.params.layers,
  );

  mark(performance, "rootCanvas-end");
  measure(performance, "rootCanvas", "rootCanvas-start", "rootCanvas-end");

  /* let entry = getEntriesByName(performance, "rootCanvas")[0]; */
  /* if (state.tickCounter^ mod 60 == 0) { */
  /*   Js.log(Js.Float.toString(entry |. durationGet) ++ "ms"); */
  /* }; */

  clearMarks(performance);
  clearMeasures(performance);
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
    | Oscillator(_) => (
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

    state.currentFilterValues :=
      Some(
        Stereo(
          Array.make(state.params.height, 0.0),
          Array.make(state.params.height, 0.0),
        ),
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

    state.currentFilterValues :=
      Some(Mono(Array.make(state.params.height, 0.0)));
    send(SetFilterBanks(MonoBank(filterBank)));
  };
};

let updateBank = (state, values, filterBank) =>
  updateFilterBank(
    ~filterBank,
    ~filterValues=values,
    ~inputGain=state.params.inputGain,
    ~outputGain=state.params.outputGain,
  );

let updateFilterBanks = ({ReasonReact.state}) =>
  switch (state.filterBanks) {
  | None => ()
  | Some(MonoBank(filterBank)) =>
    switch (state.currentFilterValues^) {
    | Some(Mono(filterValues)) =>
      updateBank(state, filterValues, filterBank)
    | Some(Stereo(filterValuesL, _)) =>
      updateBank(state, filterValuesL, filterBank)
    | None => ()
    }
  | Some(StereoBanks(filterBankL, filterBankR)) =>
    switch (state.currentFilterValues^) {
    | Some(Mono(filterValues)) =>
      updateBank(state, filterValues, filterBankL);
      updateBank(state, filterValues, filterBankR);
    | Some(Stereo(filterValuesL, filterValuesR)) =>
      updateBank(state, filterValuesL, filterBankL);
      updateBank(state, filterValuesR, filterBankR);
    | None => ()
    }
  };

let saveTick = ({ReasonReact.state}, onUnmount, key, tickFn) => {
  state.tickFunctions :=
    Belt.Map.String.set(state.tickFunctions^, key, tickFn);
  onUnmount(() =>
    state.tickFunctions := Belt.Map.String.remove(state.tickFunctions^, key)
  );
};

let make = (~audioCtx=makeDefaultAudioCtx(), _children) => {
  ...component,
  initialState: () => defaultState,
  reducer: (action, state) =>
    switch (action) {
    | AddSavedImage(url) =>
      let timestamp = [%bs.raw "new Date().toISOString()"];
      ReasonReact.Update({
        ...state,
        savedImages: Belt.Map.String.set(state.savedImages, timestamp, url),
      });
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
    | SetScoreEventIndex(i) => ReasonReact.NoUpdate
    | AdjustScoreEventIndex(i) =>
      ReasonReact.SideEffects(
        (
          _self =>
            switch (state.score) {
            | Some(({events}, eventIndexRef)) =>
              eventIndexRef :=
                clamp(
                  ~minVal=0,
                  ~maxVal=Array.length(events) - 1,
                  eventIndexRef^ + i,
                );
              let eventIndex = eventIndexRef^;

              pushParamsState(events[eventIndex].params);
              Js.log({j|Adjusting score index; score now at $(eventIndex)|j});
            | None => ()
            }
        ),
      )
    | ToggleFullscreen =>
      ReasonReact.Update({
        ...state,
        fullscreenCanvas: ! state.fullscreenCanvas,
      })
    | SetParams(params) =>
      ReasonReact.Update({
        ...state,
        params,
        drawContext: {
          ...state.drawContext,
          width: params.width,
          height: params.height,
        },
      })
    | SetMicInput(mic) => ReasonReact.Update({...state, micInput: Some(mic)})
    | SetMediaStream(stream) =>
      ReasonReact.Update({...state, mediaStream: Some(stream)})
    | SetFilterInput(filterInput) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, filterInput: Some(filterInput)},
        (self => connectInputs(self.state)),
      )
    | ChangeLayer(oldLayer, maybeNewLayer) =>
      ReasonReact.SideEffects(
        (
          self =>
            pushParamsState({
              ...self.state.params,
              layers:
                changeLayer(
                  oldLayer,
                  maybeNewLayer,
                  self.state.params.layers,
                ),
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
            state.tickCounter := state.tickCounter^ + 1;
            state.readPos :=
              wrapCoord(
                state.readPos^,
                state.params.readPosDelta,
                state.params.width,
              );

            state.writePos :=
              wrapCoord(
                state.writePos^,
                state.params.writePosDelta,
                state.params.width,
              );

            maybeUpdateCanvas(
              state.canvasRef,
              canvas => {
                drawCanvas(
                  canvas,
                  state.params.width,
                  state.params.height,
                  state,
                );

                updateFilterBanks(self);

                switch (state.oscillatorBank^) {
                | Some(bank) =>
                  switch (state.currentFilterValues^) {
                  | Some(Mono(filterValues)) =>
                    updateBankGains(~bank, ~gainValues=filterValues)
                  | Some(Stereo(filterValuesL, _)) =>
                    updateBankGains(~bank, ~gainValues=filterValuesL)
                  | None => ()
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

    /* NOTE: requestAnimationFrame cannot be used in the audio path, because we
       need a more consistent tick as provided by Js.Global.setInterval (wrapped in
       the `setTimer` function). */
    /* let rec animationFn = timestamp => { */
    /* let lastUpdated = self.state.animationLastUpdated^; */
    /* let timeSinceLastUpdate = timestamp -. lastUpdated; */

    /* self.state.animationLastUpdated := timestamp; */

    /* Time since beginning; Don't need to know yet, but maybe we would? */

    /* if (self.state.animationStartTime^ === 0.0) { */
    /*   self.state.animationStartTime := timestamp; */
    /* }; */
    /* let timeSinceBeginning = timestamp -. self.state.animationStartTime^; */

    /* Js.log(timeSinceLastUpdate); */

    /*   self.send(Tick); */
    /*   requestAnimationFrame(window, animationFn); */
    /* }; */

    /* requestAnimationFrame(window, animationFn); */

    setTimer(
      self.state.timerId,
      () => self.send(Tick),
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
      newSelf.state.currentFilterValues :=
        Some(Mono(Array.make(newSelf.state.params.height, 0.0)));
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
        () => newSelf.send(Tick),
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
                color=`Inherit
                onClick=(_evt => self.send(TogglePresetDrawer))>
                <MaterialUIIcons.Menu />
              </IconButton>
              <GrowTitle
                render=(
                  classes =>
                    <Typography
                      variant=`Title
                      color=`Inherit
                      classes=[Title(classes.grow)]>
                      (ReasonReact.string("GAYER"))
                    </Typography>
                )
              />
              (
                Belt.Option.isSome(self.state.score) ?
                  <div>
                    <IconButton
                      color=`Inherit
                      onClick=(_evt => self.send(AdjustScoreEventIndex(-1)))>
                      <MaterialUIIcons.SkipPrevious />
                    </IconButton>
                    <IconButton
                      color=`Inherit
                      onClick=(_evt => self.send(AdjustScoreEventIndex(1)))>
                      <MaterialUIIcons.SkipNext />
                    </IconButton>
                  </div> :
                  ReasonReact.null
              )
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
                    (oldLayer, maybeNewLayer) =>
                      self.send(ChangeLayer(oldLayer, maybeNewLayer))
                  )
                  onSetRef=(
                    (layer, theRef) =>
                      self.handle(setLayerRef(audioCtx), (layer, theRef))
                  )
                  layerRefs=self.state.layerRefs
                  onSetParams=(newParams => pushParamsState(newParams))
                  saveTick=(saveTick(self))
                  savedImages=self.state.savedImages
                />
              </Grid>
              <Grid item=true xs=Grid.V4>
                <div
                  id="main-display"
                  style=(
                    ReactDOMRe.Style.make(
                      ~marginBottom="24px",
                      ~minHeight="400px",
                      ~position="fixed",
                      (),
                    )
                  )>
                  <MediaProvider
                    audioCtx
                    audioGraph=self.state.audioGraph
                    globalDrawContext=self.state.drawContext
                    getAudio=(getAnalysisInput(audioCtx, self.state))
                    onSetRef=(
                      (layer, theRef) =>
                        self.handle(setLayerRef(audioCtx), (layer, theRef))
                    )
                    layerRefs=self.state.layerRefs
                    currentFilterValues=self.state.currentFilterValues
                    rootWidth=self.state.params.width
                    rootHeight=self.state.params.height
                    millisPerAudioTick=16
                    getReadAndWritePos=(self.handle(getReadAndWritePos))
                    saveTick=(saveTick(self))
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
                               400.0 /. float_of_int(self.state.params.height),
                             )
                          ++ ")",
                        ~transformOrigin="top left",
                        (),
                      )
                    )
                  />
                </div>
              </Grid>
              <Grid item=true xs=Grid.V2>
                <div style=(ReactDOMRe.Style.make(~marginBottom="24px", ()))>
                  <Button
                    variant=`Contained
                    onClick=(_evt => self.send(SaveImage))
                    style=(ReactDOMRe.Style.make(~position="fixed", ()))>
                    <MaterialUIIcons.PhotoCamera />
                    (ReasonReact.string("Snapshot"))
                  </Button>
                </div>
                <div style=(ReactDOMRe.Style.make(~marginTop="24px", ()))>
                  (
                    self.state.savedImages
                    |> Belt.Map.String.toArray
                    |> Array.map(((key, url)) =>
                         <img key width="100%" src=url />
                       )
                    |> ReasonReact.array
                  )
                </div>
              </Grid>
            </Grid>
          </div>
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
    ),
};
