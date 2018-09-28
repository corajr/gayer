open Canvas;
open Layer;

let renderLayerPreview =
    (~layer, ~changeLayer, ~setRef, ~saveTick, ~onUnmount, ~layerRefs) => {
  let layerKey = getLayerKey(layer);
  let savePreviewRef = aRef =>
    switch (Js.Nullable.toOption(aRef)) {
    | Some(previewCanvas) =>
      saveTick(onUnmount, layerKey ++ "preview", _t =>
        switch (Belt.Map.String.get(layerRefs^, layerKey)) {
        | Some(layer) =>
          let ctx = getContext(getFromReact(previewCanvas));
          let src = getElementAsImageSource(layer);
          Ctx.setFillStyle(ctx, "black");
          Ctx.fillRect(ctx, 0, 0, 120, 120);
          Ctx.drawImageDestRect(ctx, src, 0, 0, 120, 120);
        | _ => ()
        }
      )
    | None => ()
    };
  <div style=(ReactDOMRe.Style.make(~display="flex", ()))>
    (
      switch (layer.content) {
      | HandDrawn
      | Webcam
      | Slitscan(_)
      | Image(_)
      | Video(_)
      | Analysis(_)
      | MIDIKeyboard
      | KeycodeReader(_)
      | KeycodeWriter(_)
      | RawAudioWriter(_)
      | RawAudioReader(_)
      | Histogram
      | Draw(_)
      | Regl(_) =>
        <div> <canvas ref=savePreviewRef width="120" height="120" /> </div>
      | Fill(_)
      | DrawGlobal(_)
      | PitchClasses(_)
      | Text(_)
      | Reader(_) => ReasonReact.null
      }
    )
  </div>;
};

type state = {expanded: bool};
type action =
  | ToggleExpanded;

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~layer,
      ~layerKeys,
      ~layerRefs,
      ~onSetRef,
      ~saveTick,
      ~changeLayer,
      ~savedImages,
      ~width,
      ~height,
      _children,
    ) => {
  ...component,
  initialState: () => {expanded: false},
  reducer: (action, state) =>
    switch (action) {
    | ToggleExpanded =>
      ReasonReact.Update({...state, expanded: ! state.expanded})
    },
  render: self =>
    MaterialUi.(
      <Card>
        <CardHeader
          avatar=(icon_of_layerContent(layer.content))
          title={
            <Typography
              variant=`Subheading
              style=(ReactDOMRe.Style.make(~marginTop="-4px", ()))>
              (
                ReasonReact.string(
                  readable_string_type_of_layerContent(layer.content),
                )
              )
            </Typography>
          }
          action={
            <IconButton onClick=(_evt => changeLayer(layer, None))>
              <MaterialUIIcons.Delete />
            </IconButton>
          }
          style=(
            ReactDOMRe.Style.make(~paddingTop="8px", ~paddingBottom="0px", ())
          )
        />
        <div
          style=(
            ReactDOMRe.Style.make(
              ~display="flex",
              ~justifyContent="flex-start",
              (),
            )
          )>
          <CardMedia src="dummy">
            (
              renderLayerPreview(
                ~layer,
                ~saveTick,
                ~onUnmount=self.onUnmount,
                ~setRef=onSetRef(layer),
                ~changeLayer,
                ~layerRefs,
              )
            )
          </CardMedia>
          <CardContent style=(ReactDOMRe.Style.make(~flexGrow="1", ()))>
            (
              switch (layer.content) {
              | PitchClasses(xs) =>
                <div>
                  <PitchSetSelector
                    pitchSet=xs
                    onChangeSetting=(
                      newPitches =>
                        changeLayer(
                          layer,
                          Some({
                            ...layer,
                            content: PitchClasses(newPitches),
                          }),
                        )
                    )
                  />
                </div>
              | Reader(readerType) =>
                <div>
                  <ReaderType
                    readerType
                    onChangeSetting=(
                      newReaderType =>
                        changeLayer(
                          layer,
                          Some({...layer, content: Reader(newReaderType)}),
                        )
                    )
                  />
                </div>
              | Fill(v) =>
                <MaterialUi.TextField
                  label=(ReasonReact.string("style"))
                  value=(`String(v))
                  onChange=(
                    evt => {
                      let value = ReactDOMRe.domElementToObj(
                                    ReactEventRe.Form.target(evt),
                                  )##value;
                      changeLayer(
                        layer,
                        Some({...layer, content: Fill(value)}),
                      );
                    }
                  )
                  margin=`Normal
                />
              | Draw(cmds) => <div />
              | DrawGlobal(cmds) => <div />
              | Slitscan(opts) =>
                <div>
                  <LayerSelect
                    layerKeys
                    currentValue=opts.sourceLayerKey
                    onChange=(
                      newKey =>
                        changeLayer(
                          layer,
                          Some({
                            ...layer,
                            content:
                              Slitscan({...opts, sourceLayerKey: newKey}),
                          }),
                        )
                    )
                  />
                </div>
              | Image(url) =>
                <ImageSelect
                  url
                  savedImages
                  onChange=(
                    newUrl =>
                      changeLayer(
                        layer,
                        Some({...layer, content: Image(newUrl)}),
                      )
                  )
                />
              | Video(url) => <div />
              | RawAudioWriter(rawAudioFormat)
              | RawAudioReader(rawAudioFormat) => <div />
              | Analysis(analysisOptions) =>
                AnalysisOptions.(
                  <div>
                    <StringEncodeSelect
                      allSettings=[|
                        AnalysisOptions.Slit,
                        AnalysisOptions.History({w: Width, h: Height}),
                        AnalysisOptions.CircularBuffer({w: Width, h: Height}),
                      |]
                      currentSetting=analysisOptions.analysisSize
                      encoder=string_of_analysisSize
                      decoder=analysisSize_of_string
                      onChange=(
                        newSetting =>
                          changeLayer(
                            layer,
                            Some({
                              ...layer,
                              content:
                                Analysis({
                                  ...analysisOptions,
                                  analysisSize: newSetting,
                                }),
                            }),
                          )
                      )
                    />
                  </div>
                )
              /* <ReaderType */
              /*   readerType=analysisOptions.readerType */
              /*   onChangeSetting=( */
              /*     newReaderType => */
              /*       changeLayer( */
              /*         layer, */
              /*         Some({ */
              /*           ...layer, */
              /*           content: */
              /*             Analysis({ */
              /*               ...analysisOptions, */
              /*               readerType: newReaderType, */
              /*             }), */
              /*         }), */
              /*       ) */
              /*   ) */
              /* /> */
              | Regl(reglOptions) =>
                <div>
                  (
                    switch (reglOptions) {
                    | Sobel(opts) =>
                      <LayerSelect
                        layerKeys
                        currentValue=opts.sourceLayer
                        onChange=(
                          newKey =>
                            changeLayer(
                              layer,
                              Some({
                                ...layer,
                                content:
                                  Regl(
                                    Sobel({...opts, sourceLayer: newKey}),
                                  ),
                              }),
                            )
                        )
                      />
                    | Displacement(opts) =>
                      <div>
                        <LayerSelect
                          layerKeys
                          currentValue=opts.displacementSourceLayer
                          onChange=(
                            newKey =>
                              changeLayer(
                                layer,
                                Some({
                                  ...layer,
                                  content:
                                    Regl(
                                      Displacement({
                                        ...opts,
                                        displacementSourceLayer: newKey,
                                      }),
                                    ),
                                }),
                              )
                          )
                        />
                        <LayerSelect
                          layerKeys
                          currentValue=opts.displacementMap
                          onChange=(
                            newKey =>
                              changeLayer(
                                layer,
                                Some({
                                  ...layer,
                                  content:
                                    Regl(
                                      Displacement({
                                        ...opts,
                                        displacementMap: newKey,
                                      }),
                                    ),
                                }),
                              )
                          )
                        />
                      </div>
                    }
                  )
                </div>
              | KeycodeReader(fmt) =>
                <div>
                  <Typography color=`TextSecondary>
                    (
                      ReasonReact.string(
                        "If keys are stuck, press SPACE to clear.",
                      )
                    )
                  </Typography>
                </div>
              /* <KeycodeFormatSelect */
              /*   currentSetting=fmt */
              /*   onChange=( */
              /*     newFmt => */
              /*       changeLayer( */
              /*         layer, */
              /*         Some({...layer, content: KeycodeReader(newFmt)}), */
              /*       ) */
              /*   ) */
              /* /> */
              | KeycodeWriter(fmt) =>
                <div>
                  <Typography color=`TextSecondary>
                    (
                      ReasonReact.string(
                        "If keys are stuck, press SPACE to clear.",
                      )
                    )
                  </Typography>
                </div>
              /* <KeycodeFormatSelect */
              /*   currentSetting=fmt */
              /*   onChange=( */
              /*     newFmt => */
              /*       changeLayer( */
              /*         layer, */
              /*         Some({...layer, content: KeycodeWriter(newFmt)}), */
              /*       ) */
              /*   ) */
              /* /> */
              | Text(s) =>
                <TextField
                  fullWidth=true
                  multiline=true
                  value=(`String(s))
                  onChange=(
                    evt => {
                      let value = ReactDOMRe.domElementToObj(
                                    ReactEventRe.Form.target(evt),
                                  )##value;
                      changeLayer(
                        layer,
                        Some({...layer, content: Text(value)}),
                      );
                    }
                  )
                />
              | HandDrawn
              | Webcam
              | MIDIKeyboard
              | Histogram =>
                <MaterialUi.Typography color=`TextSecondary>
                  (ReasonReact.string("[no options]"))
                </MaterialUi.Typography>
              | _ =>
                <MaterialUi.Typography>
                  (
                    ReasonReact.string(
                      Js.Json.stringifyWithSpace(
                        EncodeLayer.layerContent(layer.content),
                        2,
                      ),
                    )
                  )
                </MaterialUi.Typography>
              }
            )
          </CardContent>
          (
            switch (layer.content) {
            | Text(_) => ReasonReact.null
            | _ =>
              <div
                style=(
                  ReactDOMRe.Style.make(
                    ~flexDirection="column",
                    ~alignItems="flex-start",
                    ~marginRight="24px",
                    ~marginBottom="24px",
                    (),
                  )
                )>
                <FloatSlider
                  value=layer.alpha
                  label="Alpha"
                  onChange=(
                    value =>
                      changeLayer(layer, Some({...layer, alpha: value}))
                  )
                />
                /* <FloatSlider */
                /*   min=((-0.5) *. tau) */
                /*   max=(0.5 *. tau) */
                /*   value=layer.rotation */
                /*   label="Rotation" */
                /*   step=0.01 */
                /*   onChange=( */
                /*     value => changeLayer(layer, {...layer, rotation: value}) */
                /*   ) */
                /* /> */
                <FormGroup row=true>
                  <CompositeOperationSelect
                    compositeOperation=layer.compositeOperation
                    onChange=(
                      newOperation =>
                        changeLayer(
                          layer,
                          Some({...layer, compositeOperation: newOperation}),
                        )
                    )
                  />
                </FormGroup>
                <FormGroup row=true>
                  <div style=(ReactDOMRe.Style.make(~flexGrow="1", ())) />
                  <IconButton
                    style=(ReactDOMRe.Style.make(~marginRight="-16px", ()))
                    onClick=(
                      self.handle((_evt, {ReasonReact.send}) =>
                        send(ToggleExpanded)
                      )
                    )>
                    (
                      self.state.expanded ?
                        <span
                          style=(
                            ReactDOMRe.Style.make(
                              ~transform="rotate(180deg)",
                              (),
                            )
                          )>
                          <MaterialUIIcons.ExpandMore />
                        </span> :
                        <MaterialUIIcons.ExpandMore />
                    )
                  </IconButton>
                </FormGroup>
              </div>
            }
          )
        </div>
        <Collapse in_=self.state.expanded>
          <CardContent
            style=(
              ReactDOMRe.Style.make(
                ~display="flex",
                ~flexDirection="row",
                ~justifyContent="space-between",
                (),
              )
            )>
            <FormControl component=(`String("fieldset"))>
              <FormGroup>
                <FormControlLabel
                  control={
                    <Switch
                      checked=(`Bool(layer.enabled))
                      onChange=(
                        (_evt, value) =>
                          changeLayer(
                            layer,
                            Some({...layer, enabled: value}),
                          )
                      )
                      value="enabled"
                    />
                  }
                  label=(ReasonReact.string("Active (include in render)"))
                />
              </FormGroup>
              <TransformMatrixSettings layer changeLayer />
              <NumericTextField
                label=(ReasonReact.string("Rotation (degrees)"))
                value=(`Float(radiansToDegrees(layer.rotation)))
                onChange=(
                  value =>
                    changeLayer(
                      layer,
                      Some({...layer, rotation: degreesToRadians(value)}),
                    )
                )
              />
            </FormControl>
            <FormControl
              component=(`String("fieldset"))
              style=(
                ReactDOMRe.Style.make(
                  ~display="flex",
                  ~justifyContent="flex-start",
                  (),
                )
              )>
              <MaterialUi.TextField
                label=(ReasonReact.string("filters"))
                value=(`String(layer.filters))
                onChange=(
                  evt => {
                    let value = ReactDOMRe.domElementToObj(
                                  ReactEventRe.Form.target(evt),
                                )##value;
                    changeLayer(layer, Some({...layer, filters: value}));
                  }
                )
                margin=`Normal
              />
            </FormControl>
          </CardContent>
        </Collapse>
      </Card>
    ),
};
