open AnalysisOptions;
open Audio.AudioInput;
open CameraOptions;
open Canvas;
open MIDICanvas;
open Music;
open RawAudio;
open ReaderType;
open Regl;

type layerContent =
  | Fill(string)
  | Draw(list(DrawCommand.command))
  | DrawGlobal(list(DrawCommand.command))
  | HandDrawn
  | Webcam
  | Slitscan(cameraOptions)
  | Image(string)
  | Video(string)
  | Analysis(analysisOptions)
  | PitchClasses(PitchSet.t)
  | MIDIKeyboard
  | KeycodeReader
  | KeycodeWriter
  | RawAudioWriter(rawAudioFormat)
  | RawAudioReader(rawAudioFormat)
  | Histogram
  | Regl(reglOptions)
  | Reader(readerType);

let readable_string_type_of_layerContent =
  fun
  | Fill(_) => "Fill"
  | Draw(_) => "Draw Commands"
  | DrawGlobal(_) => "Draw Commands (global)"
  | HandDrawn => "Mouse"
  | Webcam => "Webcam"
  | Slitscan(_) => "Slitscan"
  | Image(_) => "Image"
  | Video(_) => "Video"
  | Analysis(_) => "Analyzer"
  | PitchClasses(_) => "Pitch classes"
  | MIDIKeyboard => "MIDI Input"
  | KeycodeReader => "ASCII Reader"
  | KeycodeWriter => "ASCII Writer"
  | RawAudioWriter(_) => "Raw Audio Writer"
  | RawAudioReader(_) => "Raw Audio Reader"
  | Histogram => "Histogram"
  | Regl(_) => "Shader"
  | Reader(_) => "Reader";

let string_type_of_layerContent =
  fun
  | Fill(_) => "fill"
  | Draw(_) => "draw"
  | DrawGlobal(_) => "draw-global"
  | HandDrawn => "mouse"
  | Webcam => "webcam"
  | Slitscan(_) => "slitscan"
  | Image(_) => "image"
  | Video(_) => "video"
  | Analysis(_) => "analyzer"
  | PitchClasses(_) => "pitch-classes"
  | MIDIKeyboard => "midi-keyboard"
  | KeycodeReader => "keycode-reader"
  | KeycodeWriter => "keycode-writer"
  | RawAudioWriter(_) => "raw-audio-writer"
  | RawAudioReader(_) => "raw-audio-reader"
  | Histogram => "histogram"
  | Regl(_) => "shader"
  | Reader(_) => "reader";

let icon_of_layerContent =
  MaterialUIIcons.(
    fun
    | Fill(_) => <FormatPaint />
    | Draw(_) => <FormatListBulleted />
    | DrawGlobal(_) => <FormatListBulleted />
    | HandDrawn => <Brush />
    | Webcam => <Videocam />
    | Slitscan(_) => <Flip />
    | Image(_) => <Image />
    | Video(_) => <Movie />
    | Analysis(_) => <Mic />
    | PitchClasses(_) => <Tonality />
    | MIDIKeyboard => <MusicNote />
    | KeycodeReader => <Textsms />
    | KeycodeWriter => <Keyboard />
    | RawAudioWriter(_) => <Voicemail />
    | RawAudioReader(_) => <Voicemail />
    | Histogram => <ShowChart />
    | Regl(_) => <Filter />
    | Reader(_) => <Speaker />
  );

type layer = {
  content: layerContent,
  enabled: bool,
  alpha: float,
  compositeOperation,
  rotation,
  transformMatrix,
  filters: string,
  id: option(string),
};

let defaultLayer = {
  content: Fill("black"),
  enabled: true,
  alpha: 1.0,
  compositeOperation: SourceOver,
  transformMatrix: defaultTransform,
  rotation: 0.0,
  filters: "none",
  id: None,
};

let oneCompleteTurnAfterNTicks: int => rotation = n => tau /. float_of_int(n);

module DecodeLayer = {
  let transformMatrix = json =>
    Json.Decode.(
      switch (json |> list(float)) {
      | [a, b, c, d, e, f] => {
          horizontalScaling: a,
          horizontalSkewing: b,
          verticalSkewing: c,
          verticalScaling: d,
          horizontalMoving: e,
          verticalMoving: f,
        }
      | _ => defaultTransform
      }
    );

  let rotation = json => Json.Decode.(json |> float);

  let rawAudioEncodingByType = (type_, json) =>
    Json.Decode.(
      switch (type_) {
      | "float" => Float
      | "int8" =>
        json |> map(c => Int8(channel_of_int(c)), field("channel", int))
      | _ => Int8(R)
      }
    );

  let rawAudioEncoding = json =>
    Json.Decode.(
      json |> (field("type", string) |> andThen(rawAudioEncodingByType))
    );

  let rawAudioFormat = json =>
    Json.Decode.{
      x: json |> field("x", int),
      y: json |> field("y", int),
      w: json |> field("w", int),
      h: json |> field("h", int),
      encoding: json |> field("encoding", rawAudioEncoding),
      sampleRate: json |> field("sampleRate", int),
    };

  let layerByType = (type_, json) =>
    Json.Decode.(
      switch (type_) {
      | "midi-keyboard" => MIDIKeyboard
      | "keycode-writer" => KeycodeWriter
      | "keycode-reader" => KeycodeReader
      | "hand-drawn" => HandDrawn
      | "webcam" => Webcam
      | "regl" =>
        json
        |> map(
             o => Regl(o),
             field("options", DecodeReglOptions.reglOptions),
           )

      | "slitscan" =>
        json
        |> map(
             s => Slitscan(s),
             field("options", DecodeCameraOptions.cameraOptions),
           )
      | "image" => json |> map(s => Image(s), field("url", string))
      | "video" => json |> map(s => Video(s), field("url", string))
      | "raw-audio-writer" =>
        json |> map(o => RawAudioWriter(o), field("format", rawAudioFormat))
      | "raw-audio-reader" =>
        json |> map(o => RawAudioReader(o), field("format", rawAudioFormat))
      | "histogram" => Histogram
      | "reader" =>
        json
        |> map(
             t => Reader(t),
             field("readerType", DecodeReaderType.readerType),
           )
      | "analysis" =>
        json
        |> map(
             o => Analysis(o),
             field("opts", DecodeAnalysisOptions.analysisOptions),
           )

      | "fill" => json |> map(s => Fill(s), field("style", string))
      | "draw" =>
        json
        |> map(
             xs => Draw(xs),
             field("cmds", list(DrawCommand.DecodeDrawCommand.command)),
           )
      | "draw-global" =>
        json
        |> map(
             xs => DrawGlobal(xs),
             field("cmds", list(DrawCommand.DecodeDrawCommand.command)),
           )
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
      id: json |> field("id", optional(string)),
      content: json |> field("content", layerContent),
      enabled: json |> field("enabled", bool),
      alpha: json |> field("alpha", float),
      compositeOperation:
        json
        |> map(
             compositeOperation_of_string,
             field("compositeOperation", string),
           ),
      transformMatrix: json |> field("transformMatrix", transformMatrix),
      filters: json |> field("filters", string),
      rotation: json |> field("rotation", rotation),
    };
};

module EncodeLayer = {
  let transformMatrix =
      (
        {
          horizontalScaling,
          horizontalSkewing,
          verticalSkewing,
          verticalScaling,
          horizontalMoving,
          verticalMoving,
        },
      ) =>
    Json.Encode.(
      list(
        float,
        [
          horizontalScaling,
          horizontalSkewing,
          verticalSkewing,
          verticalScaling,
          horizontalMoving,
          verticalMoving,
        ],
      )
    );

  let rawAudioEncoding =
    Json.Encode.(
      fun
      | Float => object_([("type", string("float"))])
      | Int8(channel) =>
        object_([
          ("type", string("int8")),
          ("channel", int(int_of_channel(channel))),
        ])
    );
  let rawAudioFormat = r =>
    Json.Encode.(
      object_([
        ("x", int(r.x)),
        ("y", int(r.y)),
        ("w", int(r.w)),
        ("h", int(r.h)),
        ("encoding", rawAudioEncoding(r.encoding)),
        ("sampleRate", int(r.sampleRate)),
      ])
    );

  let layerContent = r =>
    Json.Encode.(
      switch (r) {
      | Webcam => object_([("type", string("webcam"))])
      | Slitscan(s) =>
        object_([
          ("type", string("slitscan")),
          ("options", EncodeCameraOptions.cameraOptions(s)),
        ])
      | Image(url) =>
        object_([("type", string("image")), ("url", string(url))])
      | Video(url) =>
        object_([("type", string("video")), ("url", string(url))])
      | Analysis(opts) =>
        object_([
          ("type", string("analysis")),
          ("opts", EncodeAnalysisOptions.analysisOptions(opts)),
        ])

      | PitchClasses(classes) =>
        object_([
          ("type", string("pitchClasses")),
          ("pc", list(int, PitchSet.elements(classes))),
        ])
      | Fill(style) =>
        object_([("type", string("fill")), ("style", string(style))])

      | Draw(cmds) =>
        object_([
          ("type", string("draw")),
          ("cmds", list(DrawCommand.EncodeDrawCommand.command, cmds)),
        ])
      | DrawGlobal(cmds) =>
        object_([
          ("type", string("draw-global")),
          ("cmds", list(DrawCommand.EncodeDrawCommand.command, cmds)),
        ])
      | MIDIKeyboard => object_([("type", string("midi-keyboard"))])
      | KeycodeWriter => object_([("type", string("keycode-writer"))])
      | KeycodeReader => object_([("type", string("keycode-reader"))])
      | Histogram => object_([("type", string("histogram"))])
      | Regl(opts) =>
        object_([
          ("type", string("regl")),
          ("options", EncodeReglOptions.reglOptions(opts)),
        ])
      | RawAudioWriter(fmt) =>
        object_([
          ("type", string("raw-audio-writer")),
          ("format", rawAudioFormat(fmt)),
        ])

      | RawAudioReader(fmt) =>
        object_([
          ("type", string("raw-audio-reader")),
          ("format", rawAudioFormat(fmt)),
        ])
      | HandDrawn => object_([("type", string("hand-drawn"))])
      | Reader(t) =>
        object_([
          ("type", string("reader")),
          ("readerType", EncodeReaderType.readerType(t)),
        ])
      }
    );

  let rotation = Json.Encode.float;

  let layer = r =>
    Json.Encode.(
      object_([
        ("id", nullable(string, r.id)),
        ("content", layerContent(r.content)),
        ("enabled", bool(r.enabled)),
        ("alpha", float(r.alpha)),
        (
          "compositeOperation",
          string(string_of_compositeOperation(r.compositeOperation)),
        ),
        ("transformMatrix", transformMatrix(r.transformMatrix)),
        ("rotation", rotation(r.rotation)),
        ("filters", string(r.filters)),
      ])
    );
};

let getLayerKey: layer => string =
  layer =>
    Belt.Option.getWithDefault(
      layer.id,
      Js.Json.stringify(EncodeLayer.layerContent(layer.content)),
    );

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
      | KeycodeReader
      | KeycodeWriter
      | RawAudioWriter(_)
      | RawAudioReader(_)
      | Histogram
      | Draw(_)
      | Regl(_) =>
        <div> <canvas ref=savePreviewRef width="120" height="120" /> </div>
      | Fill(_)
      | DrawGlobal(_)
      | PitchClasses(_)
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
              | Slitscan(cameraOptions) => <div />
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
              | Analysis(analysisOptions) => <div />
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
              | HandDrawn
              | Webcam
              | MIDIKeyboard
              | KeycodeReader
              | KeycodeWriter
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
                value => changeLayer(layer, Some({...layer, alpha: value}))
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
                        ReactDOMRe.Style.make(~transform="rotate(180deg)", ())
                      )>
                      <MaterialUIIcons.ExpandMore />
                    </span> :
                    <MaterialUIIcons.ExpandMore />
                )
              </IconButton>
            </FormGroup>
          </div>
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
              <FormGroup>
                <FloatSlider
                  label="x"
                  value=layer.transformMatrix.horizontalMoving
                  min=(-. float_of_int(width))
                  max=(float_of_int(width))
                  onChange=(
                    newX =>
                      changeLayer(
                        layer,
                        Some({
                          ...layer,
                          transformMatrix: {
                            ...layer.transformMatrix,
                            horizontalMoving: newX,
                          },
                        }),
                      )
                  )
                />
                <FloatSlider
                  label="scaleX"
                  value=layer.transformMatrix.horizontalScaling
                  min=(-5.0)
                  max=5.0
                  onChange=(
                    newScaleX =>
                      changeLayer(
                        layer,
                        Some({
                          ...layer,
                          transformMatrix: {
                            ...layer.transformMatrix,
                            horizontalScaling: newScaleX,
                          },
                        }),
                      )
                  )
                />
              </FormGroup>
              <FormGroup>
                <FloatSlider
                  label="y"
                  value=layer.transformMatrix.verticalMoving
                  min=(-. float_of_int(height))
                  max=(float_of_int(height))
                  onChange=(
                    newY =>
                      changeLayer(
                        layer,
                        Some({
                          ...layer,
                          transformMatrix: {
                            ...layer.transformMatrix,
                            verticalMoving: -. newY,
                          },
                        }),
                      )
                  )
                />
                <FloatSlider
                  label="scaleY"
                  value=layer.transformMatrix.verticalScaling
                  min=(-5.0)
                  max=5.0
                  onChange=(
                    newScaleY =>
                      changeLayer(
                        layer,
                        Some({
                          ...layer,
                          transformMatrix: {
                            ...layer.transformMatrix,
                            verticalScaling: newScaleY,
                          },
                        }),
                      )
                  )
                />
              </FormGroup>
              <FormGroup>
                <FloatSlider
                  label="skewX"
                  value=layer.transformMatrix.horizontalSkewing
                  min=(-5.0)
                  max=5.0
                  onChange=(
                    newSkewX =>
                      changeLayer(
                        layer,
                        Some({
                          ...layer,
                          transformMatrix: {
                            ...layer.transformMatrix,
                            horizontalSkewing: newSkewX,
                          },
                        }),
                      )
                  )
                />
                <FloatSlider
                  label="skewY"
                  value=layer.transformMatrix.verticalSkewing
                  min=(-5.0)
                  max=5.0
                  onChange=(
                    newSkewY =>
                      changeLayer(
                        layer,
                        Some({
                          ...layer,
                          transformMatrix: {
                            ...layer.transformMatrix,
                            verticalSkewing: newSkewY,
                          },
                        }),
                      )
                  )
                />
              </FormGroup>
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
