open Audio;
open Audio.AudioInput;
open Canvas;
open Layer;
open Music;

type params = {
  width: int,
  height: int,
  readPosDelta: int,
  writePosDelta: int,
  readPosOffset: int,
  writePosOffset: int,
  millisPerTick: int,
  audioInputSetting,
  inputGain: float,
  outputGain: float,
  q: float,
  transpose: int,
  stereo: bool,
  shouldClear: bool,
  layers: list(layer),
};

let defaultParams: params = {
  width: defaultSize,
  height: defaultSize,
  readPosDelta: 1,
  writePosDelta: 1,
  readPosOffset: 0,
  writePosOffset: 0,
  millisPerTick: 33,
  audioInputSetting: PinkNoise,
  inputGain: 1.0,
  outputGain: 0.2,
  stereo: false,
  q:
    qForBinsPerOctave(
      defaultSize === 120 ? defaultSize / 5 : defaultSize / 10,
    ),
  transpose: 0,
  shouldClear: true,
  layers: [],
};

module DecodeParams = {
  open AudioInput.DecodeAudioInput;

  let params = json =>
    Json.Decode.{
      width: json |> field("width", int),
      height: json |> field("height", int),
      readPosDelta: json |> field("readPosDelta", int),
      writePosDelta: json |> field("writePosDelta", int),
      writePosOffset: json |> field("writePosOffset", int),
      readPosOffset: json |> field("readPosOffset", int),
      millisPerTick: json |> field("millisPerTick", int),
      audioInputSetting:
        json |> field("audioInputSetting", audioInputSetting),
      inputGain: json |> field("inputGain", float),
      outputGain: json |> field("outputGain", float),
      q: json |> field("q", float),
      transpose: json |> field("transpose", int),
      shouldClear: json |> field("shouldClear", bool),
      stereo: json |> field("stereo", bool),
      layers: json |> field("layers", list(DecodeLayer.layer)),
    };
};

module EncodeParams = {
  open AudioInput.EncodeAudioInput;

  let params = r =>
    Json.Encode.(
      object_([
        ("width", int(r.width)),
        ("height", int(r.height)),
        ("readPosDelta", int(r.readPosDelta)),
        ("writePosDelta", int(r.writePosDelta)),
        ("readPosOffset", int(r.readPosOffset)),
        ("writePosOffset", int(r.writePosOffset)),
        ("millisPerTick", int(r.millisPerTick)),
        ("audioInputSetting", audioInputSetting(r.audioInputSetting)),
        ("inputGain", float(r.inputGain)),
        ("outputGain", float(r.outputGain)),
        ("q", float(r.q)),
        ("transpose", int(r.transpose)),
        ("shouldClear", bool(r.shouldClear)),
        ("stereo", bool(r.stereo)),
        ("layers", list(EncodeLayer.layer, r.layers)),
      ])
    );
};

let component = ReasonReact.statelessComponent("Params");

let make =
    (
      ~params,
      ~onMoveCard,
      ~onSetRef,
      ~layerRefs,
      ~onChangeLayer,
      ~onSetParams,
      ~getAudio,
      ~saveTick,
      ~millisPerAudioTick,
      _children,
    ) => {
  ...component,
  render: self =>
    <div>
      MaterialUi.(
        <ExpansionPanel
          style=(ReactDOMRe.Style.make(~marginBottom="12px", ()))>
          <ExpansionPanelSummary expandIcon={<MaterialUIIcons.ExpandMore />}>
            <Typography variant=`Subheading color=`Inherit>
              (ReasonReact.string("Global Settings"))
            </Typography>
          </ExpansionPanelSummary>
          <ExpansionPanelDetails>
            <FormControl component=(`String("fieldset"))>
              <FormLabel component=(`String("legend"))>
                (ReasonReact.string("Graphics"))
              </FormLabel>
              <FormGroup row=true>
                <FormControlLabel
                  control={
                    <Switch
                      checked=(`Bool(params.shouldClear))
                      onChange=(
                        (_evt, value) =>
                          onSetParams({...params, shouldClear: value})
                      )
                      value="shouldClear"
                    />
                  }
                  label=(ReasonReact.string("Clear between frames"))
                />
              </FormGroup>
              <IntSlider
                label="Read position offset"
                value=params.readPosOffset
                min=0
                max=(params.width - 1)
                step=1
                onChange=(
                  readPosOffset => onSetParams({...params, readPosOffset})
                )
              />
              <IntSlider
                label="Write position offset"
                value=params.writePosOffset
                min=0
                max=(params.width - 1)
                onChange=(
                  writePosOffset => onSetParams({...params, writePosOffset})
                )
              />
              <IntSlider
                label="Read position delta"
                value=params.readPosDelta
                onChange=(
                  readPosDelta => onSetParams({...params, readPosDelta})
                )
                min=(- params.width + 1)
                max=(params.width - 1)
              />
              <IntSlider
                label="Write position delta"
                value=params.writePosDelta
                onChange=(
                  writePosDelta => onSetParams({...params, writePosDelta})
                )
                min=(- params.width + 1)
                max=(params.width - 1)
              />
            </FormControl>
            <FormControl component=(`String("fieldset"))>
              <FormLabel component=(`String("legend"))>
                (ReasonReact.string("Audio"))
              </FormLabel>
              <FormGroup row=true>
                <FormControlLabel
                  control={
                    <Switch
                      checked=(`Bool(params.stereo))
                      onChange=(
                        (_evt, value) =>
                          onSetParams({...params, stereo: value})
                      )
                      value="stereo"
                    />
                  }
                  label=(ReasonReact.string("Stereo (red=L, blue=R)"))
                />
              </FormGroup>
              <AudioInputSelect
                audioInputSetting=params.audioInputSetting
                onChangeSetting=(
                  audioInputSetting =>
                    onSetParams({...params, audioInputSetting})
                )
              />
              <FloatSlider
                label="Input gain"
                value=params.inputGain
                onChange=(inputGain => onSetParams({...params, inputGain}))
              />
              <FloatSlider
                label="Output gain"
                value=params.outputGain
                onChange=(outputGain => onSetParams({...params, outputGain}))
              />
              <FloatSlider
                label="Q"
                value=params.q
                onChange=(q => onSetParams({...params, q}))
                min=1.0
                max=200.0
              />
              <IntSlider
                label="Transpose"
                value=params.transpose
                onChange=(transpose => onSetParams({...params, transpose}))
                min=(-119)
                max=119
              />
              <IntSlider
                label="Milliseconds per tick"
                value=params.millisPerTick
                onChange=(
                  millisPerTick => onSetParams({...params, millisPerTick})
                )
                min=10
                max=100
              />
            </FormControl>
          </ExpansionPanelDetails>
        </ExpansionPanel>
      )
      <Container
        cards=(
          List.map(
            layer => {
              let id =
                "card"
                ++ (
                  switch (layer.id) {
                  | None => string_of_int(Hashtbl.hash(layer))
                  | Some(id) => id
                  }
                );
              {T.id, T.layer};
            },
            params.layers,
          )
        )
        onMoveCard
        onSetRef
        layerRefs
        onChangeLayer
        saveTick
        rootWidth=params.width
        rootHeight=params.height
      />
    </div>,
};
