open Audio;
open Audio.AudioInput;
open Canvas;
open Layer;
open Music;

type params = {
  readPosDelta: int,
  writePosDelta: int,
  writePosOffset: int,
  audioInputSetting,
  inputGain: float,
  outputGain: float,
  q: float,
  transpose: int,
  shouldClear: bool,
  layers: list(layer),
};

let defaultParams: params = {
  readPosDelta: 1,
  writePosDelta: 1,
  writePosOffset: 0,
  audioInputSetting: PinkNoise,
  inputGain: 1.0,
  outputGain: 0.1,
  q: defaultQ,
  transpose: 0,
  shouldClear: true,
  layers: [
    {
      content: Image("media/hubble_ultra_deep_field.jpg"),
      alpha: 1.0,
      compositeOperation: SourceOver,
    },
    {content: Analysis, alpha: 0.5, compositeOperation: SourceOver},
    {
      content: Webcam({slitscan: None}),
      /* content: Webcam({slitscan: Some({x: 60})}), */
      alpha: 0.25,
      compositeOperation: SourceOver,
    },
    {content: Fill("white"), alpha: 0.0125, compositeOperation: SourceOver},
    {
      content: PitchClasses(cMajor),
      alpha: 1.0,
      compositeOperation: DestinationOut,
    },
    {content: Reader(R), alpha: 0.0, compositeOperation: SourceOver},
  ],
};

module DecodeParams = {
  open AudioInput.DecodeAudioInput;

  let params = json =>
    Json.Decode.{
      readPosDelta: json |> field("readPosDelta", int),
      writePosDelta: json |> field("writePosDelta", int),
      writePosOffset: json |> field("writePosOffset", int),
      audioInputSetting:
        json |> field("audioInputSetting", audioInputSetting),
      inputGain: json |> field("inputGain", float),
      outputGain: json |> field("outputGain", float),
      q: json |> field("q", float),
      transpose: json |> field("transpose", int),
      shouldClear: json |> field("shouldClear", bool),
      layers: json |> field("layers", list(DecodeLayer.layer)),
    };
};

module EncodeParams = {
  open AudioInput.EncodeAudioInput;

  let params = r =>
    Json.Encode.(
      object_([
        ("readPosDelta", int(r.readPosDelta)),
        ("writePosDelta", int(r.writePosDelta)),
        ("writePosOffset", int(r.writePosOffset)),
        ("audioInputSetting", audioInputSetting(r.audioInputSetting)),
        ("inputGain", float(r.inputGain)),
        ("outputGain", float(r.outputGain)),
        ("q", float(r.q)),
        ("transpose", int(r.transpose)),
        ("shouldClear", bool(r.shouldClear)),
        ("layers", list(EncodeLayer.layer, r.layers)),
      ])
    );
};

let component = ReasonReact.statelessComponent("Params");

let make =
    (~params, ~onMoveCard, ~onSetRef, ~onChangeLayer, ~onSetParams, _children) => {
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
              <FormGroup row=true>
                <div>
                  (ReasonReact.string("readPosDelta: "))
                  (ReasonReact.string(Js.Int.toString(params.readPosDelta)))
                </div>
              </FormGroup>
              <div>
                (ReasonReact.string("writePosDelta: "))
                (ReasonReact.string(Js.Int.toString(params.writePosDelta)))
              </div>
              <div>
                (ReasonReact.string("writePosOffset: "))
                (ReasonReact.string(Js.Int.toString(params.writePosOffset)))
              </div>
            </FormControl>
            <FormControl component=(`String("fieldset"))>
              <FormLabel component=(`String("legend"))>
                (ReasonReact.string("Audio"))
              </FormLabel>
              <div>
                (ReasonReact.string("audioInputSetting: "))
                (
                  ReasonReact.string(
                    Js.Json.stringify(
                      EncodeAudioInput.audioInputSetting(
                        params.audioInputSetting,
                      ),
                    ),
                  )
                )
              </div>
              <div>
                (ReasonReact.string("inputGain: "))
                (ReasonReact.string(Js.Float.toString(params.inputGain)))
              </div>
              <div>
                (ReasonReact.string("outputGain: "))
                (ReasonReact.string(Js.Float.toString(params.outputGain)))
              </div>
              <div>
                (ReasonReact.string("q: "))
                (ReasonReact.string(Js.Float.toString(params.q)))
              </div>
              <div>
                (ReasonReact.string("transpose: "))
                (ReasonReact.string(Js.Int.toString(params.transpose)))
              </div>
            </FormControl>
          </ExpansionPanelDetails>
        </ExpansionPanel>
      )
      <Container
        cards=(
          List.map(
            layer => {
              let id = "card" ++ string_of_int(Hashtbl.hash(layer));
              {T.id, T.layer};
            },
            params.layers,
          )
        )
        onMoveCard
        onSetRef
        onChangeLayer
      />
    </div>,
};
