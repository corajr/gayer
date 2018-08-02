open Audio;
open Canvas;
open Layer;
open Music;

type filterInputSetting =
  | PinkNoise
  | Mic;

type params = {
  readPosDelta: int,
  writePosDelta: int,
  writePosOffset: int,
  filterInputSetting,
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
  filterInputSetting: PinkNoise,
  inputGain: 1.0,
  outputGain: 0.2,
  q: defaultQ,
  transpose: 0,
  shouldClear: false,
  layers: [
    {content: Analysis, alpha: 1.0, compositeOperation: SourceOver},
    {
      content: Webcam({slitscan: None}),
      /* content: Webcam({slitscan: Some({x: 60})}), */
      alpha: 0.25,
      compositeOperation: SourceOver,
    },
    {
      content: Image("media/meaning_of_the_flag.png"),
      alpha: 0.5,
      compositeOperation: Multiply,
    },
    {
      content: PitchClasses(cMajor),
      alpha: 1.0,
      compositeOperation: DestinationOut,
    },
    {content: Reader(R), alpha: 0.0, compositeOperation: SourceOver},
  ],
};

module DecodeParams = {
  let filterInputSetting = json =>
    Json.Decode.(
      json
      |> string
      |> (
        fun
        | "pink-noise" => PinkNoise
        | "mic" => Mic
        | _ => PinkNoise
      )
    );
  let params = json =>
    Json.Decode.{
      readPosDelta: json |> field("readPosDelta", int),
      writePosDelta: json |> field("writePosDelta", int),
      writePosOffset: json |> field("writePosOffset", int),
      filterInputSetting:
        json |> field("filterInputSetting", filterInputSetting),
      inputGain: json |> field("inputGain", float),
      outputGain: json |> field("outputGain", float),
      q: json |> field("q", float),
      transpose: json |> field("transpose", int),
      shouldClear: json |> field("shouldClear", bool),
      layers: json |> field("layers", list(DecodeLayer.layer)),
    };
};

module EncodeParams = {
  let filterInputSetting = r =>
    Json.Encode.(
      switch (r) {
      | PinkNoise => string("pink-noise")
      | Mic => string("mic")
      }
    );
  let params = r =>
    Json.Encode.(
      object_([
        ("readPosDelta", int(r.readPosDelta)),
        ("writePosDelta", int(r.writePosDelta)),
        ("writePosOffset", int(r.writePosOffset)),
        ("filterInputSetting", filterInputSetting(r.filterInputSetting)),
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

let make = (~params, ~onMoveCard, ~onSetRef, _children) => {
  ...component,
  render: self =>
    <div>
      <Container
        cards=(
          List.map(
            layer => {
              let id = Hashtbl.hash(layer);
              {T.id, T.layer};
            },
            params.layers,
          )
        )
        onMoveCard
        onSetRef
      />
      <div>
        <div>
          (ReasonReact.string("readPosDelta: "))
          (ReasonReact.string(Js.Int.toString(params.readPosDelta)))
        </div>
        <div>
          (ReasonReact.string("writePosDelta: "))
          (ReasonReact.string(Js.Int.toString(params.writePosDelta)))
        </div>
        <div>
          (ReasonReact.string("writePosOffset: "))
          (ReasonReact.string(Js.Int.toString(params.writePosOffset)))
        </div>
        <div>
          (ReasonReact.string("filterInputSetting: "))
          (
            ReasonReact.string(
              Js.Json.stringify(
                EncodeParams.filterInputSetting(params.filterInputSetting),
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
        <div>
          (ReasonReact.string("shouldClear: "))
          (ReasonReact.string(params.shouldClear ? "true" : "false"))
        </div>
      </div>
    </div>,
};
