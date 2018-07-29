open Audio;
open Canvas;
open Layer;
open Music;

type params = {
  xDelta: int,
  inputGain: float,
  outputGain: float,
  q: float,
  transpose: int,
  shouldClear: bool,
  layers: list(layer),
};

let defaultParams: params = {
  xDelta: 1,
  inputGain: 1.0,
  outputGain: 0.2,
  q: defaultQ,
  transpose: 0,
  shouldClear: false,
  layers: [
    {content: Analysis, alpha: 1.0, compositeOperation: SourceOver},
    {content: Webcam, alpha: 0.25, compositeOperation: Overlay},
    {
      content: Image("media/DeadFishSwimming.gif"),
      alpha: 1.0,
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
  let params = json =>
    Json.Decode.{
      xDelta: json |> field("xDelta", int),
      inputGain: json |> field("inputGain", float),
      outputGain: json |> field("outputGain", float),
      q: json |> field("q", float),
      transpose: json |> field("transpose", int),
      shouldClear: json |> field("shouldClear", bool),
      layers: json |> field("layers", list(DecodeLayer.layer)),
    };
};

module EncodeParams = {
  let params = r =>
    Json.Encode.(
      object_([
        ("xDelta", int(r.xDelta)),
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

let make = (~params, ~onMoveCard, _children) => {
  ...component,
  render: self =>
    <div>
      <div>
        <div>
          (ReasonReact.string("xDelta: "))
          (ReasonReact.string(Js.Int.toString(params.xDelta)))
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
      />
    </div>,
};
