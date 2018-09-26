open Canvas;

type keycodeFormat =
  | AsciiAsHeight
  | PitchFilter;

module DecodeKeycodeFormat = {
  let keycodeFormat = json =>
    Json.Decode.(
      json
      |> map(
           fun
           | "pitch-filter" => PitchFilter
           | "ascii"
           | _ => AsciiAsHeight,
           string,
         )
    );
};

module EncodeKeycodeFormat = {
  let keycodeFormat =
    Json.Encode.(
      fun
      | AsciiAsHeight => string("ascii")
      | PitchFilter => string("pitch-filter")
    );
};

module KeycodeFormatSelect = {
  let component = ReasonReact.statelessComponent("KeycodeFormatSelect");
  let make = (~currentSetting, ~onChange, _children) => {
    ...component,
    render: self =>
      <JsonifySelect
        allSettings=[|AsciiAsHeight, PitchFilter|]
        currentSetting
        onChange
        encoder=EncodeKeycodeFormat.keycodeFormat
        decoder=DecodeKeycodeFormat.keycodeFormat
      />,
  };
};

let keyCodeToY = (height, keyCodeN) => height - (keyCodeN - 8) * 2 - 1;

let yToKeyCode = (height, keyCodeY) => (height - (keyCodeY + 1)) / 2 + 8;
