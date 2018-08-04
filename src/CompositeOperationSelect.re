open Canvas;

let allCompositeModes = [|
  SourceOver,
  SourceIn,
  SourceOut,
  SourceAtop,
  DestinationOver,
  DestinationIn,
  DestinationOut,
  DestinationAtop,
  Lighter,
  Copy,
  Xor,
  Multiply,
  Screen,
  Overlay,
  Darken,
  Lighten,
  ColorDodge,
  ColorBurn,
  HardLight,
  SoftLight,
  Difference,
  Exclusion,
  Hue,
  Saturation,
  Color,
  Luminosity,
|];

external coerceToString : Js.t({..}) => string = "%identity";

let component = ReasonReact.statelessComponent("CompositeOperationSelect");

let make = (~compositeOperation, ~onChange, _children) => {
  ...component,
  render: self =>
    MaterialUi.(
      <Select
        value=(`String(string_of_compositeOperation(compositeOperation)))
        onChange=(
          (event, _) =>
            onChange(
              compositeOperation_of_string(
                ReactDOMRe.domElementToObj(ReactEventRe.Form.target(event))##value,
              ),
            )
        )>
        (
          ReasonReact.array(
            allCompositeModes
            |> Array.map(mode => {
                 let s = string_of_compositeOperation(mode);
                 <MenuItem value=(`String(s)) key=s>
                   (ReasonReact.string(s))
                 </MenuItem>;
               }),
          )
        )
      </Select>
    ),
};
