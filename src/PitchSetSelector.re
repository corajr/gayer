open Music;

let allPitchSets = [|
  majorChord,
  minorChord,
  major7,
  minor7,
  cMajor,
  cSharpMajor,
  cMinor,
  pentatonic,
  majorHexatonic,
  wholetone,
  allPitches,
|];

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~pitchSet, ~onChangeSetting, _children) => {
  ...component,
  render: self =>
    MaterialUi.(
      <Select
        value=(
                `String(Js.Json.stringify(EncodePitchSet.pitchSet(pitchSet)))
              )
        onChange=(
          (event, _) => {
            let s = ReactDOMRe.domElementToObj(
                      ReactEventRe.Form.target(event),
                    )##value;
            let json = Json.parseOrRaise(s);
            let newSetting = DecodePitchSet.pitchSet(json);
            onChangeSetting(newSetting);
          }
        )>
        (
          ReasonReact.array(
            allPitchSets
            |> Array.map(pitchSet => {
                 let s =
                   Js.Json.stringify(EncodePitchSet.pitchSet(pitchSet));
                 <MenuItem value=(`String(s)) key=s>
                   (ReasonReact.string(s))
                 </MenuItem>;
               }),
          )
        )
      </Select>
    ),
};
