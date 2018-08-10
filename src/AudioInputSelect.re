open Audio.AudioInput;

let allAudioInputs = [|
  AudioFile("media/la_cathedrale_engloutie.m4a"),
  PinkNoise,
  Mic,
|];

let component = ReasonReact.statelessComponent("CompositeOperationSelect");

let make = (~audioInputSetting, ~onChangeSetting, _children) => {
  ...component,
  render: self =>
    MaterialUi.(
      <Select
        value=(
                `String(
                  Js.Json.stringify(
                    EncodeAudioInput.audioInputSetting(audioInputSetting),
                  ),
                )
              )
        onChange=(
          (event, _) => {
            let s = ReactDOMRe.domElementToObj(
                      ReactEventRe.Form.target(event),
                    )##value;
            let json = Json.parseOrRaise(s);
            let newSetting = DecodeAudioInput.audioInputSetting(json);
            onChangeSetting(newSetting);
          }
        )>
        (
          ReasonReact.array(
            allAudioInputs
            |> Array.map(setting => {
                 let s =
                   Js.Json.stringify(
                     EncodeAudioInput.audioInputSetting(setting),
                   );
                 <MenuItem value=(`String(s)) key=s>
                   (ReasonReact.string(s))
                 </MenuItem>;
               }),
          )
        )
      </Select>
    ),
};
