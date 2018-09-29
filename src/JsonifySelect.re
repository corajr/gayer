let component = ReasonReact.statelessComponent(__MODULE__);

let make =
    (~allSettings, ~currentSetting, ~onChange, ~encoder, ~decoder, _children) => {
  ...component,
  render: self =>
    MaterialUi.(
      <Select
        value=(`String(Js.Json.stringify(encoder(currentSetting))))
        onChange=(
          (event, _) => {
            let s = ReactDOMRe.domElementToObj(
                      ReactEventRe.Form.target(event),
                    )##value;
            let json = Json.parseOrRaise(s);
            let newSetting = decoder(json);
            onChange(newSetting);
          }
        )>
        (
          ReasonReact.array(
            allSettings
            |> Array.map(setting => {
                 let s = Js.Json.stringify(encoder(setting));
                 <MenuItem value=(`String(s)) key=s>
                   (ReasonReact.string(s))
                 </MenuItem>;
               }),
          )
        )
      </Select>
    ),
};
