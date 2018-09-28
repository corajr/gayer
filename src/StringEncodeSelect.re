let component = ReasonReact.statelessComponent(__MODULE__);

let make =
    (~allSettings, ~currentSetting, ~onChange, ~encoder, ~decoder, _children) => {
  ...component,
  render: self =>
    MaterialUi.(
      <Select
        value=(`String(encoder(currentSetting)))
        onChange=(
          (event, _) => {
            let s = ReactDOMRe.domElementToObj(
                      ReactEventRe.Form.target(event),
                    )##value;
            let newSetting = decoder(s);
            onChange(newSetting);
          }
        )>
        (
          ReasonReact.array(
            allSettings
            |> Array.map(setting => {
                 let s = encoder(setting);
                 <MenuItem value=(`String(s)) key=s>
                   (ReasonReact.string(s))
                 </MenuItem>;
               }),
          )
        )
      </Select>
    ),
};
