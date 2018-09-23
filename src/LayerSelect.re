let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~layerKeys, ~onChange, ~currentValue, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <Select
        value=(`String(currentValue))
        onChange=(
          (event, _) => {
            let s = ReactDOMRe.domElementToObj(
                      ReactEventRe.Form.target(event),
                    )##value;
            onChange(s);
          }
        )>
        (
          ReasonReact.array(
            layerKeys
            |> Array.of_list
            |> Array.map(key =>
                 <MenuItem value=(`String(key)) key>
                   (ReasonReact.string(key))
                 </MenuItem>
               ),
          )
        )
      </Select>
    ),
};
