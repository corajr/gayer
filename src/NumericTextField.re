let component = ReasonReact.statelessComponent("NumericTextField");

let make =
    (
      ~value,
      ~label,
      ~onChange,
      ~style=ReactDOMRe.Style.make(),
      ~margin=`None,
      _children,
    ) => {
  ...component,
  render: self =>
    <div style>
      <MaterialUi.TextField
        value
        label
        margin
        fullWidth=true
        onChange=(
          evt => {
            let v = ReactDOMRe.domElementToObj(ReactEventRe.Form.target(evt))##value;
            onChange(float_of_string(v));
            /* switch (value) { */
            /* | `Int(_) => onChange(`Int(int_of_string(v))) */
            /* | `Float(_) => onChange(`Float(float_of_string(v))) */
            /* }; */
          }
        )
        type_="number"
      />
    </div>,
};
