let component = ReasonReact.statelessComponent("NumericTextField");

let make = (~value, ~label, ~onChange, _children) => {
  ...component,
  render: self =>
    <MaterialUi.TextField
      value
      label
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
      margin=`None
    />,
};
