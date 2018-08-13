let component = ReasonReact.statelessComponent("IntSlider");

let make =
    (
      ~min: int=0,
      ~max: int=119,
      ~includeTextField: bool=false,
      ~label: string,
      ~value: int,
      ~step: int=1,
      ~onChange: int => unit,
      _children,
    ) => {
  ...component,
  render: self =>
    MaterialUi.(
      <FormGroup row=true>
        <div style=(ReactDOMRe.Style.make(~display="flex", ()))>
          <Typography>
            (ReasonReact.string(label ++ ": " ++ Js.Int.toString(value)))
          </Typography>
        </div>
        <Slider
          min=(float_of_int(min))
          max=(float_of_int(max))
          step=(float_of_int(step))
          value=(float_of_int(value))
          onChange=((_evt, value) => onChange(int_of_float(value)))
        />
      </FormGroup>
    ),
};
