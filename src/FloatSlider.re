let component = ReasonReact.statelessComponent("FloatSlider");

let make =
    (
      ~min: float=0.0,
      ~max: float=1.0,
      ~label: string,
      ~value: float,
      ~step: option(float)=?,
      ~onChange: float => unit,
      _children,
    ) => {
  ...component,
  render: self =>
    MaterialUi.(
      <FormGroup row=true>
        <Typography>
          (ReasonReact.string(label ++ ": " ++ Js.Float.toString(value)))
        </Typography>
        (
          switch (step) {
          | Some(f) =>
            <Slider
              min
              max
              value
              step=f
              onChange=((_evt, value) => onChange(value))
            />
          | None =>
            <Slider
              min
              max
              value
              onChange=((_evt, value) => onChange(value))
            />
          }
        )
      </FormGroup>
    ),
};
