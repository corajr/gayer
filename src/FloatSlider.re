let component = ReasonReact.statelessComponent("FloatSlider");

let make =
    (
      ~min: float=0.0,
      ~max: float=1.0,
      ~label: string,
      ~value: float,
      ~updater: float => unit,
      _children,
    ) => {
  ...component,
  render: self =>
    MaterialUi.(
      <FormGroup row=true>
        <Typography>
          (ReasonReact.string(label ++ ": " ++ Js.Float.toString(value)))
        </Typography>
        <Slider min max value onChange=((_evt, value) => updater(value)) />
      </FormGroup>
    ),
};
