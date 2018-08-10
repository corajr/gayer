let component = ReasonReact.statelessComponent("IntSlider");

let make =
    (
      ~min: int=0,
      ~max: int=119,
      ~includeTextField: bool=false,
      ~label: string,
      ~value: int,
      ~updater: int => unit,
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
          (
            includeTextField ?
              <TextField
                value=(`Int(value))
                onChange=(
                  evt => {
                    let v = ReactDOMRe.domElementToObj(
                              ReactEventRe.Form.target(evt),
                            )##value;
                    updater(int_of_string(v));
                  }
                )
                type_="number"
                margin=`None
              /> :
              ReasonReact.null
          )
        </div>
        <Slider
          min=(float_of_int(min))
          max=(float_of_int(max))
          value=(float_of_int(value))
          onChange=((_evt, value) => updater(int_of_float(value)))
        />
      </FormGroup>
    ),
};
