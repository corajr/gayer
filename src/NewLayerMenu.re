open Layer;
open LayerGenerator;

type state = {anchorEl: ref(option(Dom.element))};

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~onAdd, ~open_, ~anchorEl, _children) => {
  ...component,
  initialState: () => {anchorEl: ref(None)},
  reducer: ((), _state) => ReasonReact.NoUpdate,
  didMount: self =>
    self.state.anchorEl := Belt.Option.map(anchorEl^, ReactDOMRe.findDOMNode),
  willUpdate: ({newSelf}) =>
    newSelf.state.anchorEl :=
      Belt.Option.map(anchorEl^, ReactDOMRe.findDOMNode),
  render: self => {
    open MaterialUi;
    let items =
      ReasonReact.array(
        allLayerTypes
        |> Array.map(((s, example)) =>
             <MenuItem key=s onClick=(_evt => onAdd(example))>
               (ReasonReact.string(s))
             </MenuItem>
           ),
      );
    let anchorEl = self.state.anchorEl^;

    <Menu open_ anchorEl> items </Menu>;
  },
};
