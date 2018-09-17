type state = {canvasRef: ref(option(Dom.element))};

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~layerKey, ~layerRefs, ~setRef, _children) => {
  ...component,
  initialState: () => {canvasRef: ref(None)},
  reducer: ((), _state: state) => ReasonReact.NoUpdate,
  render: self => <canvas width="1" height="128" />,
};
