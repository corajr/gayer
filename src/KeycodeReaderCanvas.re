open RotDisplay;

type state = {
  canvasRef: ref(option(Dom.element)),
  parentElRef: ref(option(Dom.element)),
};
let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~layerKey, ~layerRefs, ~setRef, ~width=1, ~height=240, _children) => {
  let setUpRot = (theRef, {ReasonReact.state}) => {
    let maybeParent = Js.Nullable.toOption(theRef);
    state.parentElRef := maybeParent;

    switch (state.canvasRef^, maybeParent) {
    | (Some(canvas), Some(parentEl)) =>
      ElementRe.appendChild(parentEl, canvas);
      setRef(Some(canvas));
    | _ => ()
    };
  };
  {
    ...component,
    reducer: ((), _state: state) => ReasonReact.NoUpdate,
    initialState: () => {canvasRef: ref(None), parentElRef: ref(None)},
    didMount: self => {
      let display = rotDisplay(defaultOptions);
      drawText(display, 0, 0, "Press keys to play!");
      drawText(display, 0, 8, "(SPC to clear)");

      self.state.canvasRef := Some(getContainer(display));

      self.onUnmount(() =>
        switch (self.state.canvasRef^, self.state.parentElRef^) {
        | (Some(canvas), Some(parentEl)) =>
          ignore(ElementRe.removeChild(parentEl, canvas))
        | _ => ()
        }
      );
    },
    render: self => <div ref=(self.handle(setUpRot)) />,
  };
};
