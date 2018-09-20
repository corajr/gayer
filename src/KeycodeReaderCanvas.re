open Canvas;
open RotDisplay;

type state = {
  canvasRef: ref(option(Dom.element)),
  parentElRef: ref(option(Dom.element)),
  rotDisplayRef: ref(option(RotDisplay.t)),
};
let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~layerKey,
      ~layerRefs,
      ~setRef,
      ~saveTick,
      ~width=240,
      ~height=240,
      ~fontSize=8,
      _children,
    ) => {
  let setUpRot = (theRef, {ReasonReact.state}) => {
    let maybeParent = Js.Nullable.toOption(theRef);
    state.parentElRef := maybeParent;

    switch (state.canvasRef^, maybeParent) {
    | (Some(canvas), Some(parentEl)) =>
      ElementRe.appendChild(parentEl, canvas);
      setRef(Js.Nullable.return(canvas));
    | _ => ()
    };
  };

  let textHeight = height / fontSize;
  let textWidth = width / fontSize;

  {
    ...component,
    reducer: ((), _state: state) => ReasonReact.NoUpdate,
    initialState: () => {
      canvasRef: ref(None),
      parentElRef: ref(None),
      rotDisplayRef: ref(None),
    },
    didMount: self => {
      let fontSize = 8;
      let display =
        rotDisplay(
          options(
            ~width=textWidth,
            ~height=textHeight,
            ~fontSize,
            ~forceSquareRatio=true,
          ),
        );
      self.state.rotDisplayRef := Some(display);
      self.state.canvasRef := Some(getContainer(display));
      saveTick(self.onUnmount, layerKey, () =>
        switch (
          Belt.Map.String.get(layerRefs^, "root"),
          self.state.rotDisplayRef^,
        ) {
        | (Some(canvas), Some(display)) =>
          let ctx = getContext(getFromReact(canvas));
          ();
        | _ => ()
        }
      );

      self.onUnmount(() =>
        switch (self.state.canvasRef^, self.state.parentElRef^) {
        | (Some(canvas), Some(parentEl)) =>
          try (ignore(ElementRe.removeChild(parentEl, canvas))) {
          | _ => ()
          }
        | _ => ()
        }
      );
    },
    render: self => <div ref=(self.handle(setUpRot)) />,
  };
};
