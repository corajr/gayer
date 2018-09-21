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
      ~currentFilterValues,
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
        switch (currentFilterValues^, self.state.rotDisplayRef^) {
        | (Audio.Mono(values), Some(display)) =>
          let n = Array.length(values);
          let scale = float_of_int(textHeight) /. float_of_int(n);
          for (i in 0 to n - 1) {
            let v = values[i];
            if (v > 0.0) {
              drawText(
                display,
                textWidth - 1,
                int_of_float(scale *. float_of_int(i)),
                String.make(1, Char.chr(i)),
              );
            };
          };
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
