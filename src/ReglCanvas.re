open Canvas;
open Regl;

type state = {
  canvasRef: ref(option(Dom.element)),
  reglRef: ref(option(regl)),
  rootTextureRef: ref(option(texture)),
  drawCommandRef: ref(option(Regl.drawCommand)),
};

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (~layerRefs, ~setRef, ~saveTick, ~layerKey, ~width, ~height, _children) => {
  let handleSetRef = (aRef, {ReasonReact.state}) => {
    setRef(aRef);
    let maybeRef = Js.Nullable.toOption(aRef);
    state.canvasRef := maybeRef;

    switch (maybeRef, state.reglRef^) {
    | (Some(el), None) =>
      let theRegl = regl(el);
      state.reglRef := Some(theRegl);

      switch (Belt.Map.String.get(layerRefs^, "root")) {
      | Some(canvas) =>
        let rootTexture = texture(theRegl, `Canvas(canvas));
        Js.log(rootTexture);
        state.rootTextureRef := Some(rootTexture);
      | None => ()
      };

      let drawCommand = Regl.makeDrawCommand(theRegl, sobelSpec);
      state.drawCommandRef := Some(drawCommand);
    | _ => ()
    };
  };

  {
    ...component,
    initialState: () => {
      canvasRef: ref(None),
      drawCommandRef: ref(None),
      rootTextureRef: ref(None),
      reglRef: ref(None),
    },
    reducer: ((), _state) => ReasonReact.NoUpdate,
    didMount: self =>
      saveTick(layerKey, () =>
        switch (self.state.reglRef^) {
        | None => ()
        | Some(regl) =>
          clear(regl, {"color": [|0.0, 0.0, 0.0, 1.0|], "depth": 1.0});

          switch (self.state.drawCommandRef^) {
          | Some(f) => draw(f, Js.Obj.empty())
          | None => ()
          };
        }
      ),
    render: self =>
      <canvas
        ref=(self.handle(handleSetRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />,
  };
};
