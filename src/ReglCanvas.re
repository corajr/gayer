open Canvas;
open Regl;

type state = {
  canvasRef: ref(option(Dom.element)),
  reglRef: ref(option(regl)),
  textureRefs: ref(Belt.Map.String.t(Regl.texture)),
  drawCommandRef: ref(option(Regl.drawCommand)),
};

let copyLayerToTexture =
    (
      maybeRegl: ref(option(regl)),
      textureRefs: ref(Belt.Map.String.t(Regl.texture)),
      layerRefs: ref(Belt.Map.String.t(Dom.element)),
      layerKey: string,
      textureKey: string,
    ) =>
  switch (maybeRegl^, Belt.Map.String.get(layerRefs^, layerKey)) {
  | (Some(regl), Some(canvas)) =>
    try (
      {
        let aTexture = texture(regl, `Canvas(canvas));
        textureRefs := Belt.Map.String.set(textureRefs^, textureKey, aTexture);
      }
    ) {
    | _ => ()
    }
  | _ => ()
  };

let applyWithTexture =
    (
      drawCommandRef: ref(option(Regl.drawCommand)),
      textureRefs: ref(Belt.Map.String.t(Regl.texture)),
      key: string,
      width: int,
      height: int,
    ) =>
  switch (drawCommandRef^, Belt.Map.String.get(textureRefs^, key)) {
  | (Some(f), Some(texture)) =>
    draw(f, {"texture": texture, "resolution": [|width, height|]})
  | _ => ()
  };

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~layerRefs,
      ~opts,
      ~setRef,
      ~saveTick,
      ~layerKey,
      ~width,
      ~height,
      _children,
    ) => {
  let handleSetRef = (aRef, {ReasonReact.state}) => {
    setRef(aRef);
    let maybeRef = Js.Nullable.toOption(aRef);
    state.canvasRef := maybeRef;

    switch (maybeRef, state.reglRef^) {
    | (Some(el), None) =>
      let theRegl = regl(el);
      state.reglRef := Some(theRegl);

      let drawCommand =
        switch (opts) {
        | Sobel(_) => Regl.makeDrawCommand(theRegl, sobelSpec(theRegl))
        | Displacement(_) =>
          Regl.makeDrawCommand(theRegl, displaceSpec(theRegl))
        };

      state.drawCommandRef := Some(drawCommand);
    | _ => ()
    };
  };

  {
    ...component,
    initialState: () => {
      canvasRef: ref(None),
      drawCommandRef: ref(None),
      textureRefs: ref(Belt.Map.String.empty),
      reglRef: ref(None),
    },
    reducer: ((), _state) => ReasonReact.NoUpdate,
    didMount: self =>
      saveTick(self.onUnmount, layerKey, () =>
        switch (self.state.reglRef^) {
        | None => ()
        | Some(regl) =>
          clear(regl, {"color": [|0.0, 0.0, 0.0, 1.0|], "depth": 1.0});
          switch (opts) {
          | Sobel({sourceLayer}) =>
            copyLayerToTexture(
              self.state.reglRef,
              self.state.textureRefs,
              layerRefs,
              sourceLayer,
              sourceLayer,
            );

            applyWithTexture(
              self.state.drawCommandRef,
              self.state.textureRefs,
              sourceLayer,
              width,
              height,
            );
          | Displacement({displacementSourceLayer, displacementMap}) =>
            copyLayerToTexture(
              self.state.reglRef,
              self.state.textureRefs,
              layerRefs,
              displacementSourceLayer,
              displacementSourceLayer,
            );

            switch (
              self.state.drawCommandRef^,
              Belt.Map.String.get(
                self.state.textureRefs^,
                displacementSourceLayer,
              ),
              Belt.Map.String.get(self.state.textureRefs^, displacementMap),
            ) {
            | (Some(f), Some(source), Some(displacement)) =>
              draw(
                f,
                {
                  "texture": source,
                  "displace_map": displacement,
                  "maximum": 20.0,
                },
              )
            | _ => ()
            };
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
