open Canvas;

type state = {
  canvasRef: ref(option(Dom.element)),
  mouseDown: ref(bool),
};

type action =
  | MouseDown
  | MouseUp;

let component = ReasonReact.reducerComponent("HandDrawnCanvas");

let getXY: ReactEventRe.Mouse.t => (int, int) =
  evt => {
    let target = ReactEventRe.Mouse.target(evt);
    let rect = ReactDOMRe.domElementToObj(target)##getBoundingClientRect();
    (
      ReactEventRe.Mouse.clientX(evt) - rect##left,
      ReactEventRe.Mouse.clientY(evt) - rect##top,
    );
  };

let make = (~setRef, ~width, ~height, _children) => {
  let handleMouseDown = (event, {ReasonReact.state}) =>
    state.mouseDown := true;
  let handleMouseUp = (event, {ReasonReact.state}) =>
    state.mouseDown := false;

  let handleMouseMove = (event, {ReasonReact.state}) =>
    switch (state.mouseDown^, state.canvasRef^) {
    | (true, Some(canvas)) =>
      let (x, y) = getXY(event);
      let canvasElt = getFromReact(canvas);
      let ctx = getContext(canvasElt);
      Ctx.beginPath(ctx);
      Ctx.setFillStyle(ctx, "white");
      Ctx.circle(ctx, x, y, 15);
      Ctx.fill(ctx);
      Ctx.closePath(ctx);
    | _ => ()
    };

  {
    ...component,
    initialState: () => {canvasRef: ref(None), mouseDown: ref(false)},
    reducer: ((), _state) => ReasonReact.NoUpdate,
    render: self =>
      <canvas
        style=(ReactDOMRe.Style.make(~opacity="0.0", ()))
        ref=(
          aRef => {
            setRef(aRef);
            let maybeRef = Js.Nullable.toOption(aRef);
            self.state.canvasRef := maybeRef;
          }
        )
        onMouseDown=(self.handle(handleMouseDown))
        onMouseOut=(self.handle(handleMouseUp))
        onMouseUp=(self.handle(handleMouseUp))
        onMouseMove=(self.handle(handleMouseMove))
        /* onTouchStart=(self.handle(handleMouseDown)) */
        /* onTouchEnd=(self.handle(handleMouseUp)) */
        /* onTouchMove=(self.handle(handleMouseMove)) */
        /* width=(Js.Int.toString(width)) */
        /* height=(Js.Int.toString(height)) */
        width="480"
        height="480"
      />,
  };
};
