open Canvas;
open Canvas.DrawCommand;

type state = {drawContext};
let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~cmds,
      ~layerKey,
      ~layerRefs,
      ~setRef,
      ~saveTick,
      ~width,
      ~height,
      _children,
    ) => {
  let setCanvasRef = (theRef, {ReasonReact.state}) => {
    let maybeCanvas = Js.Nullable.toOption(theRef);
    state.drawContext.maybeCtxRef :=
      Belt.Option.map(maybeCanvas, x => getContext(getFromReact(x)));
    setRef(theRef);
  };

  {
    ...component,
    reducer: ((), _state: state) => ReasonReact.NoUpdate,
    initialState: () => {
      drawContext: {
        maybeCtxRef: ref(None),
        width,
        height,
        variables: Belt.Map.String.empty,
      },
    },
    didMount: self =>
      saveTick(self.onUnmount, layerKey, _t =>
        drawCommands(self.state.drawContext, cmds)
      ),
    render: self =>
      <canvas
        ref=(self.handle(setCanvasRef))
        width=(Js.Int.toString(width))
        height=(Js.Int.toString(height))
      />,
  };
};
