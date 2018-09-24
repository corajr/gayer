open Layer;

type anchorEl = ReasonReact.reactRef;

type state = {
  anchorEl: ref(option(anchorEl)),
  open_: bool,
};

type action =
  | SetAnchorEl(anchorEl)
  | Open
  | Close;

let setRef = (theRef, {ReasonReact.send}) => {
  let maybeRef = Js.Nullable.toOption(theRef);
  switch (maybeRef) {
  | Some(el) => send(SetAnchorEl(el))
  | None => ()
  };
};

let sendOpen = (_evt, {ReasonReact.send}) => send(Open);

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~onAdd, _children) => {
  ...component,
  initialState: () => {anchorEl: ref(None), open_: false},
  reducer: (action, state) =>
    switch (action) {
    | SetAnchorEl(el) =>
      ReasonReact.SideEffects((self => self.state.anchorEl := Some(el)))
    | Open => ReasonReact.Update({...state, open_: true})
    | Close => ReasonReact.Update({...state, open_: false})
    },
  render: self =>
    MaterialUi.(
      <div>
        <Button
          ref=(self.handle(setRef))
          onClick=(self.handle(sendOpen))
          variant=`Fab
          color=`Primary>
          <MaterialUIIcons.Add />
        </Button>
        <NewLayerMenu
          anchorEl=self.state.anchorEl
          open_=self.state.open_
          onAdd=(
            layer => {
              onAdd(LayerGenerator.maybeAddId(layer));
              self.send(Close);
            }
          )
        />
      </div>
    ),
};
