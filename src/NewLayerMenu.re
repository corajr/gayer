open Layer;
open LayerGenerator;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~onAdd, ~open_, ~anchorEl, _children) => {
  ...component,
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

    switch (anchorEl^) {
    | Some(el) =>
      <Menu open_ anchorEl=(ReactDOMRe.findDOMNode(el))> items </Menu>
    | None => <Menu open_> items </Menu>
    };
  },
};
