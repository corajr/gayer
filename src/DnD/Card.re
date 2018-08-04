open Layer;

let component = ReasonReact.statelessComponent("Card");

let make = (~id, ~layer, ~changeLayer, ~setRef, _children) => {
  ...component,
  render: _self =>
    <div id style=(ReactDOMRe.Style.make(~marginBottom="16px", ()))>
      <Layer layer changeLayer setRef />
    </div>,
};
