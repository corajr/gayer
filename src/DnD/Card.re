open Layer;

let component = ReasonReact.statelessComponent("Card");

let make = (~id, ~layer, ~setRef, _children) => {
  ...component,
  render: _self =>
    <div
      id
      style=(
        ReactDOMRe.Style.make(
          ~border="1px dashed gray",
          ~padding="0.5rem 1rem",
          ~marginBottom=".5rem",
          ~backgroundColor="black",
          ~cursor="move",
          (),
        )
      )>
      <Layer layer setRef />
    </div>,
};
