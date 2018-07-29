open Layer;

module DropTargetSpec =
  BsReactDnd.DropTarget.MakeSpec({
    type dragItem = T.dragItem;
    type dropItem = T.dropItem;
    type props = {
      .
      "id": int,
      "moveCard": [@bs.meth] ((int, int) => unit),
    };
  });

module DropTargetWrapper =
  BsReactDnd.DropTarget.Make({
    let itemType = T.itemType;
    type spec = DropTargetSpec.t;
    let spec: spec =
      DropTargetSpec.make(
        ~hover=
          (props, monitor, _component) => {
            let dragItem = monitor##getItem() |> Js.toOption;
            let hoverId = props##id;
            switch (dragItem) {
            | Some(dragItem) when dragItem##id !== hoverId =>
              /* Time to actually perform the action */
              props##moveCard(dragItem##id, hoverId)
            | Some(_)
            | None => ()
            };
          },
        (),
      );
    type collectedProps = {. "connectDropTarget": BsReactDnd.Utils.wrapper};
    type collect =
      (BsReactDnd.DropTarget.connector, DropTargetSpec.monitor) =>
      collectedProps;
    let collect: collect =
      (connect, _) => {"connectDropTarget": connect##dropTarget()};
  });

module DragSourceSpec =
  BsReactDnd.DragSource.MakeSpec({
    type dragItem = T.dragItem;
    type dropItem = T.dropItem;
    type props = {. "id": int};
  });

module DragSourceWrapper =
  BsReactDnd.DragSource.Make({
    let itemType = T.itemType;
    type spec = DragSourceSpec.t;
    let spec: spec =
      DragSourceSpec.make(
        ~beginDrag=(props, _, _) => {"id": props##id},
        (),
      );
    type collectedProps = {
      .
      "connectDragSource": BsReactDnd.Utils.wrapper,
      "isDragging": bool,
    };
    type collect =
      (BsReactDnd.DragSource.connector, DragSourceSpec.monitor) =>
      collectedProps;
    let collect: collect =
      (connect, monitor) => {
        "connectDragSource": connect##dragSource(),
        "isDragging": monitor##isDragging(),
      };
  });

let component = ReasonReact.statelessComponent("Card");

let make = (~id, ~layer, ~moveCard, _children) => {
  ...component,
  render: _self =>
    /* need to be very carefull when passing `props` isn't annotated, this has to be the same as DragSourceSpec.props */
    <DropTargetWrapper props={"id": id, "moveCard": moveCard}>
      ...(
           (~collectedProps as dropTarget) =>
             <DragSourceWrapper props={"id": id}>
               ...(
                    (~collectedProps as dragSource) =>
                      dropTarget##connectDropTarget(
                        dragSource##connectDragSource(
                          <div
                            style=(
                              ReactDOMRe.Style.make(
                                ~border="1px dashed gray",
                                ~padding="0.5rem 1rem",
                                ~marginBottom=".5rem",
                                ~backgroundColor="black",
                                ~cursor="move",
                                ~opacity=dragSource##isDragging ? "0" : "1",
                                (),
                              )
                            )>
                            <Layer layer />
                          </div>,
                        ),
                      )
                  )
             </DragSourceWrapper>
         )
    </DropTargetWrapper>,
};
