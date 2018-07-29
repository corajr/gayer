module RList = Rationale.RList;

let component = ReasonReact.statelessComponent("Container");

let make = (~cards, ~onMoveCard, ~onSetRef, _children) => {
  let handleMoveCard = (dragId, hoverId) => {
    let dragIndex =
      RList.findIndex((card: T.card) => card.id === dragId, cards);
    let hoverIndex =
      RList.findIndex((card: T.card) => card.id === hoverId, cards);
    switch (dragIndex, hoverIndex) {
    | (Some(dragIndex), Some(hoverIndex)) =>
      onMoveCard(dragIndex, hoverIndex)
    | (_, _) => ()
    };
  };

  {
    ...component,
    render: self =>
      <BsReactDnd.DragDropContextProvider backend=BsReactDnd.Backend.html5>
        <div style=(ReactDOMRe.Style.make(~width="400", ()))>
          (
            cards
            |> List.map((card: T.card) =>
                 <Card
                   key=(string_of_int(card.id))
                   id=card.id
                   layer=card.layer
                   moveCard=handleMoveCard
                   setRef=(theRef => onSetRef(card.layer, theRef))
                 />
               )
            |> Array.of_list
            |> ReasonReact.array
          )
        </div>
      </BsReactDnd.DragDropContextProvider>,
  };
};
