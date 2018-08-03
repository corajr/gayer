module RList = Rationale.RList;

open Dragula;

type state = {
  dragContainerRef: ref(option(Dom.element)),
  dragulaRef: ref(option(drake)),
};

let defaultState = {dragContainerRef: ref(None), dragulaRef: ref(None)};

let component = ReasonReact.reducerComponent("Container");

let make = (~cards, ~onMoveCard, ~onSetRef, _children) => {
  let handleMoveCard = (idToMove, idToInsertBefore) => {
    let idList = List.map((card: T.card) => card.id, cards);
    let indexToMove = RList.indexOf(idToMove, idList);
    switch (indexToMove) {
    | None => ()
    | Some(indexToMove) =>
      let idListWithoutMovedItem = RList.remove(indexToMove, 1, idList);
      let indexToInsertBefore =
        switch (idToInsertBefore) {
        | None => Some(List.length(idListWithoutMovedItem))
        | Some(id) => RList.indexOf(id, idListWithoutMovedItem)
        };

      switch (indexToInsertBefore) {
      | None => ()
      | Some(indexToInsertBefore) =>
        Js.log(
          "moving "
          ++ string_of_int(indexToMove)
          ++ " to before "
          ++ string_of_int(indexToInsertBefore),
        );
        onMoveCard(indexToMove, indexToInsertBefore);
      };
    };
  };

  let dropFn: dropFn =
    (~el, ~target, ~source, ~sibling) =>
      switch (el, sibling) {
      | (Some(elt), None) =>
        /* moved to the end of the list */
        let eltId = ReactDOMRe.domElementToObj(elt)##id;
        handleMoveCard(eltId, None);
      | (Some(elt), Some(sib)) =>
        /* inserted before sib */
        let eltId = ReactDOMRe.domElementToObj(elt)##id;
        let sibId = ReactDOMRe.domElementToObj(sib)##id;
        handleMoveCard(eltId, Some(sibId));
      | _ => ()
      };

  let connectDragula = (state, onUnmount) =>
    switch (state.dragulaRef^, state.dragContainerRef^) {
    | (None, Some(dragContainer)) =>
      let drake = dragula([|dragContainer|], options());
      Js.log(drake);
      onDrop(drake, dropFn);
      onUnmount(() => destroy(drake));
      state.dragulaRef := Some(drake);
      ();
    | _ => ()
    };

  let dragulaDecorator = (theRef, {ReasonReact.state, ReasonReact.onUnmount}) => {
    state.dragContainerRef := Js.Nullable.toOption(theRef);
    connectDragula(state, onUnmount);
  };

  {
    ...component,
    initialState: () => defaultState,
    reducer: ((), _state) => ReasonReact.NoUpdate,
    render: self =>
      <div ref=(self.handle(dragulaDecorator))>
        (
          cards
          |> List.map((card: T.card) =>
               <Card
                 key=card.id
                 id=card.id
                 layer=card.layer
                 setRef=(theRef => onSetRef(card.layer, theRef))
               />
             )
          |> Array.of_list
          |> ReasonReact.array
        )
      </div>,
  };
};
