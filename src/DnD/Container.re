module RList = Rationale.RList;

open Dragula;

type state = {
  dragContainerRef: ref(option(Dom.element)),
  dragulaRef: ref(option(drake)),
};

let defaultState = {dragContainerRef: ref(None), dragulaRef: ref(None)};

let component = ReasonReact.reducerComponent("Container");

let make = (~cards, ~onMoveCard, ~onSetRef, _children) => {
  let handleCardsChange = ids => {
    let idToLayer =
      List.fold_left(
        (acc, {T.id, T.layer}) => Belt.Map.String.set(acc, id, layer),
        Belt.Map.String.empty,
        cards,
      );

    let newLayers =
      Array.fold_left(
        (acc, id) =>
          switch (Belt.Map.String.get(idToLayer, id)) {
          | None => acc
          | Some(layer) => [layer, ...acc]
          },
        [],
        ids,
      );

    onMoveCard(List.rev(newLayers));
  };

  let dropFn: dropFn =
    (~el, ~target, ~source, ~sibling) =>
      switch (target) {
      | None => ()
      | Some(target) =>
        let children: array(Dom.element) = ReactDOMRe.domElementToObj(target)##childNodes;
        let childIds =
          Array.map(elt => ReactDOMRe.domElementToObj(elt)##id, children);
        handleCardsChange(childIds);
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
