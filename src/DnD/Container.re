module RList = Rationale.RList;

open Dragula;

type state = {
  dragContainerRef: ref(option(Dom.element)),
  dragulaRef: ref(option(drake)),
};

let defaultState = {dragContainerRef: ref(None), dragulaRef: ref(None)};

let component = ReasonReact.reducerComponent("Container");

let make =
    (~cards, ~onMoveCard, ~onChangeLayer, ~onSetRef, ~getAudio, _children) => {
  let handleCardsChange = ids => {
    let idToLayer =
      List.fold_left(
        (acc, {T.id, T.layer}) => Belt.Map.String.set(acc, id, layer),
        Belt.Map.String.empty,
        cards,
      );

    let newLayers =
      List.fold_left(
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
      switch (el) {
      | Some(el) =>
        let elId = ReactDOMRe.domElementToObj(el)##id;
        let ids = List.map(({T.id}) => id, cards);
        let elIndex =
          switch (RList.indexOf(elId, ids)) {
          | Some(i) => i
          | None => raise(Not_found)
          };
        let listMinusEl = ids |> RList.remove(elIndex, 1);
        let siblingIndex =
          switch (sibling) {
          | Some(sibling) =>
            let sibId = ReactDOMRe.domElementToObj(sibling)##id;
            RList.indexOf(sibId, listMinusEl);
          | None => Some(List.length(listMinusEl))
          };
        switch (siblingIndex) {
        | Some(siblingIndex) =>
          handleCardsChange(RList.insert(siblingIndex, elId, listMinusEl))
        | None => raise(Not_found)
        };
      | None => ()
      };

  let connectDragula = (state, onUnmount) =>
    switch (state.dragulaRef^, state.dragContainerRef^) {
    | (None, Some(dragContainer)) =>
      let drake =
        dragula(
          [|dragContainer|],
          options(
            ~invalid=
              (~el, ~handle) => {
                let obj = ReactDOMRe.domElementToObj(handle);
                obj##tagName === "BUTTON"
                || obj##getAttribute("role") === "slider";
              },
            (),
          ),
        );
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
                 changeLayer=onChangeLayer
                 getAudio
                 setRef=(theRef => onSetRef(card.layer, theRef))
               />
             )
          |> Array.of_list
          |> ReasonReact.array
        )
      </div>,
  };
};
