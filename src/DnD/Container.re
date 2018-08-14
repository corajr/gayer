module RList = Rationale.RList;

open Dragula;

type state = {
  cards: ref(list(T.card)),
  dragContainerRef: ref(option(Dom.element)),
  dragulaRef: ref(option(drake)),
};

let defaultState = {
  cards: ref([]),
  dragContainerRef: ref(None),
  dragulaRef: ref(None),
};

let component = ReasonReact.reducerComponent("Container");

let make =
    (
      ~cards,
      ~onMoveCard,
      ~onChangeLayer,
      ~onSetRef,
      ~getAudio,
      ~rootWidth,
      ~rootHeight,
      ~millisPerAudioTick,
      ~saveTick,
      _children,
    ) => {
  let handleCardsChange = (state, ids) => {
    let idToLayer =
      List.fold_left(
        (acc, {T.id, T.layer}) => Belt.Map.String.set(acc, id, layer),
        Belt.Map.String.empty,
        state.cards^,
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

  let makeDropFn: state => dropFn =
    (state, ~el, ~target, ~source, ~sibling) => {
      let ids = List.map(({T.id}) => id, state.cards^);

      switch (el) {
      | Some(el) =>
        let elId = ReactDOMRe.domElementToObj(el)##id;
        switch (RList.indexOf(elId, ids)) {
        | Some(elIndex) =>
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
            handleCardsChange(
              state,
              RList.insert(siblingIndex, elId, listMinusEl),
            )
          | None => ()
          };
        | None => ()
        };
      | None => ()
      };
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
      onDrop(drake, makeDropFn(state));
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
    willReceiveProps: self => {
      self.state.cards := cards;
      self.state;
    },
    reducer: ((), _state) => ReasonReact.NoUpdate,
    render: self =>
      <div ref=(self.handle(dragulaDecorator))>
        (
          cards
          |> List.map((card: T.card) =>
               <div
                 key=card.id
                 id=card.id
                 style=(ReactDOMRe.Style.make(~marginBottom="16px", ()))>
                 <Layer
                   layer=card.layer
                   changeLayer=onChangeLayer
                   getAudio
                   width=rootWidth
                   height=rootHeight
                   millisPerTick=millisPerAudioTick
                   setRef=(theRef => onSetRef(card.layer, theRef))
                   saveTick
                 />
               </div>
             )
          |> Array.of_list
          |> ReasonReact.array
        )
      </div>,
  };
};
