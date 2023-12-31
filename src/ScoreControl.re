open Routes;
open Score;

let clamp = Canvas.clamp;

let maybeResumeAudio = (audioCtx) => switch (audioCtx) {
  | None => ()
  | Some(audioCtx) => 
  Js.log(Audio.state(audioCtx));
  if (Audio.state(audioCtx) == "suspended") {
    Audio.resume(audioCtx);
  };
};

type action =
  | SetEventIndex(int)
  | AdjustEventIndex(int);


type state = {eventIndex: int, audioCtx: option(Audio.audioContext), onclickAdded: ref(bool)};

let setRef = (aRef, {ReasonReact.state}) =>
  if (!state.onclickAdded^) {
    switch (Js.Nullable.toOption(aRef)) {
    | Some(el) =>
      state.onclickAdded := true;
      el |> Webapi.Dom.Element.addEventListener("click", _evt => { maybeResumeAudio(state.audioCtx); });
    | None => ()
    };
};


let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~score, ~audioCtx=?, _children) => {
  ...component,
  initialState: () => {
    let url = ReasonReact.Router.dangerouslyGetInitialUrl();
    let startingIndex =
      switch (int_of_string(url.search)) {
      | i => i
      | exception _ => 0
      };

    {eventIndex: startingIndex, audioCtx: audioCtx, onclickAdded: ref(false)};
  },
  reducer: (action, state) =>
    switch (action) {
    | SetEventIndex(i) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          eventIndex:
            clamp(~minVal=0, ~maxVal=Array.length(score.events) - 1, i),
        },
        (
          self => {
            let eventIndex = self.state.eventIndex;
            pushParamsState(
              ~maybeI=Some(eventIndex),
              score.events[eventIndex].params,
            );
            Js.log({j|Set score index; score now at $(eventIndex)|j});
          }
        ),
      )
    | AdjustEventIndex(i) =>
      ReasonReact.UpdateWithSideEffects(
        {
          ...state,
          eventIndex:
            clamp(
              ~minVal=0,
              ~maxVal=Array.length(score.events) - 1,
              i + state.eventIndex,
            ),
        },
        (
          self => {
            let eventIndex = self.state.eventIndex;
            pushParamsState(
              ~maybeI=Some(eventIndex),
              score.events[eventIndex].params,
            );
            Js.log({j|Set score index; score now at $(eventIndex)|j});
          }
        ),
      )
    },

  render: self =>
    MaterialUi.(
      <div
        ref=(self.handle(setRef))
        style=(
          ReactDOMRe.Style.make(
            ~display="flex",
            ~flexDirection="row",
            ~flexWrap="nowrap",
            (),
          )
        )>
        <IconButton
          color=`Inherit onClick=(_evt => self.send(AdjustEventIndex(-1)))>
          <MaterialUIIcons.SkipPrevious />
        </IconButton>
        <span style=(ReactDOMRe.Style.make(~width="4em", ()))>
          <NumericTextField
            label=ReasonReact.null
            value=(`Int(self.state.eventIndex))
            onChange=(i => self.send(SetEventIndex(int_of_float(i))))
          />
        </span>
        <IconButton
          color=`Inherit onClick=(_evt => self.send(AdjustEventIndex(1)))>
          <MaterialUIIcons.SkipNext />
        </IconButton>
      </div>
    ),
};
