open Routes;
open Score;

let clamp = Canvas.clamp;

type action =
  | SetEventIndex(int)
  | AdjustEventIndex(int);

type state = {eventIndex: int};

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~score, ~startingIndexRef, _children) => {
  ...component,
  initialState: () => {eventIndex: startingIndexRef^},
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
