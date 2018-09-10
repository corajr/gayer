open Jest;
open Expect;
open Params;
open Presets;
open Score;

let exampleTransitions = [Manual];
let exampleScoreEvents = [{params: defaultParams, transition: Manual}];
let exampleScores = [
  {
    events: Array.of_list(exampleScoreEvents),
    scoreMetadata: {
      title: "Example 1",
      authors: [],
    },
  },
];

describe("EncodeScore <=> DecodeScore", () => {
  describe("transition", () =>
    testAll("decode inverts encode", exampleTransitions, transition =>
      expect(DecodeScore.transition(EncodeScore.transition(transition)))
      |> toEqual(transition)
    )
  );

  describe("scoreEvent", () =>
    testAll("decode inverts encode", exampleScoreEvents, scoreEvent =>
      expect(DecodeScore.scoreEvent(EncodeScore.scoreEvent(scoreEvent)))
      |> toEqual(scoreEvent)
    )
  );

  testAll("decode inverts encode", exampleScores, score =>
    expect(DecodeScore.score(EncodeScore.score(score))) |> toEqual(score)
  );
});
