open Params;

type transition =
  | Manual;

type scoreEvent = {
  params,
  transition,
};

type score = {
  eventIndex: int,
  events: array(scoreEvent),
};

module DecodeScore = {
  let transition = json =>
    Json.Decode.(
      json
      |> map(
           fun
           | "manual" => Manual
           | _ => Manual,
           string,
         )
    );

  let scoreEvent = json =>
    Json.Decode.{
      params: json |> field("params", DecodeParams.params),
      transition: json |> field("transition", transition),
    };

  let score = json =>
    Json.Decode.{
      eventIndex: json |> field("eventIndex", int),
      events: json |> field("events", array(scoreEvent)),
    };
};

module EncodeScore = {
  let transition =
    Json.Encode.(
      fun
      | Manual => string("manual")
    );

  let scoreEvent = r =>
    Json.Encode.(
      object_([
        ("params", EncodeParams.params(r.params)),
        ("transition", transition(r.transition)),
      ])
    );

  let score = r =>
    Json.Encode.(
      object_([
        ("eventIndex", int(r.eventIndex)),
        ("events", array(scoreEvent, r.events)),
      ])
    );
};
