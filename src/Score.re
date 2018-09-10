open Params;

type transition =
  | Manual;

type scoreEvent = {
  params,
  transition,
};

type scoreMetadata = {
  title: string,
  authors: list(string),
};

type score = {
  events: array(scoreEvent),
  scoreMetadata,
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

  let scoreMetadata = json =>
    Json.Decode.{
      title: json |> field("title", string),
      authors: json |> field("authors", list(string)),
    };

  let score = json =>
    Json.Decode.{
      events: json |> field("events", array(scoreEvent)),
      scoreMetadata: json |> field("meta", scoreMetadata),
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

  let scoreMetadata = r =>
    Json.Encode.(
      object_([
        ("title", string(r.title)),
        ("authors", list(string, r.authors)),
      ])
    );

  let score = r =>
    Json.Encode.(
      object_([
        ("events", array(scoreEvent, r.events)),
        ("meta", scoreMetadata(r.scoreMetadata)),
      ])
    );
};
