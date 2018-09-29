open Params;

[@bs.val] external decodeURIComponent : string => string = "";
[@bs.val] external encodeURIComponent : string => string = "";

let pushParamsState = (~maybeI=None, newParams) => {
  let newParamsJson =
    encodeURIComponent(Js.Json.stringify(EncodeParams.params(newParams)));
  let maybeScoreIndex =
    switch (maybeI) {
    | Some(i) => "?" ++ string_of_int(i)
    | None => ""
    };
  ReasonReact.Router.push(maybeScoreIndex ++ "#" ++ newParamsJson);
};
