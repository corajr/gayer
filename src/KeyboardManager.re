open Webapi.Dom;

type keyboardEventCallback = Dom.keyboardEvent => unit;

type keyboardManagerState = {listener: ref(option(keyboardEventCallback))};

open Rationale.Option.Infix;

let addKeyDownListenerToBody =
    (
      keyboardManagerState: keyboardManagerState,
      keyboardEventCallback: keyboardEventCallback,
    ) => {
  let maybeBody: option(Element.t) =
    document |> Document.asHtmlDocument >>= HtmlDocument.body;
  switch (maybeBody) {
  | None => ()
  | Some(bodyEl) =>
    Element.addKeyDownEventListener(keyboardEventCallback, bodyEl);
    keyboardManagerState.listener := Some(keyboardEventCallback);
  };
};

let removeKeyDownListenerFromBody =
    (keyboardManagerState: keyboardManagerState) => {
  let maybeBody: option(Element.t) =
    document |> Document.asHtmlDocument >>= HtmlDocument.body;
  switch (maybeBody, keyboardManagerState.listener^) {
  | (Some(bodyEl), Some(callback)) =>
    Element.removeKeyDownEventListener(callback, bodyEl)
  | _ => ()
  };
};

[@bs.get] external keyCode : Dom.keyboardEvent => int = "";
