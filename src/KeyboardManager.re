open Webapi.Dom;

type keyboardEventCallback = Dom.keyboardEvent => unit;

type keyboardManagerState = {
  keyDownListener: ref(option(keyboardEventCallback)),
  keyUpListener: ref(option(keyboardEventCallback)),
};

open Rationale.Option.Infix;

let addKeyListenersToBody =
    (
      ~keyDownListener: keyboardEventCallback,
      ~keyUpListener: keyboardEventCallback,
      keyboardManagerState: keyboardManagerState,
    ) => {
  let maybeBody: option(Element.t) =
    document |> Document.asHtmlDocument >>= HtmlDocument.body;
  switch (maybeBody) {
  | None => ()
  | Some(bodyEl) =>
    Element.addKeyDownEventListener(keyDownListener, bodyEl);
    keyboardManagerState.keyDownListener := Some(keyDownListener);
    Element.addKeyUpEventListener(keyUpListener, bodyEl);
    keyboardManagerState.keyUpListener := Some(keyUpListener);
  };
};

let removeKeyListenersFromBody = (keyboardManagerState: keyboardManagerState) => {
  let maybeBody: option(Element.t) =
    document |> Document.asHtmlDocument >>= HtmlDocument.body;
  switch (maybeBody) {
  | None => ()
  | Some(bodyEl) =>
    switch (keyboardManagerState.keyDownListener^) {
    | None => ()
    | Some(callback) => Element.removeKeyDownEventListener(callback, bodyEl)
    };
    switch (keyboardManagerState.keyUpListener^) {
    | None => ()
    | Some(callback) => Element.removeKeyUpEventListener(callback, bodyEl)
    };
  };
};

let key: Dom.keyboardEvent => string = e => KeyboardEvent.key(e);

let keyCode: Dom.keyboardEvent => int =
  e => {
    let k = KeyboardEvent.key(e);
    if (String.length(k) == 1) {
      Char.code(k.[0]);
    } else {
      0;
    };
  };
