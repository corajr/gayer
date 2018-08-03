type drake;

type nullableElement = Js.Nullable.t(Dom.element);
type optionalElement = option(Dom.element);

[@bs.deriving abstract]
type options = {
  [@bs.optional]
  isContainer: (~el: Dom.element) => bool,
  [@bs.optional]
  moves:
    (
      ~el: Dom.element,
      ~source: Dom.element,
      ~handle: Dom.element,
      ~sibling: Dom.element
    ) =>
    bool,
  [@bs.optional]
  accepts:
    (
      ~el: Dom.element,
      ~target: Dom.element,
      ~source: Dom.element,
      ~sibling: Dom.element
    ) =>
    bool,
  [@bs.optional]
  invalid: (~el: Dom.element, ~handle: Dom.element) => bool,
  [@bs.optional]
  direction: string,
  [@bs.optional]
  copy: bool,
  [@bs.optional]
  copySortSource: bool,
  [@bs.optional]
  revertOnSpill: bool,
  [@bs.optional]
  removeOnSpill: bool,
  [@bs.optional]
  mirrorContainer: Dom.element,
  [@bs.optional]
  ignoreInputTextSelection: bool,
};

let defaultOptions =
  options(
    ~direction="vertical",
    ~copy=false,
    ~copySortSource=false,
    ~revertOnSpill=false,
    ~removeOnSpill=false,
    ~ignoreInputTextSelection=true,
    (),
  );

[@bs.module]
external dragula : (array(Dom.element), options) => drake = "react-dragula";

type rawDropFn =
  (
    ~el: nullableElement,
    ~target: nullableElement,
    ~source: nullableElement,
    ~sibling: nullableElement
  ) =>
  unit;

type dropFn =
  (
    ~el: optionalElement,
    ~target: optionalElement,
    ~source: optionalElement,
    ~sibling: optionalElement
  ) =>
  unit;

[@bs.send]
external _onDrop : (drake, [@bs.as "drop"] _, rawDropFn) => unit = "on";

let onDrop: (drake, dropFn) => unit =
  (drake, dropFn) =>
    _onDrop(drake, (~el, ~target, ~source, ~sibling) =>
      dropFn(
        Js.Nullable.toOption(el),
        Js.Nullable.toOption(target),
        Js.Nullable.toOption(source),
        Js.Nullable.toOption(sibling),
      )
    );

[@bs.val] external destroy : drake => unit = "";
