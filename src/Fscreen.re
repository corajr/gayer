type t;

[@bs.module] external fscreen : t = "fscreen";

[@bs.get] external fullscreenEnabled : t => bool = "";

[@bs.get] external fullscreenElement : t => Js.Nullable.t(Dom.element) = "";

[@bs.send] external requestFullscreen : (t, Dom.element) => unit = "";

[@bs.send]
external requestFullscreenFunction : (t, Dom.element, unit) => unit = "";

[@bs.val] external exitFullscreen : t => unit = "";
