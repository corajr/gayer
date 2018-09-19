type t;

[@bs.deriving abstract]
type options = {
  width: int,
  height: int,
  fontSize: int,
  forceSquareRatio: bool,
};

[@bs.new] [@bs.module "rot-js"] external rotDisplay : options => t = "Display";

let defaultOptions =
  options(~width=10, ~height=10, ~fontSize=24, ~forceSquareRatio=true);

[@bs.send] external getContainer : t => Dom.element = "";

[@bs.send] external drawText : (t, int, int, string) => unit = "";
