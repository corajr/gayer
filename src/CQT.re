/* Wrapper for showcqtbar.js implementation by Muhammad Faiz. */

type t;

type float32Array = array(float);
type uint8ClampedArray = array(int);

type cqtBarParams = {
  rate: float,
  width: int,
  height: int,
  barVolume: float,
  sonogramVolume: float,
  supersampling: bool,
};

[@bs.new]
external _createShowCQTBar : (float, int, int, float, float, bool) => t =
  "ShowCQTBar";

let createShowCQTBar: cqtBarParams => t =
  p =>
    _createShowCQTBar(
      p.rate,
      p.width,
      p.height,
      p.barVolume,
      p.sonogramVolume,
      p.supersampling,
    );

[@bs.send]
external getInputArray : (t, int) => float32array = "get_input_array";

[@bs.send]
external getOutputArray : t => uint8ClampedArray = "get_output_array";

[@bs.send] external calc : t => unit = "";

[@bs.send] external renderLine : (t, int) => unit = "render_line";

[@bs.send] external setVolume : (t, float, float) => unit = "set_volume";
