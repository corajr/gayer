type arrayBuffer;

[@bs.deriving abstract]
type t =
  pri {
    [@bs.as "buffer"]
    mutable arrayBuffer,
  };

type int8Array;
type uint8Array;
type uint8ClampedArray;
type int16Array;
type uint16Array;
type int32Array;
type uint32Array;
type float32Array = t;
type float64Array;

[@bs.new] external createFloat32Array : int => float32Array = "Float32Array";

[@bs.new] external createUint8Array : int => uint8Array = "Uint8Array";

[@bs.new]
external createUint8ClampedArray : int => uint8ClampedArray =
  "Uint8ClampedArray";

external floatArrayAsArray : float32Array => array(float) = "%identity";

external arrayFloatAsFloat32Array : array(float) => float32Array =
  "%identity";

external intArrayAsArray : uint8Array => array(int) = "%identity";

external uint8ClampedArrayAsArray : uint8ClampedArray => array(int) =
  "%identity";

let toFloat32Array = [%bs.raw t => "return new Float32Array(t.buffer);"];

let float32toUint8ClampedArray = [%bs.raw
  t => "return new Uint8ClampedArray(t.buffer);"
];

let toUint8ClampedArray = [%bs.raw
  t => "return new Uint8ClampedArray(t.buffer);"
];
