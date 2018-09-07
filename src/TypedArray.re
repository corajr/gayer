type arrayBuffer;

[@bs.deriving abstract]
type t = pri {buffer: arrayBuffer};

type int8Array;
type uint8Array;
type uint8ClampedArray;
type int16Array;
type uint16Array;
type int32Array;
type uint32Array;
type float32Array;
type float64Array;

[@bs.new] external createFloat32Array : int => float32Array = "Float32Array";

external floatArrayAsArray : float32Array => array(float) = "%identity";

let toFloat32Array = [%bs.raw t => "return new Float32Array(t.buffer);"];

let float32toUint8ClampedArray = [%bs.raw
  t => "return new Uint8ClampedArray(t.buffer);"
];
