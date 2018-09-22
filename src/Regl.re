type regl;

[@bs.module] external regl : Dom.element => regl = "";

type clearParams = {
  .
  "color": array(float),
  "depth": float,
};

[@bs.send] external clear : (regl, clearParams) => unit = "";

type texture;

[@bs.send]
external texture :
  (regl, [@bs.unwrap] [ | `Canvas(Dom.element) | `Js(Js.t({..}))]) =>
  texture =
  "";

type prop;

[@bs.send] external prop : (regl, string) => prop = "";

let triangleSpec = {
  "frag": {|
     precision mediump float;
     uniform vec4 color;
     void main () {
         gl_FragColor = color;
     }
  |},
  "vert": {|
     precision mediump float;
     attribute vec2 position;
     void main () {
       gl_Position = vec4(position, 0, 1);
     }
  |},
  "attributes": {
    "position": [|[|(-1), 0|], [|0, (-1)|], [|1, 1|]|],
  },
  "uniforms": {
    "color": [|1.0, 0.0, 0.0, 1.0|],
  },
  "count": 3,
};

let sobelSpec = regl => {
  "frag": {|
     precision mediump float;
     uniform sampler2D texture;
     uniform vec2 resolution;
     varying vec2 uv;

     void main () {
	float x = 1.0 / resolution.x;
	float y = 1.0 / resolution.y;
	vec4 horizEdge = vec4( 0.0 );
	horizEdge -= texture2D(texture, vec2( uv.x - x, uv.y - y ) ) * 1.0;
	horizEdge -= texture2D(texture, vec2( uv.x - x, uv.y     ) ) * 2.0;
	horizEdge -= texture2D(texture, vec2( uv.x - x, uv.y + y ) ) * 1.0;
	horizEdge += texture2D(texture, vec2( uv.x + x, uv.y - y ) ) * 1.0;
	horizEdge += texture2D(texture, vec2( uv.x + x, uv.y     ) ) * 2.0;
	horizEdge += texture2D(texture, vec2( uv.x + x, uv.y + y ) ) * 1.0;
	vec4 vertEdge = vec4( 0.0 );
	vertEdge -= texture2D(texture, vec2( uv.x - x, uv.y - y ) ) * 1.0;
	vertEdge -= texture2D(texture, vec2( uv.x    , uv.y - y ) ) * 2.0;
	vertEdge -= texture2D(texture, vec2( uv.x + x, uv.y - y ) ) * 1.0;
	vertEdge += texture2D(texture, vec2( uv.x - x, uv.y + y ) ) * 1.0;
	vertEdge += texture2D(texture, vec2( uv.x    , uv.y + y ) ) * 2.0;
	vertEdge += texture2D(texture, vec2( uv.x + x, uv.y + y ) ) * 1.0;
	vec3 edge = sqrt((horizEdge.rgb * horizEdge.rgb) + (vertEdge.rgb * vertEdge.rgb));

	gl_FragColor = vec4( edge, texture2D(texture, uv ).a );
     }
     |},
  "vert": {|
     precision mediump float;
     attribute vec2 position;
     varying vec2 uv;
     void main () {
     uv = position;
     gl_Position = vec4(1.0 - 2.0 * position, 0, 1);
     }
     |},
  "attributes": {
    "position": [|((-2), 0), (0, (-2)), (2, 2)|],
  },
  "uniforms": {
    "resolution": prop(regl, "resolution"),
    "texture": prop(regl, "texture"),
  },
  "count": 3,
};

type drawCommand;

let makeDrawCommand = [%bs.raw
  (regl, spec) => {|
     var command = regl(spec);
     command.draw = command;
     return command;
     |}
];

[@bs.send] external draw : (drawCommand, Js.t({..})) => unit = "";

type reglOptions = {sourceLayer: string};

module EncodeReglOptions = {
  let reglOptions = r =>
    Json.Encode.(object_([("sourceLayer", string(r.sourceLayer))]));
};

module DecodeReglOptions = {
  let reglOptions = json =>
    Json.Decode.{sourceLayer: json |> field("sourceLayer", string)};
};
