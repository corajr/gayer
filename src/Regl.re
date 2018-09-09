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

let sobelSpec = {
  "frag": {|
     precision mediump float;
     uniform vec4 color;
     void main () {
     gl_FragColor = vec4(1, 0, 0, 1); /* color;*/
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

type drawCommand;

let makeDrawCommand = [%bs.raw
  (regl, spec) => {|
     var command = regl(spec);
     command.draw = command;
     return command;
     |}
];

[@bs.send] external draw : (drawCommand, Js.t({..})) => unit = "";
