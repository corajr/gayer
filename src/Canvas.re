/* Modified from https://github.com/reasonml-community/reason-maze/blob/master/src/FFI/Canvas.re */

type canvasRenderingContext2D;
type ctx = canvasRenderingContext2D;
type canvasElement;

type canvasImageSource;

type image;

let defaultSize = 480;

[@bs.deriving abstract]
type imageData =
  pri {
    data: array(int),
    width: int,
    height: int,
  };

type channel =
  | R
  | G
  | B
  | A;

let int_of_channel = channel =>
  switch (channel) {
  | R => 0
  | G => 1
  | B => 2
  | A => 3
  };

let channel_of_int = int =>
  switch (int) {
  | 0 => R
  | 1 => G
  | 2 => B
  | 3 => A
  | _ => R
  };

type pixel = {
  r: float,
  g: float,
  b: float,
  a: float,
};

type dim = int;

type rect = {
  x: dim,
  y: dim,
  w: dim,
  h: dim,
};

type compositeOperation =
  | SourceOver
  | SourceIn
  | SourceOut
  | SourceAtop
  | DestinationOver
  | DestinationIn
  | DestinationOut
  | DestinationAtop
  | Lighter
  | Copy
  | Xor
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  | ColorDodge
  | ColorBurn
  | HardLight
  | SoftLight
  | Difference
  | Exclusion
  | Hue
  | Saturation
  | Color
  | Luminosity;

let string_of_compositeOperation: compositeOperation => string =
  fun
  | SourceOver => "source-over"
  | SourceIn => "source-in"
  | SourceOut => "source-out"
  | SourceAtop => "source-atop"
  | DestinationOver => "destination-over"
  | DestinationIn => "destination-in"
  | DestinationOut => "destination-out"
  | DestinationAtop => "destination-atop"
  | Lighter => "lighter"
  | Copy => "copy"
  | Xor => "xor"
  | Multiply => "multiply"
  | Screen => "screen"
  | Overlay => "overlay"
  | Darken => "darken"
  | Lighten => "lighten"
  | ColorDodge => "color-dodge"
  | ColorBurn => "color-burn"
  | HardLight => "hard-light"
  | SoftLight => "soft-light"
  | Difference => "difference"
  | Exclusion => "exclusion"
  | Hue => "hue"
  | Saturation => "saturation"
  | Color => "color"
  | Luminosity => "luminosity";

let compositeOperation_of_string: string => compositeOperation =
  fun
  | "source-over" => SourceOver
  | "source-in" => SourceIn
  | "source-out" => SourceOut
  | "source-atop" => SourceAtop
  | "destination-over" => DestinationOver
  | "destination-in" => DestinationIn
  | "destination-out" => DestinationOut
  | "destination-atop" => DestinationAtop
  | "lighter" => Lighter
  | "copy" => Copy
  | "xor" => Xor
  | "multiply" => Multiply
  | "screen" => Screen
  | "overlay" => Overlay
  | "darken" => Darken
  | "lighten" => Lighten
  | "color-dodge" => ColorDodge
  | "color-burn" => ColorBurn
  | "hard-light" => HardLight
  | "soft-light" => SoftLight
  | "difference" => Difference
  | "exclusion" => Exclusion
  | "hue" => Hue
  | "saturation" => Saturation
  | "color" => Color
  | "luminosity" => Luminosity
  | _ => SourceOver;

type transformMatrix = {
  horizontalScaling: float,
  horizontalSkewing: float,
  verticalSkewing: float,
  verticalScaling: float,
  horizontalMoving: float,
  verticalMoving: float,
};

let defaultTransform: transformMatrix = {
  horizontalScaling: 1.0,
  horizontalSkewing: 0.0,
  verticalSkewing: 0.0,
  verticalScaling: 1.0,
  horizontalMoving: 0.0,
  verticalMoving: 0.0,
};

/* Only supported in Chrome and Firefox. */

type percentage = int;

type length = string;
type degree = string;
type color = string;

let rgba: (int, int, int, float) => color =
  (r, g, b, a) =>
    "rgba("
    ++ Js.Int.toString(r)
    ++ ","
    ++ Js.Int.toString(g)
    ++ ","
    ++ Js.Int.toString(b)
    ++ ","
    ++ Js.Float.toString(a)
    ++ ")";

type filter =
  | Url(string)
  | Blur(length)
  | Brightness(percentage)
  | Contrast(percentage)
  | DropShadow(length, length, length, color)
  | Grayscale(percentage)
  | HueRotate(degree)
  | Invert(percentage)
  | Opacity(percentage)
  | Saturate(percentage)
  | Sepia(percentage)
  | NoFilter;

let string_of_filter: filter => string =
  fun
  | Url(url) => "url(" ++ url ++ ")"
  | Blur(length) => "blur(" ++ length ++ ")"
  | Brightness(percentage) =>
    "brightness(" ++ Js.Int.toString(percentage) ++ "%)"
  | Contrast(percentage) =>
    "contrast(" ++ Js.Int.toString(percentage) ++ "%)"
  | DropShadow(offsetX, offsetY, blurRadius, color) =>
    "drop-shadow("
    ++ Rationale.RList.join(" ", [offsetX, offsetY, blurRadius, color])
    ++ ")"
  | Grayscale(percentage) =>
    "grayscale(" ++ Js.Int.toString(percentage) ++ "%)"
  | HueRotate(degree) => "hue-rotate(" ++ degree ++ ")"
  | Invert(percentage) => "invert(" ++ Js.Int.toString(percentage) ++ "%)"
  | Opacity(percentage) => "opacity(" ++ Js.Int.toString(percentage) ++ "%)"
  | Saturate(percentage) =>
    "saturate(" ++ Js.Int.toString(percentage) ++ "%)"
  | Sepia(percentage) => "sepia(" ++ Js.Int.toString(percentage) ++ "%)"
  | NoFilter => "none";

external getFromReact : Dom.element => canvasElement = "%identity";

[@bs.get] external canvasWidth : canvasElement => int = "width";
[@bs.get] external canvasHeight : canvasElement => int = "height";

[@bs.send] external toDataURL : canvasElement => string = "";

[@bs.send]
external getContext :
  (canvasElement, [@bs.as "2d"] _) => canvasRenderingContext2D =
  "getContext";

external getCanvasImageSource : image => canvasImageSource = "%identity";

[@bs.new]
external createImageData : (array(int), int, int) => imageData = "ImageData";

/* canvas api methods */
module Ctx = {
  [@bs.get] external canvas : ctx => canvasElement = "";

  [@bs.set]
  external _setGlobalCompositeOperation : (ctx, string) => unit =
    "globalCompositeOperation";

  let setGlobalCompositeOperation = (ctx, compositeOperation) =>
    _setGlobalCompositeOperation(
      ctx,
      string_of_compositeOperation(compositeOperation),
    );

  [@bs.set] external setGlobalAlpha : (ctx, float) => unit = "globalAlpha";

  [@bs.set] external setFillStyle : (ctx, string) => unit = "fillStyle";

  [@bs.set] external setStrokeStyle : (ctx, string) => unit = "strokeStyle";

  [@bs.set] external setStrokeWidth : (ctx, dim) => unit = "lineWidth";

  [@bs.set] external setLineWidth : (ctx, dim) => unit = "lineWidth";

  [@bs.set] external setLineCap : (ctx, string) => unit = "lineCap";

  [@bs.set] external setFont : (ctx, string) => unit = "font";

  [@bs.set] external _setFilter : (ctx, string) => unit = "filter";

  let setFilter: (ctx, list(filter)) => unit =
    (ctx, filters) =>
      _setFilter(
        ctx,
        Rationale.RList.join(" ", List.map(string_of_filter, filters)),
      );

  [@bs.send]
  external _setTransform :
    (ctx, float, float, float, float, float, float) => unit =
    "setTransform";

  let setTransform: (ctx, transformMatrix) => unit =
    (
      ctx,
      {
        horizontalScaling,
        horizontalSkewing,
        verticalSkewing,
        verticalScaling,
        horizontalMoving,
        verticalMoving,
      },
    ) =>
      _setTransform(
        ctx,
        horizontalScaling,
        horizontalSkewing,
        verticalSkewing,
        verticalScaling,
        horizontalMoving,
        verticalMoving,
      );

  /* void ctx.drawImage(image, dx, dy); */
  [@bs.send]
  external drawImage : (ctx, canvasImageSource, dim, dim) => unit = "";

  /* void ctx.drawImage(image, dx, dy, dWidth, dHeight); */
  [@bs.send]
  external drawImageDestRect :
    (ctx, canvasImageSource, dim, dim, dim, dim) => unit =
    "drawImage";

  /* void ctx.drawImage(image, sx, sy, sWidth, sHeight, dx, dy, dWidth, dHeight); */
  [@bs.send]
  external drawImageSourceRectDestRect :
    (ctx, canvasImageSource, dim, dim, dim, dim, dim, dim, dim, dim) => unit =
    "drawImage";

  [@bs.send] external fillRect : (ctx, dim, dim, dim, dim) => unit = "";

  [@bs.send] external fillText : (ctx, string, dim, dim) => unit = "";

  [@bs.send] external strokeRect : (ctx, dim, dim, dim, dim) => unit = "";

  [@bs.send] external clearRect : (ctx, dim, dim, dim, dim) => unit = "";

  [@bs.send]
  external ellipse : (ctx, dim, dim, dim, dim, float, float, float) => unit =
    "";

  let circle = (ctx, x, y, r) =>
    ellipse(ctx, x, y, r, r, 0.0, 0.0, 2.0 *. [%bs.raw "Math.PI"]);

  [@bs.send] external moveTo : (ctx, dim, dim) => unit = "";

  [@bs.send] external arc : (ctx, dim, dim, dim, dim, dim) => unit = "";

  [@bs.send]
  external arc_rev :
    (ctx, dim, dim, dim, dim, dim, [@bs.as {json|true|json}] _) => unit =
    "arc";

  let moveToPos = (ctx, (x, y)) => moveTo(ctx, x, y);

  [@bs.send] external lineTo : (ctx, dim, dim) => unit = "";

  let lineToPos = (ctx, (x, y)) => lineTo(ctx, x, y);

  [@bs.send] external fill : ctx => unit = "";

  [@bs.send] external beginPath : ctx => unit = "";

  [@bs.send] external stroke : ctx => unit = "";

  let line = (ctx, (x, y), (a, b)) => {
    beginPath(ctx);
    moveTo(ctx, x, y);
    lineTo(ctx, a, b);
    stroke(ctx);
  };

  /* https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/getImageData */
  /* getImageData(sx, sy, sw, sh); */
  [@bs.send]
  external getImageData : (ctx, dim, dim, dim, dim) => imageData = "";

  /* https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/putImageData */
  /* putImageData(imageData, dx, dy); */
  [@bs.send] external putImageData : (ctx, imageData, dim, dim) => unit = "";
};

external getCanvasAsSource : canvasElement => canvasImageSource = "%identity";

external getElementAsImageSource : Dom.element => canvasImageSource =
  "%identity";

let simpleDrawImage =
    (
      ~ctx: ctx,
      ~image: image,
      ~compositeOperation: compositeOperation,
      ~alphaValue: float,
    ) => {
  Ctx.setGlobalCompositeOperation(ctx, compositeOperation);
  Ctx.setGlobalAlpha(ctx, alphaValue);
  Ctx.drawImage(ctx, getCanvasImageSource(image), 0, 0);
};

let mapRawData: (array(int), (array(int), int) => 't) => array('t) =
  (rawData, f) => {
    let n = Array.length(rawData) / 4;
    Array.init(
      n,
      i => {
        let offset = i * 4;
        f(rawData, offset);
      },
    );
  };

let mapImageData: (imageData, (array(int), int) => 't) => array('t) =
  (imageData, f) => mapRawData(imageData |. data, f);

let rawDataToPixel = (rawData, offset) => {
  r: float_of_int(rawData[offset + int_of_channel(R)]) /. 255.0,
  g: float_of_int(rawData[offset + int_of_channel(G)]) /. 255.0,
  b: float_of_int(rawData[offset + int_of_channel(B)]) /. 255.0,
  a: float_of_int(rawData[offset + int_of_channel(A)]) /. 255.0,
};

let imageDataToPixels: imageData => array(pixel) =
  imageData => mapImageData(imageData, rawDataToPixel);

let rawDataToFloatArray = (channel, invert) => {
  let channelOffset = int_of_channel(channel);
  (rawData, offset) => {
    let v = float_of_int(rawData[offset + channelOffset]) /. 255.0;
    invert ? 1.0 -. v : v;
  };
};

let imageDataToFloatArray: (imageData, channel) => array(float) =
  (imageData, channel) =>
    mapImageData(imageData, rawDataToFloatArray(channel, channel === A));

let makeUint8ClampedArray = [%bs.raw
  len => {|return new Uint8ClampedArray(len)|}
];

let makeImageData = (~cqtLine: array(int)) => {
  let len = Array.length(cqtLine);
  let n = len / 4;
  let output = makeUint8ClampedArray(len);

  for (i in 0 to n - 1) {
    let offset = i * 4;
    let cqtVal = cqtLine[(n - i - 1) * 4];

    output[offset + int_of_channel(R)] = cqtVal;
    output[offset + int_of_channel(G)] = cqtVal;
    output[offset + int_of_channel(B)] = cqtVal;
    output[offset + int_of_channel(A)] = 255;
  };

  createImageData(output, 1, n);
};

let makeImageDataFromFloats: (array(float), int, int) => imageData =
  (input, w, h) => {
    let n = Array.length(input);
    let len = n * 4;
    let output = makeUint8ClampedArray(len);
    for (i in 0 to n - 1) {
      let offset = i * 4;
      let v = int_of_float(input[n - i - 1] *. 255.0);
      output[offset + int_of_channel(R)] = v;
      output[offset + int_of_channel(G)] = v;
      output[offset + int_of_channel(B)] = v;
      output[offset + int_of_channel(A)] = 255;
    };
    createImageData(output, w, h);
  };

let loadImage: (~url: string, ~onLoad: canvasImageSource => unit) => unit = [%bs.raw
  (src, onLoad) => {|
     var img = new Image;

     img.crossOrigin = "Anonymous";

     img.onload = function() {
       onLoad(img);
     }

     img.src = src;
     // make sure the load event fires for cached images too
     if ( img.complete || img.complete === undefined ) {
     img.src = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw==";
     img.src = src;
}
     |}
];

let wrapCoord: (int, int, int) => int =
  (index, delta, size) => {
    let newCoord = index + delta;
    if (newCoord >= 0 && newCoord < size) {
      newCoord;
    } else if (newCoord >= 0) {
      newCoord mod size;
    } else {
      size - abs(newCoord mod size);
    };
  };

let binsPerSemitone: int => int = height => height / 120;

module DrawCommand = {
  type imgSource =
    | Self;

  type length =
    | Pixels(int)
    | Note(int)
    | Width
    | Height;

  type rect = {
    x: length,
    y: length,
    w: length,
    h: length,
  };

  type command =
    | SetFillStyle(string)
    | FillRect(rect)
    | DrawImage(imgSource, rect);

  module EncodeDrawCommand = {
    let imgSource = _r => Json.Encode.string("self");
    let length =
      Json.Encode.(
        fun
        | Pixels(i) => int(i)
        | Note(i) => object_([("type", string("note")), ("note", int(i))])
        | Width => string("width")
        | Height => string("height")
      );

    let rect = r =>
      Json.Encode.(
        object_([
          ("x", length(r.x)),
          ("y", length(r.y)),
          ("w", length(r.w)),
          ("h", length(r.h)),
        ])
      );

    let command =
      Json.Encode.(
        fun
        | SetFillStyle(s) =>
          object_([
            ("type", string("SetFillStyle")),
            ("style", string(s)),
          ])
        | FillRect(r) =>
          object_([("type", string("FillRect")), ("rect", rect(r))])
        | DrawImage(src, r) =>
          object_([
            ("type", string("DrawImage")),
            ("src", imgSource(src)),
            ("destRect", rect(r)),
          ])
      );
  };

  module DecodeDrawCommand = {
    let imgSource = json =>
      Json.Decode.(
        switch (string(json)) {
        | _ => Self
        }
      );

    let length = json =>
      json
      |> Json.Decode.(
           oneOf([
             map(i => Pixels(i), int),
             map(
               fun
               | "width" => Width
               | "height" => Height
               | _ => Pixels(0),
               string,
             ),
             map(i => Note(i), field("note", int)),
           ])
         );

    let rect = json =>
      Json.Decode.{
        x: json |> field("x", length),
        y: json |> field("y", length),
        w: json |> field("w", length),
        h: json |> field("h", length),
      };

    let commandByType: (string, Js.Json.t) => command =
      (type_, json) =>
        Json.Decode.(
          switch (type_) {
          | "SetFillStyle" =>
            json |> map(s => SetFillStyle(s), field("style", string))
          | "DrawImage" =>
            json
            |> (
              field("src", imgSource)
              |> andThen(src =>
                   map(
                     rect_ => DrawImage(src, rect_),
                     field("destRect", rect),
                   )
                 )
            )
          | "FillRect" => json |> map(r => FillRect(r), field("rect", rect))
          | _ =>
            raise @@
            DecodeError(
              "Expected layer content, got " ++ Js.Json.stringify(json),
            )
          }
        );

    let command: Js.Json.t => command =
      json =>
        json |> Json.Decode.(field("type", string) |> andThen(commandByType));
  };

  let getLength: (ctx, length) => int =
    (ctx, len) =>
      switch (len) {
      | Pixels(i) => i
      | Note(i) =>
        let height = canvasHeight(Ctx.canvas(ctx));
        let pixelsPerSemitone = binsPerSemitone(height);
        height - i * pixelsPerSemitone;
      | Width => canvasWidth(Ctx.canvas(ctx))
      | Height => canvasHeight(Ctx.canvas(ctx))
      };

  let drawCommand: (ctx, command) => unit =
    (ctx, cmd) =>
      switch (cmd) {
      | SetFillStyle(style) => Ctx.setFillStyle(ctx, style)
      | FillRect({x, y, w, h}) =>
        Ctx.fillRect(
          ctx,
          getLength(ctx, x),
          getLength(ctx, y),
          getLength(ctx, w),
          getLength(ctx, h),
        )
      | DrawImage(src, {x, y, w, h}) =>
        switch (src) {
        | Self =>
          Ctx.drawImageDestRect(
            ctx,
            getCanvasAsSource(Ctx.canvas(ctx)),
            getLength(ctx, x),
            getLength(ctx, y),
            getLength(ctx, w),
            getLength(ctx, h),
          )
        }
      };

  let drawCommands: (ctx, list(command)) => unit =
    (ctx, cmds) => List.iter(drawCommand(ctx), cmds);
};
