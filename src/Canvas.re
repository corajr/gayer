/* Modified from https://github.com/reasonml-community/reason-maze/blob/master/src/FFI/Canvas.re */

type canvasRenderingContext2D;
type ctx = canvasRenderingContext2D;
type canvasElement;

type canvasPattern;

type canvasPatternRepeat =
  | Repeat
  | RepeatX
  | RepeatY
  | NoRepeat;

let string_of_canvasPatternRepeat =
  fun
  | Repeat => "repeat"
  | RepeatX => "repeat-x"
  | RepeatY => "repeat-y"
  | NoRepeat => "no-repeat";

type canvasImageSource;

type image;

let defaultSize = 240;

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

let string_of_channel =
  fun
  | R => "R"
  | G => "G"
  | B => "B"
  | A => "A";

type pixel = {
  r: float,
  g: float,
  b: float,
  a: float,
};

type dim = int;

type rotation = float;

let tau: rotation = Js.Math._PI *. 2.0;
let one_over_tau: rotation = 1.0 /. tau;

let degreesToRadians: float => rotation = degrees => degrees *. (tau /. 360.0);

let radiansToDegrees: rotation => float =
  radians => radians *. (360.0 *. one_over_tau);

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

external imageDataAsSource : imageData => canvasImageSource = "%identity";

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

  [@bs.set]
  external setFillStylePattern : (ctx, canvasPattern) => unit = "fillStyle";

  [@bs.set]
  external setStrokeStylePattern : (ctx, canvasPattern) => unit =
    "strokeStyle";

  [@bs.set] external setStrokeStyle : (ctx, string) => unit = "strokeStyle";

  [@bs.set] external setStrokeWidth : (ctx, dim) => unit = "lineWidth";

  [@bs.set] external setLineWidth : (ctx, dim) => unit = "lineWidth";

  [@bs.set] external setLineCap : (ctx, string) => unit = "lineCap";

  [@bs.set] external setFont : (ctx, string) => unit = "font";

  [@bs.set] external setTextAlign : (ctx, string) => unit = "textAlign";

  [@bs.set] external setTextBaseline : (ctx, string) => unit = "textBaseline";

  [@bs.set] external _setFilter : (ctx, string) => unit = "filter";

  [@bs.send]
  external _createPattern : (ctx, canvasImageSource, string) => canvasPattern =
    "createPattern";

  let createPattern = (ctx, src, repeat) =>
    _createPattern(ctx, src, string_of_canvasPatternRepeat(repeat));

  let setFilter: (ctx, list(filter)) => unit =
    (ctx, filters) =>
      _setFilter(
        ctx,
        Rationale.RList.join(" ", List.map(string_of_filter, filters)),
      );

  [@bs.send]
  external _transform : (ctx, float, float, float, float, float, float) => unit =
    "transform";

  let transform: (ctx, transformMatrix) => unit =
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
      _transform(
        ctx,
        horizontalScaling,
        horizontalSkewing,
        verticalSkewing,
        verticalScaling,
        horizontalMoving,
        verticalMoving,
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

  [@bs.send] external rotate : (ctx, rotation) => unit = "";

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

  [@bs.send] external strokeText : (ctx, string, dim, dim) => unit = "";

  [@bs.send] external strokeRect : (ctx, dim, dim, dim, dim) => unit = "";

  [@bs.send] external clearRect : (ctx, dim, dim, dim, dim) => unit = "";

  [@bs.send]
  external ellipse : (ctx, dim, dim, dim, dim, float, float, float) => unit =
    "";

  let circle = (ctx, x, y, r) => ellipse(ctx, x, y, r, r, 0.0, 0.0, tau);

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

  [@bs.send] external closePath : ctx => unit = "";

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

let clamp = (~minVal: int=0, ~maxVal: int=255, i: int) : int =>
  min(maxVal, max(minVal, i));

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
