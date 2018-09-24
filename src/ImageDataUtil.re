open Canvas;
open Color;
open ReaderType;

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

let imageDataToFloat32Array: (imageData, channel) => TypedArray.float32Array =
  (imageData, channel) => {
    let channelOffset = int_of_channel(channel);
    let rawData = imageData |. dataGet;
    let n = Array.length(rawData) / 4;
    let output =
      TypedArray.floatArrayAsArray(TypedArray.createFloat32Array(n));
    for (i in 0 to n - 1) {
      let v = float_of_int(rawData[i * 4 + channelOffset]) /. 255.0;
      output[i] = 2.0 *. v -. 1.0;
    };
    TypedArray.arrayFloatAsFloat32Array(output);
  };

let imageDataToStereo =
    (imageData, channelL, channelR)
    : (array(float), array(float)) => {
  let rawData = imageData |. dataGet;
  let n = Array.length(rawData) / 4;
  let arrayL = Array.make(n, 0.0);
  let arrayR = Array.make(n, 0.0);

  let offsetL = int_of_channel(channelL);
  let offsetR = int_of_channel(channelR);

  for (i in 0 to n - 1) {
    let offset = i * 4;
    arrayL[i] = float_of_int(rawData[offset + offsetL]) /. 255.0;
    arrayR[i] = float_of_int(rawData[offset + offsetR]) /. 255.0;
  };
  (arrayL, arrayR);
};

let imageDataToHistogram =
    (
      ~binCount: int,
      ~binFn: pixel => (int, float),
      ~divideBy: float=1.0,
      imageData: imageData,
    )
    : array(float) => {
  open Color;
  let output = Array.make(binCount, 0.0);
  let outputMax = ref(0.0);
  mapImageData(imageData, rawDataToPixel)
  |> Array.iter(pixel => {
       let (i, v) = binFn(pixel);
       output[i] = output[i] +. v;
       if (output[i] > outputMax^) {
         outputMax := output[i];
       };
     });

  let divideByFinal = max(outputMax^, divideBy);

  divideByFinal === 0.0 ? output : Array.map(x => x /. divideByFinal, output);
};

let imageDataToFilterValues =
    (
      imageData: imageData,
      filterValuesRef: ref(Audio.filterValues),
      readerType: readerType,
    )
    : unit => {
  let rawData = imageData |. dataGet;
  let n = Array.length(rawData) / 4;
  switch (readerType) {
  | Channel(channel) => ()
  | Saturation => ()
  };
};

let makeUint8ClampedArray = [%bs.raw
  len => {|return new Uint8ClampedArray(len)|}
];

let makeImageData = (~cqtLine: array(int)) => {
  let len = Array.length(cqtLine);
  let n = len / 4;
  let output = makeUint8ClampedArray(len);

  for (i in 0 to n - 1) {
    let offset = i * 4;
    let cqtOffset = (n - i - 1) * 4;

    output[offset] = cqtLine[cqtOffset];
    output[offset + 1] = cqtLine[cqtOffset + 1];
    output[offset + 2] = cqtLine[cqtOffset + 2];
    output[offset + 3] = 255;
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
