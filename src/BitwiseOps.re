let readAllBitsOfInt = (~outLength: int=12, input) : array(bool) =>
  if (input <= 0) {
    Array.make(outLength, false);
  } else {
    let x = ref(input);
    let i = ref(0);
    let bits = Array.make(outLength, false);
    while (x^ > 0) {
      bits[i^] = x^ land 1 == 1;
      x := x^ lsr 1;
      i := i^ + 1;
    };
    bits;
  };

let binaryToGray: int => int = i => i lxor i lsr 1;

let grayToBinary: int => int =
  i => {
    let num = ref(i);
    let mask = ref(i lsr 1);
    while (mask^ != 0) {
      num := num^ lxor mask^;
      mask := mask^ lsr 1;
    };
    num^;
  };
