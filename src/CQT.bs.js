// Generated by BUCKLESCRIPT VERSION 4.0.3, PLEASE EDIT WITH CARE


function createShowCQTBar(p) {
  return new ShowCQTBar(p[/* bits */0], p[/* rate */1], p[/* width */2], p[/* height */3], p[/* barVolume */4], p[/* sonogramVolume */5], p[/* supersampling */6]);
}

var defaultCqtBarParams = /* record */[
  /* bits */12,
  /* rate */44100.0,
  /* width */120,
  /* height */1,
  /* barVolume */10.0,
  /* sonogramVolume */18.0,
  /* supersampling */false
];

export {
  defaultCqtBarParams ,
  createShowCQTBar ,
  
}
/* No side effect */
