# GAYER: Graphical Audio plaYER

GAYER is a graphics-based subtractive synth inspired by the [ANS
Synthesizer][ans] and its many descendants. It is implemented in
[ReasonML][reason], using partial bindings to the WebAudio and Canvas APIs. In
lieu of the additive sine-wave synthesis used in many approaches to graphical
sound, it outputs audio by controlling the gains of bandpass filters whose
center frequencies are spaced in equal temperament, effectively approximating
the inverse of the [constant-Q transform][cqt] on its input. GAYER can analyze
audio and write its CQT representation into a canvas, then play it back by
reading the canvas pixels and adjusting the filters' gains to match (the
filterbank can be excited by mic input or a noise generator). By changing the
filters, alpha, and compositing mode of the canvas, various effects can be
achieved.

This software is licensed under GPL 3.0.

[ans]: https://en.wikipedia.org/wiki/ANS_synthesizer
[reason]: https://reasonml.github.io/
[cqt]: https://en.wikipedia.org/wiki/Constant-Q_transform 

## Run Project

```sh
yarn install
yarn start
# in another tab
yarn webpack
# in one more tab
http-server
```

Then navigate to http://localhost:8080 (or whatever port your local server uses).

## Build for Production

```sh
npm run build
npm run webpack:production
```

This will replace the development artifact `build/Index.js` for an optimized version.
