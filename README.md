# reasonml-hedgehog

This is an old project I did when I was more interested in https://reasonml.github.io/. This was a simple implementation of https://hedgehog.qa/ in ReasonML, mostly inspired by the F# version. I was trying to get the same code to compile to native code, as well as with js_of_ocaml and bucklescript.

I stopped working on it as the reason-native projects seemed to stall (both facebook repos and revery), and I generally didn't have anything to use it with. If I was doing this now I would only target rescript, or just use a newer version of the quickcheck module which includes the hedgehog-style automatic shrinking. I still think Rescript is a really good language, I'd choose it any day over typescript!

- js_of_ocaml build doesn't work any more, and I'm not sure why.
- This used to build with bucklescript, but doesn't any more with rescript:

      Signature mismatch:
      ...
      Values do not match:
        let mempty: array('_weak1)
      is not included in
        let mempty: t('a)

  the definition in question:

      let mempty: t('a);

  and the implementation:

      let mempty = [||];

  This is 'obviously correct' but for some reason it broke in a more recent release of buckescript/rescript. There's also a couple of other issues with the 'Obj' library which seem to have changed.

- The 'esy' build still works.

# Original readme

This uses some code from facebook's reason-native project https://github.com/reasonml/reason-native

```
npm install -g esy
git clone <this-repo>
esy install
esy build
```

## Running Binary:

After building the project, you can run the main binary that is produced.

```
esy x ReasonmlHedgehogApp.exe
```

## Running Tests:

```
# Runs the "test" command in `package.json`.
esy test
```

# Building via buckescript

```
npm install
npm run build
```

# Building JS via js_of_ocaml

Completely ridiculous, depends on how you build it

```
esy @js install
esy @js b refmterr dune build --root . -j 3 --verbose
esy @js run
```

## Requirements for revery

- `libx11-dev`
- `libxinerama-dev`
- `libxcursor-dev`
- `libxi-dev`
- `libglu1-mesa-dev`
- `libgl1-mesa-dev`
- `libbz2-dev`
- `libgtk-3-dev`
