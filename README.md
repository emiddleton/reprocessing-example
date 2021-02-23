## Sandworm

I wrote this to lear how to use ReasomML and Reprocessing.

## How to

```
git clone https://github.com/emiddleton/reprocessing-example.git
git checkout sandworms
```

### Install

```
npm install
```

### Build

```
npm run build
```

### Start

```
npm start
```

This will build the bytecode executable which is at `./lib/bs/bytecode/index.byte`.

To build to JS run `npm run build:web` and then run a static server, like `python -m SimpleHTTPServer` and go to `localhost:8000`.

To build to native run `npm run build:native` and run `./lib/bs/native/index.native`

The build system used is [bsb-native](https://github.com/bsansouci/bsb-native).
