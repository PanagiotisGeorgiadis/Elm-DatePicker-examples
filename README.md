# :calendar: [Elm-DatePicker](https://package.elm-lang.org/packages/PanagiotisGeorgiadis/elm-datepicker/latest/) examples

This repository is an attempt to write down some simple and some more advanced cases of using the `Elm-DatePicker` package.
It also includes __default styling__ for the picker in case you'd like to use it.

### Dev Instructions
If you'd like to play around with the examples clone the repo and run `npm install && npm start`.
<br/>
This should start the `parceljs` bundler which will handle the `watch`, `bundle` and `serve` of your assets.


### Build
If you'd like to build the assets to move them to a different project or include the styling on your project
you can clone the repo and run `npm install && npm build`. This _should_ create a `build` folder containing
all the files you'll need. If you only want to use the styles you can copy the `./build/main.<hash>.css` file and
use it on your project.

### Known issues and how to solve them
I've encountered some intermittent issues that seem to be related to the `filesystem` or to the `node-elm-compiler`
dependency used by `parceljs`.

<br/>
#### Filesystem related issues:
 - **The process cannot access the file because it is being used by another process**
 - **Error: EBUSY: resource busy or locked** <br/>

It seems that these errors can be fixed *( at least at the moment of writing )* by following these two steps:
 1) Remove the `elm-stuff` and `build` directories from the project.
 2) Run `npm run build`.

<br/>
<strike>
#### Elm related issues:
The only Elm related issue I've encountered was the ***well known*** `elm: Map.!: given key is not an element in the map`.
This issue is known to be an issue in the `0.19.0` version of the compiler and it seems to be triggered by the `--debug` flag passed onto the cli. It seems like the `node-elm-compiler` module used by `parceljs` is using the `--debug` flag when using `serve` or `watch`.<br/>
In order to mitigate for that while on the `dev` environment I suggest to do the following:

1) Open the file located at `./node-modules/node-elm-compiler/dist/index.js`
2) Search for `return ["--debug"]` and replace it with `return []`

**Note:** This will unfortunately disable the elm live debugger but it ***should*** fix the error mentioned above.
</strike>

The issue mentioned above seems to be fixed by bumping the Elm version to **0.19.1**. I've also added the Elm dependency as an npm dependency for ease of use.
