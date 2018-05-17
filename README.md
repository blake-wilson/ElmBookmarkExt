## Building from source
The following dependencies are required to build this extension
- [Elm](https://guide.elm-lang.org/install.html)
- Node (`brew install node`)
- Gulp (`npm install gulp`)

Once Elm is installed, run `elm-package install` to download the Elm package dependencies.
To copy the static resource files required by the extension into the build (`dist`) directory,
run `gulp copy-static`. Once these steps are complete, the source can be compiled into the
build directory.

To build the extension and run a file watcher which will rebuild when source
files change, run
```
gulp dev
```

## Loading the Chrome Extension
Navigate to `chrome://extensions/` and select `LOAD UNPACKED`.
In the file selection dialog, navigate to the `dist` directory of this repository and choose
`Select`.
