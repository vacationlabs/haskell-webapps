#!/usr/bin/env bash

# Compiling with ghcjs
stack build --stack-yaml=stack-ghcjs.yaml

# Moving the generated files to the js folder
rm -r js
cp -r .stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/mockClient/mockClient.jsexe/ js

# Swapping the default html with the one serving a minified version
cp assets/html/index.html js/index.html

# Minifying all.js file using the closure compiler, and removing unnecessary files
cd js
# ccjs all.js --debug --compilation_level=ADVANCED_OPTIMIZATIONS > all.min.js
ccjs all.js > all.min.js
rm all.js out.stats runmain.js lib.js out.js rts.js
