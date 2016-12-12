#!/usr/bin/env bash

# Compiling with ghcjs
stack build --stack-yaml=stack-ghcjs.yaml

# Moving the generated files to the js folder
mkdir -p js
cp -r $(stack path --local-install-root --stack-yaml=stack-ghcjs.yaml)/bin/starterApp.jsexe/all.js js/

# Minifying all.js file using the closure compiler, and removing unnecessary files
cd js
ccjs all.js --compilation_level=ADVANCED_OPTIMIZATIONS > all.min.js

# OPTIONAL: gzipping, to see the actual transferred size of the executable
atool -a all.min.js.tar.gz all.min.js
