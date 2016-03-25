#!/usr/bin/sh
# closure-compiler ~/git/ef/obvyus/.stack-work/install/x86_64-osx/lts-5.2/ghcjs-0.2.0.20151230.3_ghc-7.10.2/bin/obvyus.jsexe/all.js --js_output_file ~/Sites/main.js
cp ~/git/ef/obvyus/.stack-work/install/x86_64-osx/lts-5.2/ghcjs-0.2.0.20151230.3_ghc-7.10.2/bin/obvyus.jsexe/all.js ~/Sites/main.js
echo "Success; compiled to JS and moved to ~/Sites/main.js"
osascript -e 'tell application "Google Chrome" to reload active tab of window 1'
osascript -e 'tell application "Google Chrome" to Activate'
