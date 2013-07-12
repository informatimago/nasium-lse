#!/bin/sh
cd "$(dirname "$0")/../.."
APP="$(pwd)"
Terminal_PROFILE="$APP/Contents/Resources/nasium-lse.terminal"
NasiumLSE_PGM="$APP/Contents/MacOS/lse-ccl-darwin-apple-11.3.0-i386"
exec osascript <<EOF
tell app "Terminal" 
    open "${Terminal_PROFILE}"
    do script "${NasiumLSE_PGM} --configuration-macosx-terminal ; exit" in first window
end tell
EOF
