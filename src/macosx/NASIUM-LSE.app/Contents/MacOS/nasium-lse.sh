#!/bin/sh
cd "$(dirname "$0")/../.."
APP="$(pwd)"
Terminal_PROFILE="$APP/Contents/Resources/nasium-lse.terminal"
NasiumLSE_PGM="$APP/Contents/MacOS/lse-ccl-darwin-apple-14.1.0-x86-64"
exec osascript <<EOF
tell app "Terminal" 
    open "${Terminal_PROFILE}"
    do script "${NasiumLSE_PGM} --modern-mode --accept-lowcase --mixed-output --accented-output --bell --unicode-halfwidth-arrows --show-bindings ; exit" in first window
end tell
EOF
# --configuration-macosx-terminal
