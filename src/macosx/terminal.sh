#!/bin/sh

Terminal_PROFILE="$HOME/Desktop/NASIUM-LSE.terminal"

osascript <<EOF

tell app "Terminal" to set current settings of first window to settings set "${Terminal_PROFILE}"

tell app "Terminal"
    do script "echo hello"
end tell


EOF

