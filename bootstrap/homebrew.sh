#!/bin/sh
case "$(uname -s)" in
    Linux*)     machine=linux;;
    Darwin*)    machine=mac;;
    CYGWIN*)    machine=cygwin;;
    MINGW*)     machine=mingw;;
    *)          machine="UNKNOWN:${unameOut}"
esac

if [ "$machine" = "mac" ]; then
    sudo launchctl config user path "/usr/local/bin:$PATH"
fi

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
