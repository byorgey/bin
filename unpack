#!/bin/zsh
setopt NULL_GLOB
for d in *(/); do
    echo '----------'
    echo "Unpacking in $d..."
    cd "$d"
    unzip -n *.zip &>/dev/null
    unrar e -o- *.rar >/dev/null
    rm -rf __MACOSX
    for dd in *(/); do
        mv $dd/* .
        rmdir $dd
    done
    cd ..
done
