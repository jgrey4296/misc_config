#!/usr/bin/env bash

function imgconv () {
    shopt -s nullglob
    local allargs=( "$@" )
    local outname="$1"
    local imgnames=("${allargs[@]:1}")

    if [[ "${#allargs[@]}" -lt 2 ]]; then
        echo "imgconv: {output} *{files}"
        return 1
    fi
    echo "After Arg test"

    if [[ ! -d "$HOME/.temp/imgconv" ]]; then
        echo "Making temp dir"
        mkdir "$HOME/.temp/imgconv"
    else
        echo "temp dir exists"
    fi

    for target in "${imgnames[@]}"
    do
        local base=`basename "$target"`
        echo "Converting: $base"
        convert "$target" -alpha off "$HOME/.temp/imgconv/$base"
        mogrify -orient bottom-left "$HOME/.temp/imgconv/$base"
        img2pdf --output "$HOME/.temp/imgconv/$base.pdf" --rotation=ifvalid --pagesize A4 --auto-orient "$HOME/.temp/imgconv/$base"
    done
    local potentials=$(find "$HOME/.temp/imgconv/" -name "*.pdf")
    echo "Combining: ${potentials[@]}"
    pdftk ${potentials[@]} cat output "./$outname.pdf"

    read -p "Delete Temp Files? y/* : " dodel
    if [[ "$dodel" = "y" ]]; then
        echo "Deleting"
        rm -rf "$HOME/.temp/imgconv"
    fi
}
