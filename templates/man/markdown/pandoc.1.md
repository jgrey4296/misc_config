pandoc -- 2.12

# USAGE

  pandoc [OPTIONS] [FILES]

# OPTIONS


    -A FILE               --include-after-body=FILE
    -B FILE               --include-before-body=FILE
    -C                    --citeproc
    -D FORMAT             --print-default-template=FORMAT
    -F PROGRAM            --filter=PROGRAM
    -H FILE               --include-in-header=FILE
    -L SCRIPTPATH         --lua-filter=SCRIPTPATH
    -M KEY[:VALUE]        --metadata=KEY[:VALUE]
    -N                    --number-sections
    -T STRING             --title-prefix=STRING
    -V KEY[:VALUE]        --variable=KEY[:VALUE]
    -c URL                --css=URL
    -d FILE               --defaults=FILE
    -f FORMAT, -r FORMAT  --from=FORMAT, --read=FORMAT
    -h                    --help
    -i                    --incremental
    -o FILE               --output=FILE
    -p                    --preserve-tabs
    -s                    --standalone
    -t FORMAT, -w FORMAT  --to=FORMAT, --write=FORMAT
    -v                    --version

                          --abbreviations=FILE
                          --ascii
                          --atx-headers
                          --base-header-level=NUMBER
                          --bash-completion
                          --biblatex
                          --bibliography=FILE
                          --citation-abbreviations=FILE
                          --columns=NUMBER
                          --csl=FILE
                          --data-dir=DIRECTORY
                          --default-image-extension=extension
                          --dpi=NUMBER
                          --dump-args
                          --email-obfuscation=none|javascript|references
                          --eol=crlf|lf|native
                          --epub-chapter-level=NUMBER
                          --epub-cover-image=FILE
                          --epub-embed-font=FILE
                          --epub-metadata=FILE
                          --epub-subdirectory=DIRNAME
                          --extract-media=PATH
                          --fail-if-warnings
                          --file-scope
                          --gladtex
                          --highlight-style=STYLE|FILE
                          --html-q-tags
                          --id-prefix=STRING
                          --ignore-args
                          --indented-code-classes=STRING
                          --ipynb-output=all|none|best
                          --katex[=URL]
                          --list-extensions[=FORMAT]
                          --list-highlight-languages
                          --list-highlight-styles
                          --list-input-formats
                          --list-output-formats
                          --listings
                          --log=FILE
                          --markdown-headings=setext|atx
                          --mathjax[=URL]
                          --mathml
                          --metadata-file=FILE
                          --natbib
                          --no-check-certificate
                          --no-highlight
                          --number-offset=NUMBERS
                          --pdf-engine-opt=STRING
                          --pdf-engine=PROGRAM
                          --print-default-data-file=FILE
                          --print-highlight-style=STYLE|FILE
                          --quiet
                          --reference-doc=FILE
                          --reference-links
                          --reference-location=block|section|document
                          --request-header=NAME:VALUE
                          --resource-path=SEARCHPATH
                          --section-divs
                          --self-contained
                          --shift-heading-level-by=NUMBER
                          --slide-level=NUMBER
                          --strip-comments
                          --strip-empty-paragraphs
                          --syntax-definition=FILE
                          --tab-stop=NUMBER
                          --template=FILE
                          --toc, --table-of-contents
                          --toc-depth=NUMBER
                          --top-level-division=section|chapter|part
                          --trace
                          --track-changes=accept|reject|all
                          --verbose
                          --webtex[=URL]
                          --wrap=auto|none|preserve


# SEE ALSO
https://pandoc.org/

# COPYRIGHT
Copyright (C) 2006-2021 John MacFarlane. Web:  https://pandoc.org
This is free software; see the source for copying conditions. There is no
warranty, not even for merchantability or fitness for a particular purpose.
