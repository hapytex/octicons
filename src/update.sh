#!/bin/bash

latest=$(curl -s 'https://api.github.com/repos/primer/octicons/releases/latest' | grep 'browser_download_url' -m 1 | cut -d '"' -f 4)
wget "$latest" -q -O 'tmp.zip'

cp "Icons/Octicons.template" "Icons/Octicons.hs"
unzip -l 'tmp.zip' | grep -P -o '(?<=svg/)[^.]*(?=[.])' | sort | while read l; do
    v=$(sed -E 's/[-](\w)/\U\1/g' <<<"$l")
    echo ''
    echo "$v :: Octicon"
    echo "$v = Octicon \"$l\""
done >> "Icons/Octicons.hs"

rm 'tmp.zip'
