#!/bin/bash

status=$(sed -- 1d "$1" | sed -E 's/.*P:(.{8}).*/\1/'|sed -E 's/n/0+/; s/N/128+/; s/V/64+/; s/[uU]/32+/; s/D/8+/; s/I/4+/; s/Z/2+/; s/C/1/; s/c/0/; s/[nvbBdiz]//g'|sed '1i obase=16;'|bc)

cat -- "$1" |sed 1d|sed 's/ \+$//'|sed -E 's/(\$....):/\1 /'|sed 's/^ *\$//'|sed -E 's/(^[^ ]+  [^ ]+ [^ ]+ [^ ]+)/\1 /'|sed -E 's/(^[^ ]+  [^ ]+ [^ ]+   )/\1 /'|sed -E 's/(^[^ ]+  [^ ]+   )/\1 /'|sed -E 's/([^ ])A:/\1 A:/'|sed -E 's/( [^ ]{3}) .*A:/\1  A:/'|sed 's/ S:/ SP:/'|sed 's/ P:.*/ P:/' | paste -d '' - <(echo "$status") | sed -E 's/(SP:..) (P:..)/\2 \1/'
