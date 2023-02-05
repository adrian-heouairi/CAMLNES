#!/bin/bash

# Expected format: $E784: 4C B3 EB JMP $EBB3A:00 X:00 Y:00 S:FD P:nvubdIzc
# Line 1 will be removed because it contains "Log Start"

status=$(sed -- 1d "$1" | sed -E 's/.*P:(.{8}).*/\1/'|sed -E 's/n/0+/; s/N/128+/; s/V/64+/; s/[uU]/32+/; s/D/8+/; s/I/4+/; s/Z/2+/; s/C/1/; s/c/0/; s/[nvbBdiz]//g'|sed '1i obase=16;'|sed 's/^$/0/'|bc)

cat -- "$1" |sed 1d|sed 's/ \+$//'|sed -E 's/(\$....):/\1 /'|sed 's/^ *\$//'|sed -E 's/(^[^ ]+  [^ ]+ [^ ]+ [^ ]+)/\1 /'|sed -E 's/(^[^ ]+  [^ ]+ [^ ]+   )/\1 /'|sed -E 's/(^[^ ]+  [^ ]+   )/\1 /'|sed -E 's/([^ ])A:/\1 A:/'|sed -E 's/( [^ ]{3}) .*A:/\1  A:/'|sed 's/ S:/ SP:/'|sed 's/ P:.*/ P:/' | paste -d '' - <(echo "$status") | sed -E 's/(SP:..) (P:..)/\2 \1/'
