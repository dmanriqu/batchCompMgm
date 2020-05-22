#!/bin/bash
#------------------------------------------------------------
# Adds GPL header according to data type
#------------------------------------------------------------

INPUT=$1
EXT="${INPUT##*.}"

case "$EXT" in
        cpp)
            H='data/headerCPP.txt'
            ;;
         
        h)
            H='data/headerCPP.txt'
            ;;
         
        R)
            H='data/header_R.txt'
            ;;

        *)
            exit
esac

cat "$H" "$INPUT" > cache/XX.tmp
dos2unix cache/XX.tmp
rm -f "$INPUT" 
mv cache/XX.tmp $INPUT


