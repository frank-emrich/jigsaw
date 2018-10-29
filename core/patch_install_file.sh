#!/bin/bash
package="$1"
file="$2"
replacement="\"_build/default/${package}.jigext\" {\"${package}.jigext\"} "
#echo $replacement
body="s|lib: \[|lib: \[\n  ${replacement}|g"
#echo "$body"
sed -i "${body}" "${file}"

