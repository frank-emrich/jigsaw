#!/bin/bash


#NEWLINE=$'\n'

function bench {
   echo "Benchmarking: $2"
   cur_result=$(python -m timeit -n 10 -r 3 -v -s 'import os' "os.system(\"$1\")" | grep -o -E "[0-9\.]+ [a-z]+ per loop" | grep -o -E "[0-9\.]+")
   echo $cur_result
   result+="$2 : ${cur_result}\n"
}


function per_compiler_run {

result=""

#setup compiler

opam switch "$1"
opam install ppx_deriving dune
eval $(opam env)

echo "Using compiler" "$1"

#Open types and functions

pushd open-types-functions

pushd naive
dune clean
dune build naive.exe
bench "_build/default/naive.exe > /dev/null" "open types, naive multi module"
popd

echo "-------------------------------------------------------------------"

pushd modular
dune clean
dune build modular.exe
bench "_build/default/modular.exe > /dev/null" "open types, multi module"
popd

echo "-------------------------------------------------------------------"


pushd single-module
dune clean
dune build combined.exe
bench "_build/default/combined.exe > /dev/null" "open types, single module"
popd

echo "-------------------------------------------------------------------"


popd

#Polymorphic variants
pushd  polymoprhic-variants

pushd modular
dune clean
dune build modular.exe
bench "_build/default/modular.exe > /dev/null" "polymorphic variants (extensible design) over multiple files"
popd

echo "-------------------------------------------------------------------"

pushd single-module
dune clean
dune build combined.exe
bench "_build/default/combined.exe > /dev/null" "polymorphic variants (extensible design) in single file"
popd

popd

echo "-------------------------------------------------------------------"

#Non-extensible stuff

pushd  non-extensible


dune clean
dune build polymorphic_variants.exe
bench "_build/default/polymorphic_variants.exe > /dev/null" "polymorphic variants (non-extensible design) in single file"

echo "-------------------------------------------------------------------"

dune clean
dune build normal_datatypes.exe
bench "_build/default/normal_datatypes.exe > /dev/null"  "non-polymorphic (i.e., \"normal\") datatypes (non-extensible design) in single file"


popd

echo -e "Results:\n"$result
echo "-------------------------------------------------------------------"
echo "-------------------------------------------------------------------"

}






per_compiler_run "4.07.1"
per_compiler_run "4.07.1+flambda"