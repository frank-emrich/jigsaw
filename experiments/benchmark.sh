#!/bin/bash


#NEWLINE=$'\n'
RESULTFILE="results.txt"

function bench {
   echo "Benchmarking: $2"
   cur_result=$(python -m timeit -n 2 -r 2 -v -s 'import os' "os.system(\"$1\")" | grep -o -E "[0-9\.]+ [a-z]+ per loop" | grep -o -E "[0-9\.]+")
   echo $cur_result
   result+="$2 : ${cur_result}\n"
}


function per_compiler_run {

result=""
FLAGS="$2"

#setup compiler

opam switch "$1"
eval $(opam env)

echo "Using compiler $1, flags $2"

#Open types and functions

pushd open-types-functions

pushd naive
make clean
make FLAGS="$FLAGS" naive
bench "_build/naive.exe > /dev/null" "open types, naive multi module"
popd

echo "-------------------------------------------------------------------"

pushd naive-staged
opam switch "4.07.1+BER"
eval $(opam env)
make clean
make FLAGS="$FLAGS" naive-staged
pushd _build
bench "./naive-staged.exe $FLAGS " "open types, naive multi module"
opam switch "$1"
eval $(opam env)
popd
popd

echo "-------------------------------------------------------------------"

pushd modular
make clean
make FLAGS="$FLAGS" modular
bench "_build/modular.exe > /dev/null" "open types, multi module"
popd

echo "-------------------------------------------------------------------"


#pushd single-module
#make clean
#make FLAGS="$FLAGS" combined
#bench "_build/combined.exe > /dev/null" "open types, single module"
#popd
#
#echo "-------------------------------------------------------------------"
#
#
#popd
#
##Polymorphic variants
#pushd  polymoprhic-variants
#
#pushd modular
#make clean
#make FLAGS="$FLAGS" modular
#bench "_build/modular.exe > /dev/null" "polymorphic variants (extensible design) over multiple files"
#popd
#
#echo "-------------------------------------------------------------------"
#
#
#pushd modular
#make clean
#make FLAGS="$FLAGS" modular_combined_compilation
#bench "_build/modular_combined_compilation.exe > /dev/null" "polymorphic variants (extensible design) over multiple files, but not using separate compilation (all .ml files combined at the same time)"
#popd
#
#echo "-------------------------------------------------------------------"
#
#
#pushd single-module
#make clean
#make FLAGS="$FLAGS" combined
#bench "_build/combined.exe > /dev/null" "polymorphic variants (extensible design) in single file"
#popd
#
#popd
#
#echo "-------------------------------------------------------------------"
#
##Non-extensible stuff
#
pushd  non-extensible


#make clean
#make FLAGS="$FLAGS" polymorphic_variants
#bench "_build/polymorphic_variants.exe > /dev/null" "polymorphic variants (non-extensible design) in single file"
#
#echo "-------------------------------------------------------------------"
#
make clean
make FLAGS="$FLAGS" normal_datatypes
bench "_build/normal_datatypes.exe > /dev/null"  "non-polymorphic (i.e., \"normal\") datatypes (non-extensible design) in single file"


popd

echo -e "Results compiler $1, flags $2 :\n"$result "\n" | tee -a "$RESULTFILE"
echo "-------------------------------------------------------------------"
echo "-------------------------------------------------------------------"

}






per_compiler_run "4.07.1" "-O2 -opaque"
per_compiler_run "4.07.1" "-O3 "
per_compiler_run "4.07.1+flambda" "-O3 "
