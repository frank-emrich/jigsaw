#!/bin/bash

function bench {
   python -m timeit -n 10 -r 3 -v -s 'import os' "os.system(\"$1\")"
}


#Open types and functions

pushd open-types-functions

pushd naive
dune clean
dune build naive.exe
echo "open types, naive"
bench "_build/default/naive.exe > /dev/null"
popd

echo "-------------------------------------------------------------------"

pushd modular
dune clean
dune build modular.exe
echo "open types"
bench "_build/default/modular.exe > /dev/null"
popd

echo "-------------------------------------------------------------------"

popd

#Polymorphic variants
pushd  polymoprhic-variants

pushd modular
dune clean
dune build modular.exe
echo "polymorphic variants (extensible design) over multiple files"
bench "_build/default/modular.exe > /dev/null"
popd

echo "-------------------------------------------------------------------"

pushd single-module
dune clean
dune build combined.exe
echo "polymorphic variants (extensible design) in single file"
bench "_build/default/combined.exe > /dev/null"
popd

popd

echo "-------------------------------------------------------------------"


pushd  non-extensible


dune clean
dune build polymorphic_variants.exe
echo "polymorphic variants (non-extensible design) in single file"
bench "_build/default/polymorphic_variants.exe > /dev/null"

echo "-------------------------------------------------------------------"

dune clean
dune build normal_datatypes.exe
echo "non-polymorphic (i.e., \"normal\") datatypes (non-extensible design) in single file"
bench "_build/default/normal_datatypes.exe > /dev/null"


popd