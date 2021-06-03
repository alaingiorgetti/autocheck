#!/bin/bash

####################################################################
# Copyright 2020-2021 Alain Giorgetti, Clotilde Erard and          #
# Jérome Ricciardi                                                 #
# FEMTO-ST institute                                               #
####################################################################

####################################################################
#  This software is distributed under the terms of the GNU Lesser  #
#  General Public License version 2.1                              #
####################################################################

# File why3_check.sh

# Shell script for random and enumerative testing for WhyML.
# Prototype for a new command 'why3 check ..' with similar parameters.
# See examples of use in Makefile.

# Parameters:
# $1 is the prefix of the WhyML file $1.mlw containing the test cases
# $2 is the name of the WhyML module in the file $1.mlw containing the test cases

wchk () {
  echo "Tests with QCheck.mlw and SCheck.mlw (QuickCheck and SmallCheck for WhyML)"
  echo "WARNINGS:"
  echo "1. Works only in src/ folder in the Docker container generated with the provided Dockerfile."
  echo "2. A file $2.ml is generated and should not pre-exist in the folder. So, the test first removes this file."
  # WARNING: The testing process works well only if the file $2.ml it generates 
  # by Why3 extraction doesn't pre-exist in the folder. Therefore, it starts and ends with
  # a command removing this file:
  rm -f $2.ml
  rm -f *.bak; rm -rf _build *.byte
  why3 extract --recursive -D autocheck.drv -D ocaml64 -D ocaml-unsafe-int.drv -L . $1.$2 -o $2.ml
  eval $(opam env) ocamlbuild -use-ocamlfind -pkg qcheck -pkg zarith $2.byte > /dev/null
  ./$2.byte
  rm -rf _build
  rm -f $2.ml
}

time -p (wchk "$1" "$2")

exit
