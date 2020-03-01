#!/bin/bash

####################################################################
# Copyright 2020 Alain Giorgetti and Clotilde Erard                #
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
  rm -f *.bak; rm -rf _build *.byte
  echo "Tests with QCheck.mlw and SCheck.mlw (QuickCheck and SmallCheck for WhyML)"
  echo "WARNING: Works only in src/ folder in the Docker container generated with the provided Dockerfile"
  why3 extract --recursive -D autocheck.drv -D ocaml-unsafe-int -L . $1.$2 -o $2.ml
  ocamlbuild -use-ocamlfind -pkg qcheck -pkg zarith $2.byte > /dev/null
  ./$2.byte
  rm -rf _build
}

time -p (wchk "$1" "$2")

exit
