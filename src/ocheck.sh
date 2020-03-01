#!/bin/bash

####################################################################
# Copyright 2020 Alain Giorgetti and Clotilde Erard                #
# FEMTO-ST institute                                               #
####################################################################

####################################################################
#  This software is distributed under the terms of the GNU Lesser  #
#  General Public License version 2.1                              #
####################################################################

# File ocheck.sh

# Shell script for random and enumerative testing for OCaml.
# See examples of use in Makefile.

# Parameters:
# $1 is the prefix of the OCaml file $1.ml containing the test cases

ochk () {
  rm -f *.bak; rm -rf _build *.byte
  echo "Tests with QCheck.ml and SCheck.ml (QuickCheck and SmallCheck for OCaml)"
  echo "WARNING: Works only in src/ folder in the Docker container generated with the provided Dockerfile"
  ocamlbuild -use-ocamlfind -pkg qcheck -pkg zarith $1.byte > /dev/null
  ./$1.byte
  rm -rf _build
}

time (ochk "$1")

exit
