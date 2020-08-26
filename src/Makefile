######################################################################
# Copyright (C) 2020 Clotilde Erard and Alain Giorgetti              #
# FEMTO-ST institute                                                 #
######################################################################

######################################################################
#  This software is distributed under the terms of the GNU Lesser    #
#  General Public License version 2.1                                #
######################################################################

# src/Makefile

DIRS?= -L .

.PHONY: ide ot wrt wet wt clean

ide:
	why3 ide $(DIRS) TestExamples.mlw
	why3 ide $(DIRS) Enum.mlw
	why3 ide $(DIRS) QCheck.mlw
	why3 ide $(DIRS) SCheck.mlw
	echo 'Incomplete proof of inverse_in_place.mlw with Alt-Ergo 2.2.0, CVC4 1.6 and Z3 4.7.1 (one unproved assertion):'
	why3 ide inverse_in_place.mlw

# Tests of OCaml properties with QCheck.ml
ot:
	bash ./ocheck.sh TestExamples

# Random tests of WhyML properties with QCheck.mlw
wrt:
	bash ./why3_check.sh TestExamples RandomTests

# Enumerative tests of WhyML properties by extraction to OCaml 
wet:
	bash ./why3_check.sh TestExamples EnumerativeTests

# All tests for WhyML together
wt: wrt wet

clean:
	rm -f *.bak; rm -rf _build *.byte

