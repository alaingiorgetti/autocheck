AutoCheck is a random and enumerative testing tool for OCaml and WhyML. It is developed by Clotilde Erard and [Alain Giorgetti](https://members.femto-st.fr/alain-giorgetti/en) in the [DISC departement](https://www.femto-st.fr/en/Research-departments/DISC/Presentation) of [FEMTO-ST institute (UMR CNRS 6174)](https://www.femto-st.fr/en).

# Project home

[https://github.com/alaingiorgetti/autocheck](https://github.com/alaingiorgetti/autocheck)

# Contact

Please use github issue manager at [https://github.com/alaingiorgetti/autocheck/issues](https://github.com/alaingiorgetti/autocheck/issues) to report installation and usage issues.

For other subjects, please send a message to alain.giorgetti AT femto-st.fr.

# Installation

See the file INSTALL.md at [https://github.com/alaingiorgetti/autocheck](https://github.com/alaingiorgetti/autocheck).

# Documentation

The project is documented by the present file and by the documents listed in the [reference](#references) section below.

# Folders

* src: tool code
* docs: Documentation

# How to use this code?

See [EG20, Sections 4 and 5], [Gio20] and the Makefile for the different possible actions.

The command to test WhyML properties is

	bash ./why3_check.sh TestFilePrefix ModuleName

where `TestFilePrefix` is the prefix of the WhyML file TestFilePrefix.mlw containing 
modules with test cases, and `ModuleName` is the name of the WhyML module in the file 
TestFilePrefix.mlw containing the test cases you want to execute.

WARNING: The testing process works well only if the file ModuleName.ml it generates 
doesn't pre-exist in the folder. Therefore, it starts and ends with a command removing
this file. Before running a test, make sure that there is no important file with this
name in the folder.

# Applications

1. Enumerative testing of function properties in theories of permutations with Why3: 
  communication at JFLA 2021 [Gio21], research report [Gio20], 
  code files [functions.mlw](https://github.com/alaingiorgetti/autocheck/blob/master/src/functions.mlw) and 
   [ABF20.mlw](https://github.com/alaingiorgetti/autocheck/blob/master/src/ABF20.mlw), 
 [code documentation](https://alaingiorgetti.github.io/autocheck/permutTheory/index.html), 
 [Why3 proof results for functions.mlw](https://alaingiorgetti.github.io/autocheck/permutTheory/functions/why3session.html), 
 [Why3 proof results for ABF20.mlw](https://alaingiorgetti.github.io/autocheck/permutTheory/ABF20/why3session.html).

# References

[EG19] C. Erard and A. Giorgetti. Bounded Exhaustive Testing with Certified
and Optimized Data Enumeration Programs. In: Gaston, C., Kosmatov, N., Le Gall,
P. (eds.) Testing Software and Systems. ICTSS 2019. LNCS, vol. 11812, pp. 159-175.
Springer, Cham (2019).
[https://doi.org/10.1007/978-3-030-31280-0_10](https://doi.org/10.1007/978-3-030-31280-0_10).

[EG20] C. Erard and A. Giorgetti. Random and Enumerative Testing for OCaml and 
WhyML Properties. 20 pages.
[https://alaingiorgetti.github.io/autocheck/EG20.pdf](https://alaingiorgetti.github.io/autocheck/EG20.pdf).

[Gio20] A. Giorgetti. Formalisation et vérification de théories de permutations. Research report RR-1715 (in French), 
UBFC (Université de Bourgogne Franche-Comté) and FEMTO-ST, 17 pages, December 2020.
[https://hal.archives-ouvertes.fr/hal-03033416](https://hal.archives-ouvertes.fr/hal-03033416).

[Gio21] A. Giorgetti. Théories de permutations avec Why3. In JFLA 2021, 32 èmes Journées Francophones des Langages Applicatifs,
pages 202-209, April 2021.
[https://hal.inria.fr/hal-03190426](https://hal.inria.fr/hal-03190426).

Copyright (C) 2019-2021 Clotilde Erard and Alain Giorgetti
