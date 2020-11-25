Copyright (C) 2019-2020 Clotilde Erard and Alain Giorgetti

AutoCheck is a random and enumerative testing tool for OCaml and WhyML.

Authors: Clotilde Erard and Alain Giorgetti

FEMTO-ST institute (UMR CNRS 6174)

Contact: alain.giorgetti AT femto-st.fr

Project home
============

[https://github.com/alaingiorgetti/autocheck](https://github.com/alaingiorgetti/autocheck)

Please use github issue manager at [https://github.com/alaingiorgetti/autocheck/issues](https://github.com/alaingiorgetti/autocheck/issues) to report any installation or usage issues.

Copyright
=========

This program is distributed under the GNU LGPL 3. See the enclosed file LICENSE.

Documentation
=============

The project is documented by the present file and in [EG20].

Installation
============

See INSTALL.md.

Folders
=======

src
---

  Tool code.

docs
----

  Documentation.

How to use this code?
=====================

See [EG20, Sections 4 and 5], [Gio21] and the Makefile for the different possible actions.

The command to test WhyML properties is

	bash ./why3_check.sh TestFilePrefix ModuleName

where TestFilePrefix is the prefix of the WhyML file TestFilePrefix.mlw containing 
modules with test cases, and ModuleName is the name of the WhyML module in the file 
TestFilePrefix.mlw containing the test cases you want to execute.

WARNING: The testing process works well only if the file ModuleName.ml it generates 
doesn't pre-exist in the folder. Therefore, it starts and ends with a command removing
this file. Before running a test, make sure that there is no important file with this
name in the folder.

References
==========

[EG19] C. Erard and A. Giorgetti. Bounded Exhaustive Testing with Certified
and Optimized Data Enumeration Programs. In: Gaston, C., Kosmatov, N., Le Gall,
P. (eds.) Testing Software and Systems. ICTSS 2019. LNCS, vol. 11812, pp. 159-175.
Springer, Cham (2019), https://doi.org/10.1007/978-3-030-31280-0_10.

[EG20] C. Erard and A. Giorgetti. Random and Enumerative Testing for OCaml and 
WhyML Properties. 20 pages. Submitted the 20th August 2020. The author version 
docs/EG20.pdf contains the following improvements of the submitted paper:


- 'For technical reasons, AutoCheck 0.1.0 is developed for release 1.2.0 of
Why3, exploiting the SMT solvers Alt-Ergo 2.2.0, CVC4 1.6 and Z3 4.7.1.' is
replaced by 'AutoCheck 0.1.0 is developed for release 1.3.1 of Why3. It exploits
QCheck 0.12 and the SMT solvers Alt-Ergo 2.2.0, CVC4 1.6 and Z3 4.7.1.'


- 'It was complete with releases 2.0.0, 1.4 and 4.4.1 of Alt-Ergo, CVC4 and Z3,
but is no longer complete with our more recent releases 2.2.0, 1.6 and 4.7.1 of
these tools, for the same release 1.2.0 of Why3!' is replaced by 'It is complete
with releases 2.0.0 and 1.4 of Alt-Ergo and CVC4, but is no longer complete with
the more recent releases 2.2.0 and 1.6 of these tools, for the same releases
4.7.1 of Z3 and 1.3.1 of Why3!'


- 'The fourth (resp. fifth) column gives the number of transformations (resp.
lemmas) needed for the proof of soundness, progress and completeness properties.
All of them have been proved automatically with Why3 1.2.0 and the SMT solvers
Alt-Ergo 2.2.0, CVC4 1.6 and Z3 4.7.1, except the completeness property for the
generator of permutations, which required an interactive proof of two lemmas
with Coq 8.9.0.' is replaced by 'The fourth (resp. fifth) column gives the
number of transformations (resp. lemmas) needed to prove their soundness,
progress and completeness properties. All of them have been proved automatically
with Why3 1.2.0 and the SMT solvers Alt-Ergo 2.2.0, CVC4 1.6 and Z3 4.7.1,
except the completeness property for the generator of permutations, which
required an interactive proof of two lemmas with Coq 8.9.0 [22]. After
integration of the generators in AutoCheck their properties have been similarly
proved with Why3 1.3.1 and the same releases of the SMT solvers.'


- References revised, more details, one broken link repaired.


- 'or to derive s from' -> 'or to derive effective generators from'


- 'However, we have shown in a fomer work' -> 'However, we have shown in a
former work'


- Acknowledgements section added

The first three changes reflect the replacement of Why3 1.2.0 by Why3 1.3.1 in
the tool, to solve a couple of issues related to the contents of the older
release 1.2.0 of Why3.


[Gio21] A. Giorgetti. Théories de permutations avec Why3. 17 pages. Submitted in October 2020.