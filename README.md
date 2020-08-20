Copyright (C) 2019-2020 Clotilde Erard and Alain Giorgetti

AutoCheck is a random and enumerative testing tool for OCaml and WhyML.

Authors: Clotilde Erard and Alain Giorgetti

FEMTO-ST institute (UMR CNRS 6174)

Contact: alain.giorgetti AT femto-st.fr

Project home
============

https://github.com/alaingiorgetti/autocheck

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

See [EG20, Sections 4 and 5] and the Makefile for the different possible actions.

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
and Optimized Data Enumeration Programs. In ICTSS’19, 31st IFIP Int. Conf.
on Testing Software and Systems, volume 11812 of Lecture Notes in Computer
Science, pages 159–175, Paris, France, 2019. Springer, Cham.

[EG20] C. Erard and A. Giorgetti. Random and Enumerative Testing for OCaml and 
WhyML Properties. 20 pages. Submitted to the special issue “Testing of Software
 and Systems” of the Software Quality Journal.