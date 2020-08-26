######################################################################
# Copyright 2019-2020 Clotilde Erard and Alain Giorgetti             #
# FEMTO-ST institute                                                 #
######################################################################

######################################################################
#  This software is distributed under the terms of the GNU Lesser    #
#  General Public License version 2.1                                #
######################################################################

# Dockerfile to build a Docker image with all tools for random and enumerative
# testing of OCaml and WhyML properties

# 1. OCaml 4.06.0 and latest version of the OPAM package manager for Linux Ubuntu 18.04
FROM ocaml/opam2:ubuntu-18.04

# 2.
RUN sudo apt-get update && sudo apt-get install -y autoconf automake wget m4 time libgmp-dev

# 3. Confirm the working directory
WORKDIR /home/opam

######################################################################
# Installation of CVC3, CVC4 and Z3                                  #
######################################################################

# 4. CVC3 2.4.1
RUN wget http://www.cs.nyu.edu/acsys/cvc3/releases/2.4.1/linux64/cvc3-2.4.1-optimized-static.tar.gz && \
 tar -xzf cvc3-2.4.1-optimized-static.tar.gz && \
 sudo cp -R /home/opam/cvc3-2.4.1-optimized-static/* /usr/local/

# 5. CVC4 1.6 from source
RUN wget http://cvc4.cs.stanford.edu/downloads/builds/x86_64-linux-opt/cvc4-1.6-x86_64-linux-opt \
 && sudo cp cvc4-1.6-x86_64-linux-opt /usr/local/bin/cvc4 \
 && sudo chmod +x /usr/local/bin/cvc4

# 6. Z3 4.7.1 from source
RUN wget https://github.com/Z3Prover/z3/releases/download/z3-4.7.1/z3-4.7.1-x64-ubuntu-16.04.zip \
 && unzip z3-4.7.1-x64-ubuntu-16.04.zip \
 && sudo cp z3-4.7.1-x64-ubuntu-16.04/bin/z3 /usr/local/bin \
 && sudo chmod +x /usr/local/bin/z3

######################################################################
# Installation of Coq 8.9.0 and coqide with opam                     #
######################################################################

# 7. Configuration of opam
RUN export OPAMROOT=~/opam-coq.8.9.0 \
  && opam init -n --comp=ocaml-base-compiler.4.07.0 \
  && echo "\nexport OPAMROOT=~/opam-coq.8.9.0 \neval $(opam env --root=/home/opam/opam-coq.8.9.0)" >> ~/.profile \
  && echo "\n# opam configuration \nexport OPAMROOT=~/opam-coq.8.9.0 \neval $(opam env --root=/home/opam/opam-coq.8.9.0)" >> ~/.bashrc

# 8. Coq 8.9.0
RUN . ~/.profile \
 && opam repo add coq-released http://coq.inria.fr/opam/released \
 && opam install -y depext \
 && opam pin add coq 8.9.0 --yes \
 && opam depext --install -y coq-mathcomp-ssreflect \
 && opam depext --install -y coqide

######################################################################
# Installation of Alt-Ergo 2.2.0 with opam                           #
######################################################################

# 9. Alt-Ergo 2.2.0
RUN . ~/.profile && opam depext --install alt-ergo=2.2.0

######################################################################
# Installation of Why3 1.3.1 from git                                #
######################################################################

# 10. Why3 1.3.1 from git
RUN . ~/.profile && git clone -b 1.3.1 https://gitlab.inria.fr/why3/why3.git

# 11. Compilation of Why3 (1/2)
RUN . ~/.profile && cd why3 && autoconf && automake --add-missing; exit 0

# 12. Compilation of Why3 (2/2) and installation
RUN . ~/.profile && cd why3 && ./configure && make && sudo make install

# 13. Configuration of Why3
RUN . ~/.profile && why3 config --detect-provers --detect-plugins

# 14. Installation of QCheck
RUN . ~/.profile && opam depext --install qcheck=0.12


# 16. Remove the installation files
RUN rm -rf cvc* z* *.tar.gz
