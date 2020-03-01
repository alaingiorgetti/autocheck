#!/bin/bash

# Creation script for the Docker container

# Get the working directory:
_pwd="$(pwd)"

docker create -ti -e DISPLAY=$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix \
 -v ${_pwd}/src:/home/opam/src \
 --name="autocheckctr" autocheck:latest

