#!/bin/bash

# Creation script for the Docker container

# Get the working directory:
_pwd="$(pwd)"

docker create -ti -e DISPLAY=docker.for.mac.host.internal:0 \
 -v ${_pwd}/src:/home/opam/src \
 --name="autocheckctr" autocheck:latest

