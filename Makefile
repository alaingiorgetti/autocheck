#######################################################################
# Copyright (C) 2020-21 Alain Giorgetti and Clotilde Erard            #
# FEMTO-ST institute                                                  #
#######################################################################

#######################################################################
#  This software is distributed under the terms of the GNU Lesser     #
#  General Public License version 2.1                                 #
#######################################################################

# File: Makefile

# Run
#  make build
# to build a docker image,
#  make ctr
# to create the Docker container,
#  make start
# to start an interactive session in the Docker container.

.PHONY: build ctr start clean

build: Dockerfile
	docker build --tag autocheck:latest .

# Sometimes 'make ctr' does not work. Then run 'bash ./ctr.sh' directly!
ctr:
	# Uncomment next line if the container already exists
	# docker container rm autocheckctr
	bash ./ctr.sh

# For more safety xhost is opened only to the container hostname, retrieved by
# the command
# docker inspect --format='{{ .Config.Hostname }}.
start:
	@xhost +local:`docker inspect --format='{{ .Config.Hostname }}' autocheckctr` >> /dev/null
	docker start --attach --interactive autocheckctr

