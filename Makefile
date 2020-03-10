#######################################################################
# Copyright (C) 2020 Alain Giorgetti and Clotilde Erard               #
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
# to start an interactive session in the Docker container

.PHONY: build ctr start clean

build: Dockerfile
	docker build --tag autocheck:latest .

# Sometimes does not work! Then run 'bash ./ctr.sh' directly!
ctr:
	# Uncomment next line if the container already exists
	# docker container rm autocheckctr
	bash ./ctr.sh

# Sometimes does not work! Then run 'bash ./ctrmac.sh' directly!
ctrmac:
	# Uncomment next line if the container already exists
	# docker container rm autocheckctr
	bash ./ctrmac.sh

# For more safety xhost is opened only to the container on the local host's docker daemon
# whose container's ID has been stored by ctr.sh in the shell variable containerId.
start:
	@xhost +local:`docker inspect --format='{{ .Config.Hostname }}' autocheckctr` >> /dev/null
	docker start --attach --interactive autocheckctr

