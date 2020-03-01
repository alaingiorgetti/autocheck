Installation instructions
=========================

Release 0.1.0 of AutoCheck is proposed only for Linux and to be used with files
in the folder src/ in a Docker container, as detailed below. All Docker commands
are shortened thanks to Makefile entries.

Installation with Docker
------------------------

1. If Docker is not installed, follow the instructions at
   [https://docs.docker.com/install/linux/docker-ce/ubuntu/](https://docs.docker.com/install/linux/docker-ce/ubuntu/)
   and 
   [https://docs.docker.com/engine/installation/linux/linux-postinstall/](https://docs.docker.com/engine/installation/linux/linux-postinstall/))
   to install Docker and run Docker commands without using sudo.

2. Build the Docker image:

     make build

   The Docker image will contain compatible releases of all the required tools.

   Warnings: The Docker image is a large file, its construction can be very long.
   A network connection is required. If you already have a Docker image with this
   name, either remove it or change the image name in Makefile.

5. Create the container:

     make ctr

   Warning: If you already have a Docker container with the same name, either
   remove it or change the container name in Makefile.

6. Start an interactive session in the container and move to the src/ folder in it:

     make start
     cd src

7. See the entries in the Makefile. They encapsulate various possible testing 
   actions that you can experiment. For instance, run all OCaml tests as follows:

     make ot

   or all WhyML tests as follows:

     make wt

   Warning: The folder src/ is mounted in the container. So, any change in it
   in the container is actually done in its local version.

8. Finally, quit the container:

    exit
