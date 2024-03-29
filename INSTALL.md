Installation instructions
=========================

AutoCheck is proposed only for Linux and to be used with files in the folder src/ 
in a Docker container, as detailed below. All Docker commands are shortened thanks
to Makefile entries.

Installation with Docker
------------------------

- If Docker is not installed, follow the instructions at
  [https://docs.docker.com/engine/install/debian/](https://docs.docker.com/engine/install/debian/)
  and 
  [https://docs.docker.com/engine/install/linux-postinstall/](https://docs.docker.com/engine/install/linux-postinstall/)
  to install Docker and run Docker commands without using sudo.

- Build the Docker image:

        make build

   The Docker image will contain compatible releases of all the required tools.

   Warnings: The Docker image is a large file, its construction can be very long.
    A network connection is required. If you already have a Docker image with this
    name, then rename it, remove it or change the image name in Makefile and ctr.sh.

- Create the container:

        make ctr

   Warnings: If you already have a Docker container with the same name, then rename it,
   remove it or change the container name in Makefile and ctr.sh. Make sure the root 
   folder where you write the command has no spaces.

- Start an interactive session in the container and move to the src/ folder in it:

        make start
        cd src

- See the entries in the Makefile. They encapsulate various possible testing or
  proving actions that you can experiment. For instance, you can run all OCaml tests as follows:

        make ot

   or all WhyML tests as follows:

        make wt

   Warning: The folder src/ is mounted in the container. So, any change in it
   in the container is actually done in its local version.

- Finally, quit the container:

       exit