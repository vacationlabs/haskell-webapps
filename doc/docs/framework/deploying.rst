Deploying
=================================

Using stack with Docker
-----------------------

NOTE: If you are using Windows operating system, this is not yet working for Windows. Watch this
issue https://github.com/commercialhaskell/stack/issues/2421

The Stack tool has built in support for executing builds inside a docker container. But first
you have to set up some stuff on your machine.  First of which is installing docker on your system. 

https://docs.docker.com/engine/installation/

Download and install the CE (Community Edition) version. After the installation
you should have a ``docker`` command available in your terminal.

Try the docker command ``docker images`` and see if works without errors. If
you are getting a permission denied error, try running the following command,

    sudo usermod -a -G docker $USER

NOTE: After the above command, you should completly log out and log in to see the affect.
Or if you cannot do that, just relogin as the same user, for ex, if you are loggied in as user ``vl``
just do a ``su vl`` and that should be enough.

Next you have to build the docker image that we will use for our builds. You have two options here.

1. You can either build one from using the docker file
2. You can pull a prebuilt image from the docker hub.

Building from docker file
-------------------------

Open up a terminal and go to the root of the app. There should be a ``docker`` folder there. Go to that folder,
and do ``docker build .`` there.

.. code:: bash

  cd docker
  docker build -t vacationlabs-ubuntu .

When this is done, you will have a new docker image with name "vl-ubuntu-image".

Configuring Stack
-----------------

Your stack.yaml will contain the following lines.

.. code:: yaml

  docker:
    env:
      - "APP_ENV=development"
    enabled: false
    image: vacationlabs-ubuntu
    run-args: ["--ulimit=nofile=60000", "--memory=4g"]

1. The ``env`` key contains a list and is used to set environment variables inside the container
   before the build.yaml

2. The ``enabled`` flag set to false to NOT use docker by default. Docker will be involved only
   upon specifing the command line flag ``--docker``.

2. The ``image`` key is used to specify the docker image from which the container for the build will be made.
   This should already exist.

3. The ``run-args`` key us used to pass arguments to the docker command that created the container. Here we
   have used it to increase the maximum number of open files that will be allowed inside the container and
   the maximum amount of host memory the container is allowed to use.

Now you can build the app using the ``stack build --docker``

When you do this for the first time, stack will complain there is no compiler installed in
the container. Just use ``--install-ghc`` flag like ``stack build --docker --install-ghc``. And it will
install the compiler inside the container. 

Stack will mount the ~/.stack folder inside the container, so installing compiler and dependencies
only need to be done once. That is unless you change the image for the container.

If you find that stack gets stalled after downloading the compiler at around 90mb, you can just download
the required tar archive from https://github.com/commercialhaskell/ghc/releases to the ``~/.stack/programs/x86_64-linux-*`` folder and name it using format ``ghc-8.0.2.tar.xz`` and run the build command again. That stack will use
downloaded archive instead of downloading it again.

After the build, the binary file will be in the usual location.


Further reference : https://docs.haskellstack.org/en/stable/docker_integration/
