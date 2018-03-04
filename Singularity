# -*- mode : conf -*-
Bootstrap: docker
From: ubuntu

%setup
     ## The "%setup"-part of this script is called to bootstrap an empty
     ## container. It copies the source files from the branch of your
     ## repository where this file is located into the container to the
     ## directory "/planner". Do not change this part unless you know
     ## what you are doing and you are certain that you have to do so.

    REPO_ROOT=`dirname $SINGULARITY_BUILDDEF`
    cp -r $REPO_ROOT/ $SINGULARITY_ROOTFS/planner
    
%environment
    
    export ROSWELL_HOME=/planner/.roswell
    export PATH=/planner/.roswell/bin:$PATH
    export MAKEFLAGS="-j $((2*$(nproc)))"
    
%post

    ## The "%post"-part of this script is called after the container has
    ## been created with the "%setup"-part above and runs "inside the
    ## container". Most importantly, it is used to install dependencies
    ## and build the planner. Add all commands that have to be executed
    ## once before the planner runs in this part of the script.

    apt-get update
    apt-get -y install git curl zip

    cd /planner

    ## Install all necessary dependencies.
    
    apt-get -y install build-essential automake libcurl4-openssl-dev

    if ! which ros ; then
        (
        git clone -b release https://github.com/roswell/roswell.git roswell-src
        cd roswell-src
        sh bootstrap
        ./configure
        make
        make install
        ros setup
        )
    fi

    # unpack the irregular dependencies to the directory recognized by quicklisp
    unzip -u zip/type-r-master.zip     -d .roswell/local-projects/
    unzip -u zip/cl-prolog2-master.zip -d .roswell/local-projects/
    unzip -u zip/cl-cudd-master.zip    -d .roswell/local-projects/
    
    (
       # cloning /planner to /planner/.roswell/local-projects/strips (the directory recognized by quicklisp)
       cd .roswell/local-projects/
       git clone /planner strips || true  # continue if it fails (already exists)
       cd strips
       git fetch /planner        # fetch from /planner
       git checkout FETCH_HEAD
       # the reason I do this is to maintain the idempotency of MAKE and reduce the build time on my side
    )
    
    # this now downloads from quicklisp, not from github.
    ros install trivial-package-manager
    # Quicklisp downloads the rest of dependencies,
    # recognize the roswell script (roswell/alien.ros) and copy it as an executable .roswell/bin/alien .
    ros install strips
    
    # alien is not compiled. If you run it once, it self-compiles itself. The
    # compilation of lisp code is quite, quite different from what you usually
    # expect from C or C++. Don't ask.
    alien
    chmod 777 $(which alien)
    chmod -R 777 /root/
    
%runscript
    ## The runscript is called whenever the container is used to solve
    ## an instance.

    ls -la
    ls -la /planner/.roswell/local-projects/strips
    ls -la /planner/.roswell/bin
    
    pwd
    env
    which alien
    alien -m 6000 -t 300 $1 $2 $3
    ls



## Update the following fields with meta data about your submission.
## Please use the same field names and use only one line for each value.
%labels
Name        alien
Description forward search with a satisficing heuristic not based on lower bound
Authors     Masataro Asai <guicho2.71828@gmail.com>
SupportsDerivedPredicates yes
SupportsQuantifiedPreconditions yes
SupportsQuantifiedEffects yes
