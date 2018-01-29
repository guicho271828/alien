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
    apt-get -y install git curl
   
    cd /planner
    
    ## placeholder for initial submission
    
    git submodule status
    git submodule update --init

    # ( apt-get -y install cmake g++ make python flex bison ; cd downward ; ./build.py release64 )
    
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
    
    apt-get -y install software-properties-common python-software-properties g++ flex bison
    ros install guicho271828/cl-prolog2
    ros install guicho271828/trivial-package-manager
    ros install guicho271828/strips
    alien
    
%runscript
    ## The runscript is called whenever the container is used to solve
    ## an instance.

    ## placeholder for initial submission

    ls -la
    ls -la /planner/.roswell/local-projects/guicho271828/strips
    
    DOMAINFILE=$1
    PROBLEMFILE=$2
    PLANFILE=$3
    # /planner/downward/fast-downward.py \
    #     --build=release64 \
    #     --plan-file $PLANFILE \
    #     $DOMAINFILE \
    #     $PROBLEMFILE \
    #     --search "eager(single_buckets(ff()))"

    alien $1 $2 $3



## Update the following fields with meta data about your submission.
## Please use the same field names and use only one line for each value.
%labels
Name        alien
Description forward search with a satisficing heuristic not based on lower bound
Authors     Masataro Asai <guicho2.71828@gmail.com>
SupportsDerivedPredicates yes
SupportsQuantifiedPreconditions yes
SupportsQuantifiedEffects yes
