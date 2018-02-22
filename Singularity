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

    # Dependent external libraries. They do not contain performance sensitive code.
    # 
    # These are general purpose and are submitted to / already part of quicklisp
    # (quicklisp = library installation system similar to pip). I am the author of the library.
    # Quicklisp libraries are verified by the quicklisp maintainers so that they build without errors.
    # 
    # Usually "ros install cl-prolog2" suffices (which pulls from quicklisp), but
    # I specifically pull from github (guicho271828/cl-prolog2 etc) because I
    # recently found a bug in them or they are still in the process of inclusion
    # to quicklisp and quicklisp is updated only monthly.
    # 
    # For IPC I want to make sure it always pulls from the most up-to-date
    # version.  but later, I can change it to "ros install cl-prolog2" and such,
    # so that it pulls from quicklisp. (which pulls from my repository anyways, because I am the author,
    # but in this case the library is verified by the quicklisp organization)
    # 
    ros install guicho271828/cl-prolog2
    ros install guicho271828/type-r
    ros install guicho271828/trivial-package-manager

    # Another hack here.
    # Two repositories, "guicho271828/alien" on bitbucket is merely a (possibly outdated) copy of "guicho271828/strips" on github.
    # In the older commits of this Singularity file,
    # I used to pull from github (i.e. download the same repository into a subdirectory) because of lazyness.
    
    # However, just to avoid fetching from external github,
    # I make a symbolic link to the current directory (/planner)
    # from the internal directory of quicklisp local-project (/planner/.roswell/local-projects).
    # Now quicklisp recognizes the current directory.
    ln -s /planner /planner/.roswell/local-projects/guicho271828/strips

    # now quicklisp downloads all standard dependencies, and also recognize the script (alien.ros),
    # put it in .roswell/bin .
    ros install guicho271828/strips

    # alien.ros is not compiled. If you run it once, it self-compiles itself.
    alien
    
%runscript
    ## The runscript is called whenever the container is used to solve
    ## an instance.

    ls -la
    ls -la /planner/.roswell/local-projects/guicho271828/strips
    
    pwd
    env
    which alien
    alien $1 $2 $3
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
