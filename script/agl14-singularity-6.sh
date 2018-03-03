#!/bin/bash

parallel --progress -j 6 --joblog parallel.log \
         script/singularity-alien.sh ::: $(ls ipc2014-agl/*/*.pddl | grep -v domain)
