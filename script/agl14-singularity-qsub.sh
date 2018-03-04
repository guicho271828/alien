#!/bin/bash

parallel qsub -l "pmem=8000,cput=300" script/singularity-alien.sh \
         ::: $(ls ipc2014-agl/*/*.pddl | grep -v domain)
