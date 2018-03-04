#!/bin/bash

dir=$(readlink -ef $(dirname $0))

parallel qsub -l "pmem=8000mb,cput=300" -v problem={} $dir/singularity-alien.sh ::: \
         $(ls ipc2014-agl/*/*.pddl | grep -v domain | head -n 1 )
