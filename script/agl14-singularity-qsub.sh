#!/bin/bash

dir=$(readlink -ef $(dirname $0))
export SHELL=bash
export options="-m 6000 -t 300 --search-option '(eager (bucket-open-list (novelty :k 5)))'"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v options,problem={} \
         $dir/singularity-alien.sh ::: \
         $(ls ipc2011-opt/*/*.pddl | grep -v domain )
