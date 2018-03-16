#!/bin/bash -x

dir=$(readlink -ef $(dirname $0))
export SHELL=bash

export probdir="../results3/ipc2014-agl-probe"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v problem={} \
         $dir/qsub-probe.sh ::: \
         $(ls $probdir/*/*.pddl | grep -v domain )
