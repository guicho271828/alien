#!/bin/bash

dir=$(readlink -ef $(dirname $0))
export SHELL=bash

export options="-m 6000 -t 300 --search-option '(eager (bucket-open-list (blind)))'"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v options,problem={} \
         $dir/singularity-alien.sh ::: \
         $(ls ipc2011-opt-blind/*/*.pddl | grep -v domain )

export options="-m 6000 -t 300 --search-option '(eager (bucket-open-list (ff/rpg)))'"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v options,problem={} \
         $dir/singularity-alien.sh ::: \
         $(ls ipc2011-opt-ff/*/*.pddl | grep -v domain )

export options="-m 6000 -t 300 --search-option '(eager (bucket-open-list (sum (product (constant (expt 2 (ceiling (log strips::*op-size* 2)))) (novelty4)) (ff/rpg))))'"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v options,problem={} \
         $dir/singularity-alien.sh ::: \
         $(ls ipc2011-opt-bwfs/*/*.pddl | grep -v domain )
