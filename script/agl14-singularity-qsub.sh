#!/bin/bash

dir=$(readlink -ef $(dirname $0))
export SHELL=bash

export options="--alias alien3"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v options,problem={} \
         $dir/singularity-alien.sh ::: \
         $(ls ipc2014-agl-alien3/*/*.pddl | grep -v domain )

export options="--alias alien2"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v options,problem={} \
         $dir/singularity-alien.sh ::: \
         $(ls ipc2014-agl-alien2/*/*.pddl | grep -v domain )

export options="--alias alien"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v options,problem={} \
         $dir/singularity-alien.sh ::: \
         $(ls ipc2014-agl-alien/*/*.pddl | grep -v domain )

export options="--alias ff"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v options,problem={} \
         $dir/singularity-alien.sh ::: \
         $(ls ipc2014-agl-ff/*/*.pddl | grep -v domain )

export options="--alias bwfs"

parallel qsub \
         -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
         -e /dev/null \
         -o /dev/null \
         -v options,problem={} \
         $dir/singularity-alien.sh ::: \
         $(ls ipc2014-agl-bwfs/*/*.pddl | grep -v domain )
