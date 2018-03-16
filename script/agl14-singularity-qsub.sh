#!/bin/bash

dir=$(readlink -ef $(dirname $0))
export SHELL=bash

export options
export probdir

run (){
    parallel qsub \
             -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
             -e /dev/null \
             -o /dev/null \
             -v options,problem={} \
             $dir/singularity-alien.sh ::: \
             $(ls $probdir/*/*.pddl | grep -v domain )
}

options="--alias alien3"     probdir="../results3/ipc2014-agl-alien3" run
options="--alias alien2"     probdir="../results3/ipc2014-agl-alien2" run
options="--alias alien"      probdir="../results3/ipc2014-agl-alien"  run
options="--alias ff"         probdir="../results3/ipc2014-agl-ff"     run
options="--alias blind"      probdir="../results3/ipc2014-agl-blind"  run
options="--alias goal-count" probdir="../results3/ipc2014-agl-gc"     run
options="--alias bwfs2"      probdir="../results3/ipc2014-agl-bwfs2"  run
options="--alias bwfs3"      probdir="../results3/ipc2014-agl-bwfs3"  run
options="--alias bwfs4"      probdir="../results3/ipc2014-agl-bwfs4"  run

options="--alias alien4"     probdir="../results3/ipc2014-agl-alien4" run



