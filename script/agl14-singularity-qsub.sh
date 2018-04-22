#!/bin/bash

dir=$(readlink -ef $(dirname $0))
export SHELL=bash

export options
export probdir

origdir=$dir/../../results3/ipc2014-agl

run (){
    cp -r $origdir $probdir
    parallel qsub \
             -l "mem=8000mb,pmem=8000mb,cput=300,walltime=300" \
             -e /dev/null \
             -o /dev/null \
             -v options,problem={} \
             $dir/singularity-alien.sh ::: \
             $(ls $probdir/*/*.pddl | grep -v domain )
}

options="--alias alien"             probdir="../results3/ipc2014-agl-alien"              run
options="--alias alien+ff"          probdir="../results3/ipc2014-agl-alien+ff"           run
options="--alias novelty2+alien+ff" probdir="../results3/ipc2014-agl-novelty2+alien+ff"  run
options="--alias novelty3+alien+ff" probdir="../results3/ipc2014-agl-novelty3+alien+ff"  run

options="--alias alien/rpg"             probdir="../results3/ipc2014-agl-alien_rpg"              run
options="--alias alien/rpg+ff"          probdir="../results3/ipc2014-agl-alien_rpg+ff"           run
options="--alias novelty2+alien/rpg+ff" probdir="../results3/ipc2014-agl-novelty2+alien_rpg+ff"  run
options="--alias novelty3+alien/rpg+ff" probdir="../results3/ipc2014-agl-novelty3+alien_rpg+ff"  run

options="--alias alieni"            probdir="../results3/ipc2014-agl-alieni"             run
options="--alias alieni/rpg"            probdir="../results3/ipc2014-agl-alieni_rpg"             run


options="--alias ff"         probdir="../results3/ipc2014-agl-ff"     run
options="--alias blind"      probdir="../results3/ipc2014-agl-blind"  run
options="--alias goal-count" probdir="../results3/ipc2014-agl-gc"     run
options="--alias bwfs2"      probdir="../results3/ipc2014-agl-bwfs2"  run
options="--alias bwfs3"      probdir="../results3/ipc2014-agl-bwfs3"  run
options="--alias bwfs4"      probdir="../results3/ipc2014-agl-bwfs4"  run

options="--alias wffw1"      probdir="../results3/ipc2014-agl-wffw1"  run
options="--alias wffw2"      probdir="../results3/ipc2014-agl-wffw2"  run

