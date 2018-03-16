#!/bin/bash

for d in ../results3/ipc2014-agl*/ ; do (cd $d ; git clean -fdx ) ; done

script/agl14-singularity-qsub.sh
