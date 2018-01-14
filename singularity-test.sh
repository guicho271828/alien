#!/bin/bash


mkdir -p rundir
cp movie/domain.pddl rundir
cp movie/p01.pddl rundir

RUNDIR="$(pwd)/rundir"
DOMAIN="$RUNDIR/domain.pddl"
PROBLEM="$RUNDIR/p01.pddl"
PLANFILE="$RUNDIR/sas_plan"

singularity run -C -H $RUNDIR planner.img $DOMAIN $PROBLEM $PLANFILE
