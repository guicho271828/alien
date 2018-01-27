#!/bin/bash

../../downward/fast-downward.py --run-all --alias lama-first domain.pddl p00.pddl

../../downward/builds/release32/bin/validate -v domain.pddl p00.pddl p00.sas_plan ; echo $?
../../downward/builds/release32/bin/validate -v domain.pddl p00.pddl p00.alien_plan ; echo $?

# there was a problem in adding & deleting the same literal

../../downward/fast-downward.py --run-all --alias lama-first domain.pddl p01.pddl
