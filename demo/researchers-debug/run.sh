#!/bin/bash

../../downward/fast-downward.py --run-all --alias lama-first domain.pddl p01.pddl

../../downward/builds/release32/bin/validate -v domain.pddl p01.pddl p01.sas_plan ; echo $?
../../downward/builds/release32/bin/validate -v domain.pddl p01.pddl p01.alien_plan ; echo $?

