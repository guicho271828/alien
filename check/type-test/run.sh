#!/bin/bash

../mwup --plain -v --search fd-clean "--search astar(lmcut())" - p01.pddl

../downward/builds/release32/bin/validate -v domain.pddl problem.pddl p01.plan.1
