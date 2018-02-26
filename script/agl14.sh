#!/bin/bash

parallel --progress --files -j 6 --joblog parallel.log alien -t 300 -m 2000 {//}/domain.pddl {} {.}.plan ::: ipc2014-agl/*/p*.pddl
