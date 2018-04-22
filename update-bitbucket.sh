#!/bin/bash -x

git checkout master
git rebase develop
git checkout ipc-2018-seq-agl
git rebase develop
git checkout ipc-2018-seq-sat
git rebase develop
git push
git checkout develop
git push
