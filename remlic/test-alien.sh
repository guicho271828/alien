#!/bin/bash


alien-simulate \
    rf-2-others1b-t.pddl \
    lightsout_ZeroSuppressConvolutionalGumbelAE_twisted_4_100_20000_0.7_True_rf-2-others1b-t_ff.pddl \
    lightsout_ZeroSuppressConvolutionalGumbelAE_twisted_4_100_20000_0.7_True_rf-2-others1b-t_ff.plan

alien-simulate \
    rf-2-others1b-t.pddl \
    lightsout_ZeroSuppressConvolutionalGumbelAE_twisted_4_100_20000_0.7_True_rf-2-others1b-t_ff.pddl \
    lightsout_ZeroSuppressConvolutionalGumbelAE_twisted_4_100_20000_0.7_True_rf-2-others1b-t_ff.plan \
    lightsout_ZeroSuppressConvolutionalGumbelAE_twisted_4_100_20000_0.7_True_rf-2-others1b-t_ff.trace