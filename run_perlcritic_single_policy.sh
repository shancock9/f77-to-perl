#!/bin/sh
# Set the policy to what you want
# Do a run with -verbose=8 to find the name
perlcritic --verbose=5 --single-policy Variables::ProhibitUnusedVariables lib/Fortran/ >tmp/single_policy.out


