#!/bin/bash

if [ -z "$1" ]
then
    bsub -Is -W 2:00 -nnodes 1 -P cli115 $SHELL
else
    bsub -Is -W 2:00 -nnodes 1 -P $1 $SHELL
fi


# run a command on a batch node
#$ hostname
#batch2
#
# run a command on a compute node
#$ jsrun -n1 hostname
#a35n03
