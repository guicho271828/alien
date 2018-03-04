#!/bin/bash +x

[ -z $PBS_O_WORKDIR ] || cd $PBS_O_WORKDIR

tmp=$(mktemp -d)
echo $tmp
chmod 777 $tmp

export PATH=/usr/local/bin:$PATH

echo "problem: ${problem:=$1}"
echo "options: ${options:=$2}"

dir=$(dirname $problem)

basename=$dir/$(basename $problem .pddl)


if [ -f $dir/domain.pddl ]
then
    domain=$dir/domain.pddl
elif [ -f $basename-domain.pddl ]
then
    domain=$basename-domain.pddl
else
    echo "no domain file!" >&2
    exit 1
fi

plan=$basename.plan
out=$basename.out
err=$basename.err


t_problem=$tmp/$(basename $problem)
t_domain=$tmp/$(basename $domain)
t_plan=$tmp/$(basename $plan)
t_err=$tmp/$(basename $err)
t_out=$tmp/$(basename $out)

cp -t $tmp $problem $domain

trap "cp -u -t $dir $tmp/*; rm -r $tmp" exit

singularity run -C -H $tmp planner.img $options $t_domain $t_problem $t_plan > $t_out 2> $t_err
