#!/bin/sh
option=${1:-all}
path="../build/data/benchmarks/"
execfile="../build/test"
common_extension=".smips.vmh"

case $option in
	all)
		for x in $path*$common_extension
		do
			echo $x...
			cp $x code.mem
			$execfile > out.tex
		done
		;;
	*)
		file=$path$option$common_extension
		echo $file...
		cp $file code.mem
		if [ $? -eq 0 ]
		then
			$execfile > out.tex
		fi
		;;
esac
