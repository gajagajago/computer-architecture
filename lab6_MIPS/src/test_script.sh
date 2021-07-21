#!/bin/sh
option=${1:-all}
path="../build/data/smips/"
execfile="../build/test"
common_name="smipsv2_"
common_extension=".S.vmh"

case $option in
	all)
		for x in $path$common_name*$common_extension
		do
			echo $x...
			cp $x code.mem
			$execfile > out.tex
		done
		;;
	*)
		file=$path$common_name$option$common_extension
		echo $file...
		cp $file code.mem
		if [ $? -eq 0 ]
		then
			$execfile > out.tex
		fi
		;;
esac
