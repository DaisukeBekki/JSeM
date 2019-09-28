#1/bin/bash
nkf -w -Lu | stack exec tsv2xml | tidy --tab-size 2 -xml -utf8 -indent -quiet
