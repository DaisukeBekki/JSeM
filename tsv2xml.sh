#1/bin/bash
cat $1 | nkf -w | stack exec tsv2xml | tidy --tab-size 2 --input-xml true --indent-cdata true -utf8 -indent
