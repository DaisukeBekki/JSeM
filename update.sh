#!/bin/bash
# rm data/JSeM_beta/*.txt
rm data/v1.0/*.xml
stack exec restore -- data/JSeM_beta
mv data/JSeM_beta/*.txt data/v1.0/
stack exec update -- data/v1.0
