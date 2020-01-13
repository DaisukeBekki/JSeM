#!/bin/bash
rm data/v1.0/*.xml
git checkout -- data/v1.0/*.xml
rm data/v1.0/Adjectives.txt
rm data/v1.0/Attitudes.txt
rm data/v1.0/Comparatives.txt
rm data/v1.0/Ellipsis.txt
rm data/v1.0/GeneralizedQuantifier.txt
rm data/v1.0/NominalAnaphora.txt
rm data/v1.0/Plurals.txt
rm data/v1.0/TemporalReference.txt
rm data/v1.0/Verbs.txt
rm stat.txt
git checkout -- data/v1.0/Adjectives.txt
git checkout -- data/v1.0/Attitudes.txt
git checkout -- data/v1.0/Comparatives.txt
git checkout -- data/v1.0/Ellipsis.txt
git checkout -- data/v1.0/GeneralizedQuantifier.txt
git checkout -- data/v1.0/NominalAnaphora.txt
git checkout -- data/v1.0/Plurals.txt
git checkout -- data/v1.0/TemporalReference.txt
git checkout -- data/v1.0/Verbs.txt
git checkout -- stat.txt
