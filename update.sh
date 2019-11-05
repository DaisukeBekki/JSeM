#!/bin/bash
stack runghc app/restore.hs -- data/JSeM_beta
mv data/JSeM_beta/*.txt data/v1.0/
stack runghc app/update.hs -- data/v1.0
