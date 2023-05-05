#!/bin/bash
# rm data/JSeM_beta/*.txt
# rm data/v1.0/*.xml
stack run restore -- data/JSeM_beta # JSeM_beta内のtsvファイルを更新する
cp data/JSeM_beta/*.txt data/v1.0/  # tsvファイルをv1.0にコピー
stack run update -- data/v1.0       # v1.0内のtsvファイルからxmlファイルを生成
