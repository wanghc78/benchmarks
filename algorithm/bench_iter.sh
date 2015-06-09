#!/bin/bash

cd ICA
echo ICA nonVec 1M samples, 2 signals, 15 iterations
Rscript --vanilla ica_lapply.R 1000000 2 15 | ../../../valor/report.py
echo ICA Vec 1M samples, 2 signals, 15 iterations
Rscript --vanilla ica_lapply_cmp.R 1000000 2 15 | ../../../valor/report.py
cd ..
cd k-means
echo k-Means nonVec 1M points, 10 cluster, 15 iterations
Rscript --vanilla k-means-1D_lapply.R 1000000 10 15 | ../../../valor/report.py
echo k-Means Vec 1M points, 10 cluster, 15 iterations
Rscript --vanilla k-means-1D_lapply_cmp.R 1000000 10 15 | ../../../valor/report.py
echo k-Means-nD nonVec 1M 3D points, 10 cluster, 15 iterations
Rscript --vanilla k-means_lapply.R 1000000 10 3 15 | ../../../valor/report.py
echo k-Means-nD Vec 1M 3D points, 10 cluster, 15 iterations
Rscript --vanilla k-means_lapply_cmp.R 1000000 10 3 15 | ../../../valor/report.py
cd ..
cd LogitRegression
echo LogitReg nonVec 1M points, 15 iterations
Rscript --vanilla LogitRegre-1var_lapply.R 1000000 15 | ../../../valor/report.py
echo LogitReg Vec 1M points, 15 iterations
Rscript --vanilla LogitRegre-1var_lapply_cmp.R 1000000 15 | ../../../valor/report.py
echo LogitReg-n nonVec 1M length 10 vec variable, 15 iterations
Rscript --vanilla LogitRegre_lapply.R 1000000 10 15 | ../../../valor/report.py
echo LogitReg-n Vec 1M length 10 vec variable, 15 iterations
Rscript --vanilla LogitRegre_lapply_cmp.R 1000000 10 15 | ../../../valor/report.py
cd ..
cd LR
echo LR nonVec 1M points, 15 iterations
Rscript --vanilla LR-1var_lms_lapply.R 1000000 15 | ../../../valor/report.py
echo LR Vec 1M points, 15 iterations
Rscript --vanilla LR-1var_lms_lapply_cmp.R 1000000 15 | ../../../valor/report.py
echo LR-n nonVec 1M length 10 vec variable, 15 iterations
Rscript --vanilla LR_lms_lapply.R 1000000 10 15 | ../../../valor/report.py
echo LR-n Vec 1M length 10 vec variable, 15 iterations
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 10 15 | ../../../valor/report.py
cd ..

