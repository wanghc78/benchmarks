#!/bin/bash

cd k-NN
echo NN nonVec 10k 3D Training samples, 10K 3D testing samples, 10 clusters
Rscript --vanilla NN_lapply.R 10000 10000 10 | ../../../valor/report.py
echo NN Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters
Rscript --vanilla NN_lapply_cmp.R 10000 10000 10 | ../../../valor/report.py
echo k-NN nonVec 10k 3D Training samples, 10K 3D testing samples, 10 clusters, k=5
Rscript --vanilla k-NN_lapply.R 10000 10000 10 5 | ../../../valor/report.py
echo k-NN Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters, k=5
Rscript --vanilla k-NN_lapply_cmp.R 10000 10000 10 5 | ../../../valor/report.py
cd ..
cd LR
echo LR-OLS nonVec 1M points
Rscript --vanilla LR-1var_ols_lapply.R 1000000 | ../../../valor/report.py
echo LR-OLS Vec 1M points
Rscript --vanilla LR-1var_ols_lapply_cmp.R 1000000 | ../../../valor/report.py
echo LR-OLS-n nonVec 1M length 10 vec variable
Rscript --vanilla LR_ols_lapply.R 1000000 10| ../../../valor/report.py
echo LR-OLS-n Vec 1M length 10 vec variable
Rscript --vanilla LR_ols_lapply_cmp.R 1000000 10 | ../../../valor/report.py
cd ..
cd Pi
echo nonVec Monte Carlo 1M points
Rscript --vanilla Pi_lapply.R 1000000 | ../../../valor/report.py
echo Vec Monte Carlo 1M points
Rscript --vanilla Pi_lapply_cmp.R 1000000 | ../../../valor/report.py
cd ..
cd PCA
echo nonVec PCA 1M length 10 vector samples
Rscript --vanilla PCA_lapply.R 1000000 10 | ../../../valor/report.py
echo nonVec PCA 1M length 10 vector samples
Rscript --vanilla PCA_lapply_cmp.R 1000000 10 | ../../../valor/report.py
cd ..


