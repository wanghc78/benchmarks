#!/bin/bash

cd k-means
echo k-Means Inner Apply Vec 1M points, 10 cluster, 15 iterations
Rscript --vanilla k-means-1D_lapply_mtrans_level2.R 1000000 10 15 | ../../../valor/report.py
echo k-Means Both Applys Vec 1M points, 10 cluster, 15 iterations
Rscript --vanilla k-means-1D_lapply_mtrans_opt.R 1000000 10 15 | ../../../valor/report.py
echo k-Means-nD Inner Apply Vec 1M 3D points, 10 cluster, 15 iterations
Rscript --vanilla k-means_lapply_mtrans_level2.R 1000000 10 3 15 | ../../../valor/report.py
echo k-Means-nD Inner Applys Vec 1M 3D points, 10 cluster, 15 iterations
Rscript --vanilla k-means_lapply_mtrans_opt.R 1000000 10 3 15 | ../../../valor/report.py
cd ..
cd k-NN
echo NN Inner Apply Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters
Rscript --vanilla NN_lapply_mtrans_level2.R 10000 10000 10 | ../../../valor/report.py
echo NN Both Applys Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters
Rscript --vanilla NN_lapply_mtrans.R 10000 10000 10 | ../../../valor/report.py
echo k-NN Inner Apply Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters, k=5
Rscript --vanilla k-NN_lapply_mtrans_level2.R 10000 10000 10 5 | ../../../valor/report.py
echo k-NN Both Applys Vec 10k 3D Training samples, 10K 3D testing samples, 10 clusters, k=5
Rscript --vanilla k-NN_lapply_mtrans.R 10000 10000 10 5 | ../../../valor/report.py
cd ..

