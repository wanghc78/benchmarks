#!/bin/bash

cd LR
echo LR 1M samples, 15 iterations. tile = [1000 2000 5000 10000 20000 50000 100000 200000 500000]
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 1000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 2000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 5000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 10000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 20000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 50000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 100000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 200000 | ../../../valor/report.py
Rscript --vanilla LR-1var_lms_lapply_mtrans_tiling.R 1000000 15 500000 | ../../../valor/report.py
echo LR-n 1M samples with n = 10, 15 iterations. tile = [1000 2000 5000 10000 20000 50000 100000 200000 500000]
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 1000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 2000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 5000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 10000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 20000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 50000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 100000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 200000 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_mtrans_tiling.R 1000000 10 15 500000 | ../../../valor/report.py
cd ..

