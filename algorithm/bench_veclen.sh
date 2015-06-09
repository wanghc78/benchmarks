#!/bin/bash

cd LR
echo LR-n 1M samples, 15 iterations. n = 1
Rscript --vanilla LR_lms_lapply.R 1000000 1 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 1 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 2
Rscript --vanilla LR_lms_lapply.R 1000000 2 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 2 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 3
Rscript --vanilla LR_lms_lapply.R 1000000 3 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 3 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 4
Rscript --vanilla LR_lms_lapply.R 1000000 4 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 4 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 5
Rscript --vanilla LR_lms_lapply.R 1000000 5 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 5 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 6
Rscript --vanilla LR_lms_lapply.R 1000000 6 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 6 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 7
Rscript --vanilla LR_lms_lapply.R 1000000 7 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 7 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 8
Rscript --vanilla LR_lms_lapply.R 1000000 8 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 8 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 9
Rscript --vanilla LR_lms_lapply.R 1000000 9 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 9 15 | ../../../valor/report.py
echo LR-n 1M samples, 15 iterations. n = 10
Rscript --vanilla LR_lms_lapply.R 1000000 10 15 | ../../../valor/report.py
Rscript --vanilla LR_lms_lapply_cmp.R 1000000 10 15 | ../../../valor/report.py
cd ..

