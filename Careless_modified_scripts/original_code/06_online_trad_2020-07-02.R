library(tidyverse)
library(careless)
library(solitude)
library(caret)

source("03_trad_detection_fun_2020-07-02.R")

# Sample an amount of respondents
n_rr <- 162
n_cr <- 18
iterations <- 1000
acc_results <- matrix(nrow = 18, ncol = iterations)
rownames(acc_results) <- c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper",
                           "AccuracyNull", "AccuracyPValue", "McnemarPValue",
                           "Sensitivity", "Specificity", "Pos Pred Value",
                           "Neg Pred Value", "Precision", "Recall",
                           "F1", "Prevalence", "Detection Rate",
                           "Detection Prevalence", "Balanced Accuracy")


# -------------------------------------------
# Mahalanobi's distance ----
# Psychometric antonyms ----
# intra-individual response variability ----
# polytomous U3 statistic ----
# -------------------------------------------
norm_respo <- data_mod_resp %>% dplyr::filter(Careless == 0)
careless_respo <- data_mod_resp %>% dplyr::filter(Careless == 1)
res_maha <- res_ant <- res_irv <- res_u3poly <- acc_results

set.seed(10121976)
for (i in 1:iterations) {
  sample_norm <- norm_respo %>% sample_n(size = n_rr, replace = FALSE)
  sample_cr <- careless_respo %>% sample_n(size = n_cr, replace = FALSE)
  test_set_lab <- rbind(sample_norm, sample_cr) %>% sample_frac(.) # shuffle

  test_set_unlab <- test_set_lab %>% dplyr::select(-Careless)
  res_maha[, i] <- acc.mahad(test_set_unlab, test_set_lab)
  res_ant[, i] <- acc.psych(test_set_unlab, test_set_lab, -0.2)
  res_irv[, i] <- acc.irv(test_set_unlab, test_set_lab, 10)
  res_u3poly[, i] <- acc.u3poly(test_set_unlab, test_set_lab)
}
round(rowMeans(res_irv, na.rm = T), 2)
# write.csv2(t(res_maha), file="results/res_maha.csv")
# write.csv2(t(res_ant), file="results/res_ant.csv")
# write.csv2(t(res_irv), file="results/res_irv.csv")
write.csv2(t(res_u3poly), file="results/res_u3poly.csv")


# -------------------------------------------
# longstring ----
# -------------------------------------------
# attention: longstring needs dat_mod_unrecoded
norm_respo_un <- dat_mod_unrecoded %>% filter(Careless == 0)
careless_respo_un <- dat_mod_unrecoded %>% filter(Careless == 1)
res_long <- acc_results

set.seed(10121976)
for (i in 1:iterations) {
  sample_norm <- norm_respo_un %>% sample_n(size = n_rr, replace = FALSE)
  sample_cr <- careless_respo_un %>% sample_n(size = n_cr, replace = FALSE)
  test_set_lab <- rbind(sample_norm, sample_cr) %>% sample_frac(.) # shuffle
  
  test_set_unlab <- test_set_lab %>% dplyr::select(-Careless)
  res_long[, i] <- acc.long(test_set_unlab, test_set_lab)
}
write.csv2(t(res_long), file="results/res_long.csv")


# -------------------------------------------
# even-odd consistency ----
# -------------------------------------------
items_hh <- paste0(c("HE01_"), c("06", "30_r", "54", "12_r", "36", "60_r", "18", "42_r", "24_r", "48_r"))
items_em <- paste0(c("HE01_"), c("05", "29", "53_r", "11", "35_r", "17", "41_r", "23", "47", "59_r"))
items_ex <- paste0(c("HE01_"), c("04", "28_r", "52_r", "10_r", "34", "58", "16", "40", "22", "46_r"))
items_ag <- paste0(c("HE01_"), c("03", "27", "09_r", "33", "51", "15_r", "39", "57_r", "21_r", "45"))
items_co <- paste0(c("HE01_"), c("02", "26_r", "08", "32_r", "14_r", "38", "50", "20_r", "44_r", "56_r"))
items_op <- paste0(c("HE01_"), c("01_r", "25", "07", "31_r", "13", "37", "49_r", "19_r", "43", "55_r"))

model_data_reorder <- data_mod_resp %>% 
  dplyr::select(Careless, all_of(c(items_hh, items_em, items_ex, items_ag, items_co, items_op)))

norm_respo_reorder <- model_data_reorder %>% filter(Careless == 0)
careless_respo_reorder <- model_data_reorder %>% filter(Careless == 1)
res_evod <- acc_results

set.seed(10121976)
for (i in 1:iterations) {
  sample_norm <- norm_respo_reorder %>% sample_n(size = n_rr, replace = FALSE)
  sample_cr <- careless_respo_reorder %>% sample_n(size = n_cr, replace = FALSE)
  test_set_lab <- rbind(sample_norm, sample_cr) %>% sample_frac(.) # shuffle
  
  test_set_unlab <- test_set_lab %>% dplyr::select(-Careless)
  res_evod[, i] <- acc.evenodd(test_set_unlab, test_set_lab)
}
round(rowMeans(res_evod, na.rm = T), 2)
write.csv2(t(res_evod), file="results/res_evod.csv")

