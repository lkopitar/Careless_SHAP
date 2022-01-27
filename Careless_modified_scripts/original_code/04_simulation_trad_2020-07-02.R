library(tidyverse)
library(careless)
library(caret)

source("03_trad_detection_fun_2020-07-02.R")

percent_cr <- 5
n_rr_test <- 171; n_cr_test <- 9   #  5 % prevalence
# n_rr_test <- 162; n_cr_test <- 18  # 10 % prevalence
# n_rr_test <- 153; n_cr_test <- 27  # 15 % prevalence
cond <- "pattern"
iterations <- 1000

model_data <- list.files(path = paste0("data\\sim_600\\dat_", cond, "\\"),
                         pattern = '.csv$', full.names = T) %>% map(read.csv2) %>% map(data.table)

# Sample an amount of respondents
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
# Intra-individual response variability ----
# Polytomous U3 statistic ----
# -------------------------------------------
res_maha <- res_ant <- res_irv <- res_u3poly <- acc_results

set.seed(10121976)
for (i in 1:iterations) {
  dat <- model_data[[i]] %>% 
    mutate(id = row_number()) %>% 
    group_split(., Careless)
  
  # draw random test sample
  test_set_rr <- dat[[1]] %>% sample_n(., n_rr_test, replace = F)
  test_set_cr <- dat[[2]] %>% sample_n(., n_cr_test, replace = F)
  test_set_lab <- rbind(test_set_rr, test_set_cr) %>% 
    dplyr::select(., -id) %>% 
    sample_frac(.)
  test_set_unlab <- test_set_lab %>% dplyr::select(-Careless)
  
  # res_maha[, i] <- acc.mahad(test_set_unlab, test_set_lab)
  # res_ant[, i] <- acc.psych(test_set_unlab, test_set_lab, -0.2)
  res_irv[, i] <- acc.irv(test_set_unlab, test_set_lab, percent_cr, 3)
  # res_u3poly[, i] <- acc.u3poly(test_set_unlab, test_set_lab)
}

write.csv2(t(res_maha), file = paste0("results/res_sim600_", cond, "_", percent_cr, "_maha.csv"))
write.csv2(t(res_ant), file = paste0("results/res_sim600_", cond, "_", percent_cr, "_ant.csv"))
write.csv2(t(res_irv), file = paste0("results/res_sim600_", cond, "_", percent_cr, "_irv.csv"))
write.csv2(t(res_u3poly), file = paste0("results/res_sim600_", cond, "_", percent_cr, "_u3poly.csv"))


# -------------------------------------------
# even-odd consistency ----
# -------------------------------------------
# attention: needs item in order of the scales
items_hh <- paste0(c("sim_"), c("06", "30", "54", "12", "36", "60", "18", "42", "24", "48"))
items_em <- paste0(c("sim_"), c("05", "29", "53", "11", "35", "17", "41", "23", "47", "59"))
items_ex <- paste0(c("sim_"), c("04", "28", "52", "10", "34", "58", "16", "40", "22", "46"))
items_ag <- paste0(c("sim_"), c("03", "27", "09", "33", "51", "15", "39", "57", "21", "45"))
items_co <- paste0(c("sim_"), c("02", "26", "08", "32", "14", "38", "50", "20", "44", "56"))
items_op <- paste0(c("sim_"), c("01", "25", "07", "31", "13", "37", "49", "19", "43", "55"))
all_items_reorder <- c(items_hh, items_em, items_ex, items_ag, items_co, items_op)
res_evod <- acc_results


set.seed(10121976)
for (i in 1:iterations) {
  dat_reorder <- model_data[[i]] %>% 
    dplyr::select(Careless, all_of(all_items_reorder)) %>% 
    mutate(id = row_number()) %>% 
    group_split(., Careless)
  
  # draw random test sample
  test_set_rr <- dat_reorder[[1]] %>% sample_n(., n_rr_test, replace = F)
  test_set_cr <- dat_reorder[[2]] %>% sample_n(., n_cr_test, replace = F)
  test_set_lab <- rbind(test_set_rr, test_set_cr) %>% 
    dplyr::select(., -id) %>% 
    sample_frac(.)
  test_set_unlab <- test_set_lab %>% dplyr::select(-Careless)

  res_evod[, i] <- acc.evenodd(test_set_unlab, test_set_lab)
}
# rowMeans(res_evod[, ], na.rm=TRUE)
write.csv2(t(res_evod), file = paste0("results/res_sim600_", cond, "_", percent_cr, "_evod.csv"))


# -------------------------------------------
# longstring ----
# -------------------------------------------
# attention: longstring needs model_data_unrecoded
# since the simulated data set is already recoded, it is necessary to do it again
items_r <- c("(01|09|10|12|14|15|19|20|21|24|26|28|30|31|32|35|41|42|44|46|48|49|52|53|55|56|57|59|60)")
res_long <- acc_results

recode2 <- function(x) {
  return(car::recode(x, '5=1; 4=2; 2=4; 1=5'))
}

set.seed(10121976)
for (i in 1:iterations) {
  dat_unrecoded <- model_data[[i]] %>% 
    mutate_at(.vars = vars(matches(paste0("^sim_.*", items_r))),
              .funs = list(recode2)) %>% 
    mutate(id = row_number()) %>% 
    group_split(., Careless)
  
  # draw random test sample
  test_set_rr <- dat_unrecoded[[1]] %>% sample_n(., n_rr_test, replace = F)
  test_set_cr <- dat_unrecoded[[2]] %>% sample_n(., n_cr_test, replace = F)
  test_set_lab <- rbind(test_set_rr, test_set_cr) %>% 
    dplyr::select(., -id) %>% 
    sample_frac(.)
  test_set_unlab <- test_set_lab %>% dplyr::select(-Careless)
  
  res_long[, i] <- acc.long(test_set_unlab, test_set_lab)
}
rowMeans(res_long[, ], na.rm=TRUE)
write.csv2(t(res_long), file = paste0("results/res_sim600_", cond, "_", percent_cr, "_long.csv"))

