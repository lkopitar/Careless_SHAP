library(tidyverse)
library(careless)
library(PerFit)

resp <- read.csv2("data_mod_resp.csv")
resp_unrecoded <- read.csv2("data_mod_unrecoded.csv")

# Longstring ----
res_longstring <- longstring(resp_unrecoded, avg = TRUE)
res_longstring$longstr1 <- longstring(resp_unrecoded[,1:10], avg = FALSE)
res_longstring$longstr6 <- longstring(resp_unrecoded[,51:60], avg = FALSE)
res_longstring$longstrDiff <- res_longstring$longstr1 - res_longstring$longstr6
hist(res_longstring$longstrDiff)

# intra-individual response variability (IRV) ----
res_irv <- irv(resp_unrecoded, split = TRUE, num.split = 6)[,c(1,2,7)]
res_irv$irvDiff <- res_irv$irv1 - res_irv$irv6

# Mahalanobis Distance ----
res_mahad <- mahad(resp, plot = FALSE, flag = FALSE, confidence = 0.95)
hist(res_mahad)

# psychometric synonyms ----
res_psycsyn <- psychant(resp, critval = -0.2, diag = TRUE)$cor %>% 
  replace_na(., 0) %>%
  as.numeric 
hist(res_psycsyn)

# even-odd consistency ----
items_hh <- paste0(c("HE01_"), c("06", "30_r", "54", "12_r", "36", "60_r", "18", "42_r", "24_r", "48_r"))
items_em <- paste0(c("HE01_"), c("05", "29", "53_r", "11", "35_r", "17", "41_r", "23", "47", "59_r"))
items_ex <- paste0(c("HE01_"), c("04", "28_r", "52_r", "10_r", "34", "58", "16", "40", "22", "46_r"))
items_ag <- paste0(c("HE01_"), c("03", "27", "09_r", "33", "51", "15_r", "39", "57_r", "21_r", "45"))
items_co <- paste0(c("HE01_"), c("02", "26_r", "08", "32_r", "14_r", "38", "50", "20_r", "44_r", "56_r"))
items_op <- paste0(c("HE01_"), c("01_r", "25", "07", "31_r", "13", "37", "49_r", "19_r", "43", "55_r"))

model_data_reorder <- resp %>% 
  dplyr::select(all_of(c(items_hh, items_em, items_ex, items_ag, items_co, items_op)))
res_evenodd <- evenodd(model_data_reorder, factors = rep(10, 6)) 
res_evenodd[is.na(res_evenodd)] <- 0

# u3poly ----
res_u3poly <- U3poly(resp-1, IRT.PModel = "GRM", Ncat=5)[[1]]

# Collect all extracted features
res <- cbind(res_longstring, res_irv, res_mahad, res_psycsyn, res_evenodd, res_u3poly) 
names(res)[13] <- "res_u3poly"

write.csv2(res, file = paste0("data_mod_extracted.csv"), quote = FALSE, row.names = F)

# prepare all + extracted
data <- read.csv2("data_mod_all.csv")
data <- cbind(data, res)
write.csv2(data, file = paste0("data_mod_all_extracted.csv"), quote = FALSE, row.names = F)
