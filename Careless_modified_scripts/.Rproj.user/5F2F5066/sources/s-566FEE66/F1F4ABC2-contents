library(tidyverse)

start <- "data_mod_"
end <- "_1_100_2020-12-30.csv"

names <- c("dem", "resp", "rt", "resp_dem", "rt_dem", "resp_rt", "all", "all_extracted")

res_cond <- read.csv2(paste0(start, names[1], end))
for(name in names[2:8]) {
  res_cond <- rbind(res_cond, read.csv2(paste0(start, name, end))) 
}
res_cond$mod <- rep(c(1:8), each = 100, len = 800)

res_cond %>% group_by(mod) %>% 
  summarize_all(funs(mean(., na.rm = TRUE),
                     sd(., na.rm = TRUE))) %>% 
  dplyr::select("Accuracy_mean", "Accuracy_sd",
         "Sensitivity_mean", "Sensitivity_sd",
         "Specificity_mean", "Specificity_sd",
         "Balanced.Accuracy_mean", "Balanced.Accuracy_sd",
         "AUC_mean", "AUC_sd") %>% 
  t %>% 
  magrittr::set_colnames(names) %>% 
  round(., 3)

