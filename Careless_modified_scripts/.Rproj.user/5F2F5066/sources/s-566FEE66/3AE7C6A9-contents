library(tidyverse)

cond <- "pattern"
percent_cr <- 15

url <- paste0("results/simulation/", percent_cr, "_", cond, "/")
file_name <- paste0("res_sim600_", cond, "_", percent_cr)

res_cond <- read.csv2(paste0(url, file_name, "_maha.csv")) %>% 
  rbind(., read.csv2(paste0(url, file_name, "_ant.csv"))) %>% 
  rbind(., read.csv2(paste0(url, file_name, "_evod.csv"))) %>% 
  rbind(., read.csv2(paste0(url, file_name, "_long.csv"))) %>% 
  rbind(., read.csv2(paste0(url, file_name, "_irv.csv"))) %>% 
  rbind(., read.csv2(paste0(url, file_name, "_u3poly.csv"))) %>% 
  rbind(., read.csv2(paste0(url, file_name, "_ml.csv"))) %>% 
  mutate('mod' = rep(c(1:7), each = 1000, len = 7000))

res_cond %>% group_by(mod) %>% 
  summarize_all(funs(mean(., na.rm = TRUE),
                     sd(., na.rm = TRUE))) %>% 
  dplyr::select("Accuracy_mean", "Accuracy_sd",
         "Sensitivity_mean", "Sensitivity_sd",
         "Specificity_mean", "Specificity_sd",
         "Precision_mean", "Precision_sd",
         "Balanced.Accuracy_mean", "Balanced.Accuracy_sd") %>% 
  t %>% 
  magrittr::set_colnames(c("Maha", "Ant", "EvenOdd", "Long", "IRV", "U3poly", "GBM")) %>% 
  round(., 2)

