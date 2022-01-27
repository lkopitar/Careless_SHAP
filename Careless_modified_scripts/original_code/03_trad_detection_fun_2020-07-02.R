library(tidyverse)
library(careless)
library(PerFit)

# Mahalanobis Distance ----
acc.mahad <- function(df1, df2) {
  preds_mahad <- mahad(df1, plot = FALSE, flag = TRUE, confidence = 0.95)
  mahad_preds <- preds_mahad$flagged %>% as.numeric %>% factor
  
  cm <- confusionMatrix(
    data = mahad_preds,
    reference = factor(df2$Careless),
    positive = '1')
  
  return(as.matrix(c(cm$overall, cm$byClass)))
}

# psychometric synonyms ----
acc.psych <- function(df1, df2, crit_value) {
  preds_psychant <- psychant(df1, critval = crit_value, diag = TRUE)$cor %>% 
    replace_na(., 0) %>%
    as.numeric 
  hist(preds_psychant)
  
  cm <- confusionMatrix(
    data = factor(ifelse(preds_psychant > 0, 1, 0)),
    reference = factor(df2$Careless),
    positive = '1')
  
  return(as.matrix(c(cm$overall, cm$byClass)))
}

# even-odd consistency ----
acc.evenodd <- function(df1, df2) {
  preds_evenodd <- evenodd(df1, factors = rep(10, 6)) 
  
  cm <- confusionMatrix(
    data = factor(ifelse(preds_evenodd < 0, 1, 0)),
    reference = factor(df2$Careless),
    positive = '1')
  
  return(as.matrix(c(cm$overall, cm$byClass)))
}

# Longstring ----
acc.long <- function(df1, df2) {
  careless_long <- longstring(df1, avg = FALSE)
  long_preds <- (careless_long  >= 6) %>% as.numeric %>% factor
  
  cm <- confusionMatrix(
    data = long_preds,
    reference = factor(df2$Careless),
    positive = '1')
  
  return(as.matrix(c(cm$overall, cm$byClass)))
}
# relevant_respondents$long <- longstring(relevant_respondents, avg= FALSE)
# table(relevant_respondents$Careless, relevant_respondents$long)
# relevant_respondents[relevant_respondents$long == 60, ]

# intra-individual response variability (IRV) ----
acc.irv <- function(df1, df2, percent, num_split) {
  irv_res <- irv(df1, split = FALSE, num.split = 3)
  irv_cutoff <- quantile(irv_res, percent/100)
  irv_prds <- ifelse(irv_res <= irv_cutoff, 1, 0) %>%
  factor # Warning: both sides are possible (highest and lowest values)
  
  # using the highest values
  # irv_cutoff <- quantile(irv_res, (1-percent/100))
  # irv_prds <- ifelse(irv_res >= irv_cutoff, 1, 0) %>%
  # factor # Warning: both sides are possible (highest and lowest values)

  cm <- confusionMatrix(
    data = irv_prds,
    reference = factor(df2$Careless),
    positive = '1')  
  return(as.matrix(c(cm$overall, cm$byClass)))
}


# u3poly ----
acc.u3poly <- function(df1, df2) {
  U3poly_stat <- U3poly(df1-1, IRT.PModel = "GRM", Ncat=5)
  critval_U3poly <- cutoff(U3poly_stat, Blvl=.10)
  
  pred <- ifelse(U3poly_stat$PFscores$PFscores >= critval_U3poly$Cutoff, 1, 0)
  
  cm <- confusionMatrix(
    data = factor(pred),
    reference = factor(df2$Careless),
    positive = '1')
  
  return(as.matrix(c(cm$overall, cm$byClass)))
}
