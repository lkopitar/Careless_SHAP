# ------------------------------------------
# this syntax runs a UNIX-based systems only
# ------------------------------------------
.libPaths('/gpfs/home08/0036/uk057365/lib')
library(foreach)
library(iterators)
library(doParallel)
library(tidyverse)
library(caret)
library(gbm)
library(data.table)

registerDoParallel(20)
i_start <- 1
i_end <- 200

percent_cr <- 15
# n_rr_test <- 171; n_cr_test <- 9   #  5 % prevalence
# n_rr_test <- 162; n_cr_test <- 18  # 10 % prevalence
n_rr_test <- 153; n_cr_test <- 27  # 15 % prevalence
cond <- "pattern"

						 
# https://stackoverflow.com/questions/60971747/foreach-and-doparallel-instead-of-for-loop-in-r
res <- foreach(i = i_start:i_end, .combine = rbind) %dopar% {
  			 
  dat <- read.csv2(paste0("~/simulation_600/dat_", cond, "/dat_", (i - i_start + 1),
  "_", cond, ".csv")) %>% 
  data.table() %>%
  mutate(id = row_number()) %>% 
  group_split(., Careless)
  
  # regular
  set.seed(10121976+i)
  test_set_rr <- dat[[1]] %>% sample_n(., n_rr_test, replace = F)
  train_set_rr  <- anti_join(dat[[1]], test_set_rr, by = 'id')
  
  # careless
  set.seed(10121976+i)
  test_set_cr <- dat[[2]] %>% sample_n(., n_cr_test, replace = F)
  train_set_cr  <- anti_join(dat[[2]], test_set_cr, by = 'id')
  
  test_set <- rbind(test_set_rr, test_set_cr) %>% 
    dplyr::select(., -id) %>% 
    sample_frac(.)
  
  train_set <- rbind(train_set_rr, train_set_cr) %>% 
    dplyr::select(., -id) %>% 
    sample_frac(.)
  
  # http://topepo.github.io/caret/model-training-and-tuning.html
  # For a gradient boosting machine (GBM) model, there are three main tuning parameters:
  # * number of iterations, i.e. trees, (called n.trees in the gbm function)
  # * complexity of the tree, called interaction.depth
  # * learning rate: how quickly the algorithm adapts, called shrinkage
  # * the minimum number of training set samples in a node to commence splitting (n.minobsinnode)
  
  my_grid <- expand.grid(interaction.depth = c(2,3,4),
                         n.minobsinnode = 4:10,
                         n.trees = (5:16)*50,
                         shrinkage = seq(.001, .03, .002))
						 
  # negative Log Loss with classProbs = TRUE
  train_set$Careless <- ifelse(train_set$Careless == 0, 'zeros', 'ones')
  train_set$Careless <- factor(train_set$Careless)
  train_set$Careless <- relevel(train_set$Careless, ref = 'zeros')
  
  # https://www.r-bloggers.com/dealing-with-unbalanced-data-in-machine-learning/
  gbm_class <- train(Careless ~ ., data = train_set, 
                     method = 'gbm',
                     metric  = 'logLoss',
                     trControl = trainControl(method = 'cv',
                                              number = 10,
                                              summaryFunction = mnLogLoss,
                                              classProbs = TRUE,
                                              trim = TRUE,
                                              returnData = FALSE,
                                              allowParallel = TRUE,
                                              sampling = "up"),
                     tuneGrid = my_grid,
                     verbose = FALSE)
  
  # plot(gbm_class)
  # print(gbm_class)
  
  gbm_class_preds <- predict(gbm_class, test_set)
  gbm_class_preds <- ifelse(gbm_class_preds == 'zeros', 0, 1)
  gbm_class_preds <- factor(gbm_class_preds)
  
  
  (cm <- confusionMatrix(
    data = gbm_class_preds,
    reference = factor(test_set$Careless),
    positive = '1'))
 
  rm(test_set)
  rm(train_set)
  rm(dat)
  gc()
 
  return(data.frame(i, t(data.frame(c( round(cm$overall, 12),
                                       round(cm$byClass, 12) )))))
									   
}
res
name <- paste0("res_sim_15p_", i_start, "_", i_end, "_", Sys.Date(), ".csv")
write.table(res, name, sep=";", dec=",", quote = FALSE,
            row.names = FALSE, col.names = FALSE)

# Stop the cluster
# stopCluster(cl)
# registerDoSEQ()
