library(tidyverse)
library(gmodels) # confidence interval - CI() function
library(plyr)

# ########################################
# # YOU MAY WANT TO SKIP THIS PART # GENERATING RESULTS
# ########################################
# getwd()
# # reading test dataset
# test_set <- readRDS("Careless_Shap_results/datasets/test_set_seed_1.rds")
# 
# # Is it a regular or careless response?
# class_rb = c("regular", "careless")
# 
# # Is response it misclassified by GBM model?
# misclas_rb = c("no", "yes")
# 
# # prepare an empty data.frame
# res = data.frame(type = c(),variable = c(), contribution = c(), variable_name = c(), variable_value = c(), sign = c(), label = c(), B = c())
# 
# # Proceed through all combinations, in total four (TP, FP. TN, FN)
# for (i in c(1:length(class_rb))){
#   for(j in c(1:length(misclas_rb))){
# 
#     if(class_rb[i] == "regular"){ # is it regular response?
#       if(misclas_rb[j] == "no"){ # that was correctly predicted by GBM
#         subset = filter(test_set[which(predicted == test_set[,"Careless"]),], Careless == 0)
#       }else{ # or that was incorrectly predicted by GBM
#         subset = filter(test_set[which(predicted != test_set[,"Careless"]),], Careless == 0)
#       }
#     }else{ # it is careless response
#       if(misclas_rb[j] == "no"){ # that was correctly predicted by GBM
#         subset = filter(test_set[which(predicted == test_set[,"Careless"]),], Careless == 1)
#       }else{  # or that was incorrectly predicted by GBM
#         subset = filter(test_set[which(predicted != test_set[,"Careless"]),], Careless == 1)
#       }
#     }
# 
#     # In a chosen combination go throught all responses (rows)
#     for(row_idx in dim(subset)[1]){
#       shap_results <- predict_parts(explainer = explain_gbm,
#                                     new_observation =  subset[row_idx,],
#                                     type = "shap",
#                                     B = 25)
#       # store shap results
#       res = rbind(res, data.frame(type = rep(paste0(class_rb[i], " misclass:",misclas_rb[j])),shap_results))
#     }
#     # write shap results
#     write.csv2(x = res,file = "Careless_Shap_results/results/SHAP_regular_careless_all.csv", row.names = FALSE)
#   }
# }

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {


  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}

display_SHAP_contribution <- function(res_2, display_n = 10, focus_type = "careless misclass:no", types = c("careless misclass:no", "regular misclass:no")){
  
  filtered_results <- filter(res_2, type %in% types)
  
  # get negative n vars based on focus type
  top_n <- filter(filtered_results, type == focus_type) %>% arrange(contribution) %>% head(10)
  top_n_vars <- top_n$variable_name
  subset_top_n_vars <- filter(filtered_results, variable_name %in% top_n_vars)
  subset_top_n_vars$variable_name <- factor(subset_top_n_vars$variable_name,levels = top_n_vars)
  
  plot_negative <- # Use 95% confidence intervals instead of SEM
    ggplot(subset_top_n_vars, aes(x=variable_name, y=contribution, fill=type)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=contribution-ci, ymax=contribution+ci),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    xlab("Variable name")+
    ylab("SHAP Contribution")+
    ggtitle(paste0("Variables with the lowest (negative) average SHAP contribution (bottom ",display_n,")"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  # get positive n vars based on focus type
  tail_n <- filter(filtered_results, type == focus_type) %>% arrange(contribution) %>% tail(10)
  tail_n_vars <- tail_n$variable_name
  subset_tail_n_vars <- filter(filtered_results, variable_name %in% tail_n_vars)
  subset_tail_n_vars$variable_name <- factor(subset_tail_n_vars$variable_name,levels = tail_n_vars)
  plot_positive <- # Use 95% confidence intervals instead of SEM
    ggplot(subset_tail_n_vars, aes(x=variable_name, y=contribution, fill=type)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=contribution-ci, ymax=contribution+ci),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
    xlab("Variable name")+
    ylab("SHAP Contribution")+
    ggtitle(paste0("Variables with the highest (positive) average Shap contribution (top ",display_n,")"))+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(list(plot_negative=plot_negative, plot_positive=plot_positive, filtered_results=filtered_results))
}

