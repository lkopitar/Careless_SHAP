# loading resuts
res = read.csv2("Careless_Shap_results/results/SHAP_regular_careless_all.csv")
# load helper script
source("Careless_Shap_results/scripts/SHAP_results_display_helper.R") 

# get summary by grouped by type and variable_name
# possible types: careless misclass:yes, careless misclass:no, regular misclass:yes, regular misclass:no
res_2 <- summarySE(res, measurevar="contribution", groupvars=c("type","variable_name"))

# COMPARISON 1
# get results from the helper's function. Display by groups "careless misclass:no", "regular misclass:no"
results <- display_SHAP_contribution(res_2 = res_2,display_n = 10, focus_type = "careless misclass:no", types =  c("careless misclass:no", "regular misclass:no"))
png("Careless_Shap_results/figures/CR_RR_correct_top_neg_contr.png", width = 700, height = 400, units = "px")
results$plot_negative
dev.off()
png("Careless_Shap_results/figures/CR_RR_correct_top_pos_contr.png", width = 700, height = 400, units = "px")
results$plot_positive
dev.off()

# COMPARISON 2
# get results from the helper's function. Display by groups "careless misclass:yes", "careless misclass:no"
results <- display_SHAP_contribution(res_2 = res_2,display_n = 10, focus_type = "careless misclass:yes", types =  c("careless misclass:yes", "careless misclass:no"))
png("Careless_Shap_results/figures/CR_correct_misclass_top_pos_contr.png", width = 700, height = 400, units = "px")
results$plot_positive
dev.off()
png("Careless_Shap_results/figures/CR_correct_misclass_top_neg_contr.png", width = 700, height = 400, units = "px")
results$plot_negative
dev.off()
#results$filtered_results

# COMPARISON 3
# get model fit (seed = 1) and train/test set (without extracted variables)
fit <- readRDS("Careless_Shap_results/model/model_gbm_seed_1_mod_all.rds")
train_set <- readRDS("Careless_Shap_results/datasets/train_set_mod_all_seed_1.rds")
test_set <- readRDS("Careless_Shap_results/datasets/test_set_mod_all_seed_1.rds")

# get model fit (seed = 1) and train/test set (including extracted variables)
fit_extracted <- readRDS("Careless_Shap_results/model/model_gbm_seed_1_mod_all_extracted.rds")
train_set_extracted <- readRDS("Careless_Shap_results/datasets/train_set_mod_all_extracted_seed_1.rds")
test_set_extracted <- readRDS("Careless_Shap_results/datasets/test_set_mod_all_extracted_seed_1.rds")

# create explainer with all data
explain_gbm <- DALEX::explain(model = fit,
                       data = train_set,
                       y = train_set$Careless == "ones",
                       label = "GBM (all)")

# get shap values for the first observation
shap <- DALEX::predict_parts(explainer = explain_gbm,
                      new_observation = test_set[1,],
                      type = "shap",
                      B = 25)

# create explainer with all_extracted data
explain_gbm_extracted <- DALEX::explain(model = fit_extracted,
                                 data = train_set_extracted,
                                 y = train_set$Careless == "ones",
                                 label = "GBM (all_extracted)")

# get shap values for the first observation
shap_extracted <- DALEX::predict_parts(explainer = explain_gbm_extracted,
                                new_observation = test_set_extracted[1,],
                                type = "shap",
                                B = 25)

# generate figures
png("Careless_Shap_results/figures/wo_extracted.png", width = 700, height = 400, units = "px")
plot(shap)
dev.off()

png("Careless_Shap_results/figures/w_extracted.png", width = 700, height = 400, units = "px")
plot(shap_extracted)
dev.off()
