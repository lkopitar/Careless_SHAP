library(tidyverse)
library(ggpol)
library(gghalves)

names <- c("dem", "resp", "rt", "resp_dem", "rt_dem", "resp_rt", "all", "all_extracted")

start <- "data_mod_"
end <- "_1_100_2020-12-30.csv"

res <- read.csv2(paste0(start, names[1], end))
for(name in names[2:8]) {
  res <- rbind(res, read.csv2(paste0(start, name, end))) 
}
res$mod <- rep(c(1:8), each = 100, len = 800)

a <- aggregate(AUC ~  mod, res, mean)
res %>% group_by(mod) %>% 
  summarize_all(mean) %>% 
  dplyr::select('Specificity')

name.plot <- paste0("Figure_02_bal_acc_", Sys.Date() , ".pdf")
cairo_pdf(name.plot, family="Lucida Sans", width=12, height=8 ) # width = 10.15
par(mar=c(4.5,5,4,2))

# https://erocoar.github.io/gghalves/
set.seed(12345)
ggplot() +
  geom_half_boxplot(
    data = res, 
    aes(x = factor(mod), y = AUC, fill = factor(mod)),
    side = "l", 
    width = 0.55,
    size= 0.3,
    errorbar.draw = TRUE,
    outlier.color = NA) +
  geom_half_point(
    data = res %>% group_by(mod) %>% sample_n(100), 
    aes(x = factor(mod), y = AUC, color=factor(mod)),
    side = "r",
    transformation_params = list(height = 0, width = 1, seed = 1), alpha = .7) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = names) +
  labs(x="Detection Method", y = "Balanced Accuracy") +
  ylim(.3,.9) +
  scale_color_manual(values = c("1" = "#34638b", 
                                "2" = "#5c84a7", 
                                "3" = "#6498c4", 
                                "4" = "#76a7d1", 
                                "5" = "#94bee2", 
                                "6" = "#a9d3f7",
                                "7" = "#f3bb87", 
                                "8" = "#f7b06e", 
                                "9" = "#f19a48")) +
scale_fill_manual(values = c("#34638b", "#5c84a7", "#6498c4", "#76a7d1", "#94bee2", "#a9d3f7",
                           "#f3bb87", "#f7b06e", "#f19a48"))
  
# ggplotly(boxjitter_plot)

dev.off()

