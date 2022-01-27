library(tidyverse)
library(ggpol)
library(gghalves)

res <- read.csv2("results/online/res_maha.csv") %>% 
  rbind(., read.csv2("results/online/res_ant.csv")) %>% 
  rbind(., read.csv2("results/online/res_evod.csv")) %>% 
  rbind(., read.csv2("results/online/res_long.csv")) %>% 
  rbind(., read.csv2("results/online/res_irv.csv")) %>% 
  rbind(., read.csv2("results/online/res_u3poly.csv")) %>% 
  rbind(., read.csv2("results/online/res_ml_resp.csv")) %>%
  rbind(., read.csv2("results/online/res_ml_rt.csv")) %>%
  rbind(., read.csv2("results/online/res_ml_resp_rt.csv")) %>%
  mutate('mod' = rep(c(1:9), each = 1000, len = 9000))

a <- aggregate(Balanced.Accuracy ~  mod, res, mean)
res %>% group_by(mod) %>% 
  summarize_all(mean) %>% 
  dplyr::select('Specificity')

label_mod <- c("Maha.", "Ant.", "EvenOdd", "Long.", "IRV", 
               expression('U'['3']*'poly'),
               expression(GBM["Res"]),
               expression(GBM["RT"]),
               expression(GBM["Res+RT"]))

name.plot <- paste0("Figure_02_bal_acc_", Sys.Date() , ".pdf")
cairo_pdf(name.plot, family="Lucida Sans", width=6, height=4 ) # width = 10.15
par(mar=c(4.5,5,4,2))

# https://erocoar.github.io/gghalves/
set.seed(12345)
ggplot() +
  geom_half_boxplot(
    data = res, 
    aes(x = factor(mod), y = Balanced.Accuracy, fill = factor(mod)),
    side = "l", 
    width = 0.55,
    size= 0.3,
    errorbar.draw = TRUE,
    outlier.color = NA) +
  geom_half_point(
    data = res %>% group_by(mod) %>% sample_n(100), 
    aes(x = factor(mod), y = Balanced.Accuracy, color=factor(mod)),
    side = "r",
    transformation_params = list(height = 0, width = 1, seed = 1),
    alpha = .7) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = label_mod) +
  labs(x="Detection Method", y = "Balanced Accuracy") +
  ylim(0.3,.85) +
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
