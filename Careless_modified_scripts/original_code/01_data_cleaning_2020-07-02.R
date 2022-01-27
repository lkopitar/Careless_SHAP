library(tidyverse)
library(haven)
library(ggplot2)
library(careless)
library(plotly)

online_study <- read.csv2("careless.csv")

# Numbers for Figure OS 01
# table(online_study$IN01)
# table(online_study$IN01, online_study$IN06)
# table(online_study$IN01, online_study$LASTPAGE)
# table(online_study[online_study$LASTPAGE==12, c("IN01", "IN02")])

online_study <- online_study %>% 
  replace(. == '', NA) %>%
  replace(. == '-6', NA) %>%
  mutate_at(vars(everything()), ~as.numeric(.)) %>% 
  mutate(IN06.r = case_when(IN06_01 == 2 ~ 1,   # I should work carefully.
                            IN06_02 == 2 ~ 2,   # I should work quickly and inattentively.
                            IN06_03 == 2 ~ 3,   # I should quickly give credible answers.
                            IN06_04 == 2 ~ 4),  # I should give answers that make me look good. (no corresponding instruction)
         
         IN08.r = case_when(IN08_01 == 2 ~ 1, 
                            IN08_02 == 2 ~ 2, 
                            IN08_03 == 2 ~ 3, 
                            IN08_04 == 2 ~ 4)) %>% 
   filter(IN06.r == IN08.r,                     # Participants have to remember the instruction correctly
          IN08 == 1,                            # and only one response option is selected
          IN02 == 1)                            # Participants confirmed that they followed the instruction

table(online_study[online_study$LASTPAGE==12 &     # pp. completed the questionnaire
                      online_study$IN02 == 1 &
                      online_study$IN08 == 1, c("IN06.r", "IN08.r")])


# Define careless and regular respondents ----
relevant_respondents <- online_study %>% 
  mutate_at(vars(starts_with('IN08_')), function(x){
    x - 1
  }) %>%
  mutate(Careless = ifelse(IN08_02 == 1, 1, 0)) %>%
  filter(IN08_03 == 0, IN08_04 == 0, LASTPAGE == 12)


# handling of reaction times ----
# Calculating the count of missings in HE - columns ----
relevant_respondents <- relevant_respondents %>% 
  dplyr::select(starts_with('HE04')) %>% 
  mutate_all(~is.na(.)) %>% 
  rowSums() %>% 
  data.frame() %>% 
  set_names('NA_count_HE4') %>% 
  cbind(relevant_respondents)

relevant_respondents %>% 
  group_by(Careless) %>% 
  summarise(mean(NA_count_HE4))


na_count_plt <- relevant_respondents %>% 
  mutate_at('Careless', factor) %>% 
  ggplot() + geom_density(mapping = aes(NA_count_HE4, fill = Careless), 
                          alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 35, 5), limits = c(0, 35)) + theme_bw() + 
  ggtitle('Number of Missing entries on HE4-items of all respondents') + 
  xlab('Number of Missing Entries')


# winsorizing reaction time data ----
# https://www.r-bloggers.com/winsorization/
winsor2 <- function (x, multiple = 3) {
  q95 <- quantile(x, probs = 0.95, na.rm = T)
  ifelse(x > q95, q95, x)
}

rel_res <- relevant_respondents %>% 
  # group_by(Careless) %>%
  filter(., NA_count_HE4 < 20) %>% # ansonsten ist die Zeitsch?tzung nicht zuverl?ssig
  mutate_at(.vars = vars(starts_with('HE04')),
            .funs = list(wins = winsor2)) %>% 
  # rowMeans * 10 instead of rowSums due to missingness
  mutate('time_p1' = 10 * rowMeans(dplyr::select(.,HE04_01_wins:HE04_10_wins), na.rm = TRUE),
         'time_p2' = 10 * rowMeans(dplyr::select(.,HE04_11_wins:HE04_20_wins), na.rm = TRUE),
         'time_p3' = 10 * rowMeans(dplyr::select(.,HE04_21_wins:HE04_30_wins), na.rm = TRUE),
         'time_p4' = 10 * rowMeans(dplyr::select(.,HE04_31_wins:HE04_40_wins), na.rm = TRUE),
         'time_p5' = 10 * rowMeans(dplyr::select(.,HE04_41_wins:HE04_50_wins), na.rm = TRUE),
         'time_p6' = 10 * rowMeans(dplyr::select(.,HE04_51_wins:HE04_60_wins), na.rm = TRUE)) %>% 
  mutate_at(.vars = vars(starts_with('time_p')),
            .funs = round, 2) %>%  
  mutate('duration_hexaco' = rowSums(dplyr::select(., time_p1:time_p6), na.rm = TRUE)/60000) %>% 
  mutate_at('Careless', factor)


# some descriptives of the final sample
rel_res %>% group_by(Careless) %>% count

# 1=female, 2=male, 3=diverse
rel_res %>% count(sex) %>% 
  mutate(prop = prop.table(n))

# SO06 occupation
rel_res %>% count(SO06) %>% 
  mutate(prop = prop.table(n))
# psych::describe(rel_res[rel_res$age <= 90, "age"])
psych::describe(rel_res$age)

Duration_plot <- rel_res %>% 
  mutate_at('Careless', factor) %>% 
  ggplot() + geom_density(mapping = aes(duration_hexaco, fill = Careless), 
                          alpha = 0.5) + 
  scale_x_continuous(breaks = seq(0, 15, 5), limits = c(0, 15)) + theme_bw() + 
  ggtitle('Duration in Minutes among both respondent groups') + 
  xlab('Minutes') + ylab('Density')
# ggplotly(Duration_plot)


# recoding reversly coded items
# -----------------------------
items_r <- c("(01|09|10|12|14|15|19|20|21|24|26|28|30|31|32|35|41|42|44|46|48|49|52|53|55|56|57|59|60)")
items_nr <- c("(02|03|04|05|06|07|08|11|13|16|17|18|22|23|25|27|29|33|34|36|37|38|39|40|43|45|47|50|51|54|58)")

items_p1 <- paste0(c("HE01_"), formatC(1:10, flag="0", width=2))
items_p2 <- paste0(c("HE01_"), 11:20)
items_p3 <- paste0(c("HE01_"), 21:30)
items_p4 <- paste0(c("HE01_"), 31:40)
items_p5 <- paste0(c("HE01_"), 41:50)
items_p6 <- paste0(c("HE01_"), 51:60)

recode2 <- function(x) {
  return(car::recode(x, '5=1; 4=2; 2=4; 1=5'))
}

# Generate data
data_mod_all <- rel_res %>% 
  mutate_at(.vars = vars(matches(paste0("^HE01_.*", items_r))),
            .funs = list(r = recode2)) %>%
  dplyr::select(Careless,
                matches(paste0("^HE01_.*", items_r, "_r")),
                matches(paste0("^HE01_.*", items_nr)),
                starts_with("time_p"),
                starts_with(c("sex", "age", "SO"))) %>%
  mutate_at('Careless', factor) %>% 
  drop_na

data_mod_unrecoded <- rel_res %>% 
  dplyr::select(Careless,
                starts_with("HE01"),
                starts_with("time_p"),
                starts_with(c("sex", "age", "SO"))) %>%
  mutate_at('Careless', factor) %>% 
  drop_na %>%
  dplyr::select(starts_with("HE01"))
                

data_mod_resp_rt <- data_mod_all %>% 
  dplyr::select(-starts_with(c("sex", "age", "SO")))

data_mod_resp_dem <- data_mod_all %>% 
  dplyr::select(-starts_with(c("time_p")))

data_mod_rt_dem <- data_mod_all %>% 
  dplyr::select(-matches(paste0("^HE01_.*", items_r, "_r")),
                -matches(paste0("^HE01_.*", items_nr)))

data_mod_resp <- data_mod_all %>%
  dplyr::select(matches(paste0("^HE01_.*", items_r, "_r")),
                matches(paste0("^HE01_.*", items_nr)))

data_mod_rt <- data_mod_all %>%
  dplyr::select(Careless, starts_with("time_p"))

data_mod_dem <- data_mod_all %>%
  dplyr::select(Careless, starts_with(c("sex", "age", "SO")))


write.csv2(data_mod_all, file = paste0("data_mod_all.csv"), quote = FALSE)

write.csv2(data_mod_resp_rt, file = paste0("data_mod_resp_rt.csv"), quote = FALSE)
write.csv2(data_mod_resp_dem, file = paste0("data_mod_resp_dem.csv"), quote = FALSE)
write.csv2(data_mod_rt_dem, file = paste0("data_mod_rt_dem.csv"), quote = FALSE)

write.csv2(data_mod_resp, file = paste0("data_mod_resp.csv"), quote = FALSE)
write.csv2(data_mod_rt, file = paste0("data_mod_rt.csv"), quote = FALSE)
write.csv2(data_mod_dem, file = paste0("data_mod_dem.csv"), quote = FALSE)

write.csv2(data_mod_unrecoded, file = paste0("data_mod_unrecoded.csv"), quote = FALSE)
