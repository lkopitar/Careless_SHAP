# install.packages("GenOrd")
library(tidyverse)
library(GenOrd)

# simulate data ----
# 1. Extract correlation matrix and marginal distribution from real dataset
# 2. set.seed() and ordsample() with n observations
# 3. create random responses

# 1. Extract (polychoric) correlation matrix of the recoded data_set...
hexaco <- read.csv2("data_mod_resp.csv") %>% 
  filter(Careless == 0) %>% 
  dplyr::select(contains('HE01')) %>% 
  rename_all(funs(str_replace(., "_r", ""))) %>% 
  rename_all(funs(str_replace(., "HE01", "sim")))
(cor_mtx_sim <- stats::cor(hexaco, use="p"))
# describe(cor_mtx_sim[lower.tri(cor_mtx_sim, diag = FALSE)])
hist(cor_mtx_sim[lower.tri(cor_mtx_sim, diag = FALSE)])

# ... and item-wise marginal distribution from real dataset
marg <- apply(hexaco, 2, function (z) { 
  cumsum(prop.table(table(z))) 
})
a <- apply(t(marg[1:4, ]), 1, function(z) { list(z) })
b <- lapply(a, "[[", 1)
corrcheck(b)


# 2. set.seed() and ordsample() with n observations
# sample sizes are based on the empirical data, i.e. rr (361) and cr (244), from which
# a random sample will be drawn for validation purpose: rr (144) and cr (16)
n_rr <- 540
n_cr <- 60

# attention: 1000 iterations takes a lot of time
set.seed(10121976)
for (i in 1:1000) {
    for (cond in c("uni", "mid", "pattern")) {
      # simulate regular responses
    dat_valid <- ordsample(n_rr, b, cor_mtx_sim)
    # mean(cor_mtx_sim - cor(dat_valid))

    # 3. create random responses and append 
    dat_cr <- as.data.frame(matrix(0, ncol = 60, nrow = n_cr))
    colnames(dat_cr) <- paste0("sim_", 1:60)
    dat_cr_mid <- dat_cr_uni <- dat_cr_pattern <- dat_cr
    
    dat_cr_mid <- t(apply(dat_cr[1:n_cr, ], 1, function(z) sample(5, 60, prob = c(.05, .20, .50, .20, .05), replace = T)))
    dat_cr_uni <- t(apply(dat_cr[1:n_cr, ], 1, function(z) sample(5, 60, prob = c(.20, .20, .20, .20, .20), replace = T)))
    dat_cr_pattern <- t(apply(dat_cr[1:n_cr, ], 1, function(z) rep(sample.int(5,1):sample.int(5,1), length.out=60)))

    dat_sim_mid <- rbind(dat_valid, dat_cr_mid) %>% 
      as.data.frame(.) %>% 
      mutate(Careless = c(rep(0, n_rr), rep(1, n_cr))) %>% 
      write.csv2(file = paste0("simulation_600/dat_mid/dat_", i, "_mid.csv"), row.names = FALSE)
    dat_sim_uni <- rbind(dat_valid, dat_cr_uni) %>% 
      as.data.frame(.) %>% 
      mutate(Careless = c(rep(0, n_rr), rep(1, n_cr))) %>% 
      write.csv2(file = paste0("simulation_600/dat_uni/dat_", i, "_uni.csv"), row.names = FALSE)
    dat_sim_pattern <- rbind(dat_valid, dat_cr_pattern) %>% 
      as.data.frame(.) %>% 
      mutate(Careless = c(rep(0, n_rr), rep(1, n_cr))) %>% 
      write.csv2(file = paste0("simulation_600/dat_pattern/dat_", i, "_pattern.csv"), row.names = FALSE)
    }
}

