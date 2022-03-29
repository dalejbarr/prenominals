options(crayon.enabled = FALSE, tidyverse.quiet = TRUE)
requireNamespace("plyr")
suppressPackageStartupMessages(library("dplyr"))
library("tidyr")
library("purrr")
library("parallel")

## strategy: get p-values using bootstrapping
##   then cluster randomization using permutation tests
permute <- function(x, sinf, unit) {
  ndat <- sinf %>%
    nest(data = c(-.data[[unit]]))
  ndat %>%
    mutate(Sx = sample(c(-1, 1), nrow(ndat), TRUE),
           Lx = sample(c(-1, 1), nrow(ndat), TRUE)) %>%
    unnest(data) %>%
    mutate(S = S * Sx, L = L * Lx) %>%
    select(-Sx, -Lx) %>%
    inner_join(x, c(unit, "Speaker", "Listener"))
}


## get p-values by bootstrapping standard errors
teststat_by_frame <- function(x, unit, nmc = 1000L) {
  boot <- function(x, id) {
    x %>%
      nest(d = c(-.data[[id]])) %>%
      slice_sample(n = nrow(.), replace = TRUE) %>%
      unnest(d)
  }  
  ## get parameter estimates for a single frame
  fit_once <- function(x) {
    capture.output(mod <-
      nnet::multinom(cbind(Competitor, Target, `Target Contrast`,
                           `Competitor Contrast`, Blank) ~ S * L, x))

    coef(mod)[1, ]
  }  
  fit_by_frame <- function(x) { # for a single frame
    dat_bin <- x %>%
      group_by(ms, S, L, Image) %>%
      summarize(Y = sum(Y), .groups = "drop") %>%
      pivot_wider(names_from = "Image", values_from = "Y")

    mods <- dat_bin %>%
      nest(data = c(-ms)) %>%
      mutate(coef = map(data, fit_once))

    matrix(unlist(mods$coef), nrow = 4,
           dimnames = list(names(mods$coef[[1]]),
             mods$ms))
  }  
  orig <- fit_by_frame(x)
  boot_ax <- replicate(nmc,
                       x %>%
                         boot(unit) %>%
                         fit_by_frame())
  std_err <- apply(boot_ax, c(1, 2), sd)
  z_val <- orig / std_err
  p_val <- apply(z_val, 1:2, function(x) 2 * (1 - pnorm(abs(x))))
  sign(orig) * (-log(p_val))
}

get_cluster_teststats <- function(x, unit, cl,
                                  nboot = 10L, nperm = 10L) {
  x2 <- x %>%
    mutate(ms = frame_adj * 4) %>%
    select(-frame_adj)

  uinf <- x2[, c(unit, "Speaker", "Listener")] %>%
    distinct() %>%
    mutate(S = ifelse(Speaker == "late", .5, -.5),
           L = ifelse(Listener == "late", .5, -.5))

  dat_orig <- x2 %>%
    inner_join(uinf, c(unit, "Speaker", "Listener"))

  orig_tstat <- teststat_by_frame(dat_orig, unit, nboot)

  res <- parLapply(cl, seq_len(nperm), function(ix, y, u, un, nb) {
    y %>% permute(u, un) %>% teststat_by_frame(un, nb)
  }, y = x2, u = uinf, un = unit, nb = nboot)

  tibble(run = 1:(nperm + 1),
         tstat = c(list(orig_tstat), res))
}

get_cluster_pvals <- function(x) {
  get_runs <- function(x) {
    detect_runs <- function(vec, crit = -log(.05)) {
      runinfo <- rle((abs(vec) >= crit) * sign(vec))
      res <- NULL
      runs <- which(runinfo$values != 0L)
      map_df(runs, function(r) {
        f0_ix <- reduce(runinfo$lengths[seq_len(r - 1)], `+`, .init = 1L)
        if (r == 1L) {
          f0_ix <- 1L
        }
        f1_ix <- f0_ix + runinfo$lengths[r[1]][[1]] - 1L
        tibble(f0 = names(vec)[f0_ix] %>% as.integer(),
               f1 = names(vec)[f1_ix] %>% as.integer(),
               cms = vec[f0_ix:f1_ix] %>% sum)
      })
    }
    plyr::adply(x[-1, ], 1, detect_runs, .id = "effect")
  }  
  get_cluster_pval <- function(data, cms) {
    h0_dist <- data %>%
      arrange(run) %>%
      `[[`("max_cms")
    sum(abs(h0_dist) >= abs(cms)) / length(h0_dist)
  }  
  clust <- x %>%
    mutate(cstat = map(tstat, get_runs)) %>%
    select(-tstat) %>%
    unnest(cstat)

  full_obs <- expand.grid(run = x %>% `[[`("run") %>% unique(),
              effect = clust %>% `[[`("effect") %>% unique(),
              stringsAsFactors = FALSE) %>%
    arrange(run, effect)

  max_stat_obs <- clust %>%
    group_by(run, effect) %>%
    summarize(max_cms = max(cms), .groups = "drop")

  max_stat <- left_join(full_obs, max_stat_obs, c("run", "effect")) %>%
    replace_na(list(max_cms = 0))

  clust_inf <- max_stat %>%
    arrange(run) %>%
    nest(data = c(-effect)) %>%
    inner_join(clust %>% filter(run == 1L), "effect") %>%
    select(-run)

  clust_inf %>%
    mutate(pval = map2_dbl(data, cms, get_cluster_pval),
           nhd_n = map_dbl(data, nrow)) %>%
    select(-data)
}

pog_subj <- readRDS("data-derived/pog-window-subj.rds")
pog_item <- readRDS("data-derived/pog-window-item.rds")

source("cluster-setup.R")

cl <- makeCluster(hosts)
clusterCall(cl, function(x) {
  suppressPackageStartupMessages(library("dplyr"))
  library("tidyr")
  library("purrr")
}) %>% invisible()
clusterExport(cl, c("permute", "teststat_by_frame"))

nboot <- 500L
nperm <- 1000L
## nboot <- 50L
## nperm <- 10L

message("calculating by-subject NHD from ", nperm, " permutations")
tstat_subj <- get_cluster_teststats(pog_subj, "SessionID", cl, nboot, nperm)
saveRDS(tstat_subj, "data-tmp/tstat_subj.rds")
clust_subj <- get_cluster_pvals(tstat_subj) %>%
  mutate(unit = "SessionID")

message("calculating by-item NHD from ", nperm, " permutations")
tstat_item <- get_cluster_teststats(pog_item, "ItemID", cl, nboot, nperm)
saveRDS(tstat_item, "data-tmp/tstat_item.rds")
clust_item <- get_cluster_pvals(tstat_item) %>%
  mutate(unit = "ItemID")

stopCluster(cl)

all_clust <- bind_rows(clust_subj, clust_item) %>%
  arrange(effect, f0, unit) 
print(all_clust, n = nrow(all_clust))

saveRDS(all_clust, "data-derived/cluster-detection-results.rds")
