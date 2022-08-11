options(crayon.enabled = FALSE, tidyverse.quiet = TRUE)
requireNamespace("plyr") ## for adply()
suppressPackageStartupMessages(library("dplyr"))
library("tidyr")
library("purrr")
library("parallel")

source("scripts/cluster-permutation-functions.R")
source("computing-cluster-setup.R")

pog_subj <- readRDS("data-derived/pog-window-subj-noun.rds") %>%
  rename(frame = frame_n) %>%
  select(-Listener)
pog_item <- readRDS("data-derived/pog-window-item-noun.rds") %>%
  rename(frame = frame_n) %>%
  select(-Listener)

cl <- makeCluster(hosts)
clusterCall(cl, function(x) {
  suppressPackageStartupMessages(library("dplyr"))
  library("tidyr")
  library("purrr")
}) %>% invisible()
clusterExport(cl, c("permute_intercept", "teststat_by_frame",
                    "boot", "fit_once", "fit_by_frame"))

if (length(commandArgs(TRUE)) == 2L) {
  nboot <- as.integer(commandArgs(TRUE)[1])
  nperm <- as.integer(commandArgs(TRUE)[2])
} else {
  ## just testing
  nboot <- 20L
  nperm <- 10L
}

tstat_subj <- get_cluster_teststats(pog_subj, "SessionID", cl,
                                    permute_intercept, nboot, nperm)

clust_subj <- get_cluster_pvals(tstat_subj) %>%
  filter(effect == "(Intercept)") %>%
  mutate(unit = "SessionID")

tstat_item <- get_cluster_teststats(pog_item, "ItemID", cl,
                                    permute_intercept, nboot, nperm)

clust_item <- get_cluster_pvals(tstat_item) %>%
  filter(effect == "(Intercept)") %>%
  mutate(unit = "ItemID")

stopCluster(cl)

all_clust <- bind_rows(clust_subj, clust_item) %>%
  arrange(effect, f0, unit) 
print(all_clust, n = nrow(all_clust))

saveRDS(all_clust, "data-derived/cluster-detection-results-noun.rds")
