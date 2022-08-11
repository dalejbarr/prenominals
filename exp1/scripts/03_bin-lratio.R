## ** Bin up the log ratio eye data for plotting **
options(crayon.enabled = FALSE, tidyverse.quiet = TRUE)
suppressPackageStartupMessages(library("dplyr"))
library("tidyr")

bin_up <- function(x, full = FALSE) {
  agg <- x %>%
    group_by(Speaker, bin) %>%
    summarize(comp = sum(Competitor),
              targ = sum(Target), .groups = "drop") %>%
    mutate(lratio = log((targ + .5) / (comp + .5))) %>%
    arrange(Speaker, bin)

  if (full) agg else agg$lratio    
}

## compute the main effect
## so that we can later calculate the standard error
calc_maineff <- function(x) {
  x %>%
    mutate(cond = Speaker) %>%
    select(cond, bin, lratio) %>%
    spread(cond, lratio) %>%
    mutate(eff = early - late) %>%
    `[[`("eff")
}

boot_once <- function(x) {
  nest(x, data = c(-SessionID)) %>%
    slice_sample(n = nrow(.), replace = TRUE) %>%
    unnest(data)
}

pog_subj_bin <- readRDS("data-derived/pog-window-subj-adjective.rds") %>%
  filter(Image %in% c("Target", "Competitor")) %>%
  mutate(bin = floor((frame_adj + 6) / 12) * 48) %>%
  group_by(SessionID, Speaker, Listener, bin, Image) %>%
  summarize(Y = sum(Y), .groups = "drop") %>%
  spread(Image, Y) %>%
  arrange(SessionID, Speaker, Listener, bin)

orig <- bin_up(pog_subj_bin, TRUE)

message("running 1000 monte carlo simulations...")
ix_eff <- replicate(1000,
                    pog_subj_bin %>% 
                    boot_once() %>%
                    bin_up(TRUE) %>%
                    calc_maineff())

sderr <- tibble(bin = orig$bin %>% unique() %>% sort(),
                se = apply(ix_eff, 1, sd))

orig2 <- orig %>%
  inner_join(sderr, "bin") %>%
  mutate(lmin = lratio - se,
         lmax = lratio + se) %>%
  select(-se, -comp, -targ)

saveRDS(orig2, "data-derived/pog-lratio-binned.rds")
