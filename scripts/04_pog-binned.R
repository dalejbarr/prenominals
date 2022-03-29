options(crayon.enabled = FALSE, tidyverse.quiet = TRUE)
suppressPackageStartupMessages(library("dplyr"))
library("tidyr")
library("ggplot2")

boot_once <- function(x) {
  x %>%
    nest(data = c(-SessionID)) %>%
    slice_sample(n = nrow(.), replace = TRUE) %>%
    unnest(data)
}

bin_up <- function(x, full = FALSE) {
  pog_y <- x %>%
    count(Speaker, Listener, bin, Image) %>%
    rename(Y = n)

  pog_n <- x %>%
    count(Speaker, Listener, bin) %>%
    rename(N = n)

  ff <- inner_join(pog_y, pog_n, c("Speaker", "Listener", "bin")) %>%
    mutate(p = Y / N) %>%
    filter(bin >= -800) %>%
    select(-Y, -N) %>%
    complete(Image, nesting(Speaker, Listener, bin),
	     fill = list(p = 0)) %>%
	       arrange(Speaker, Listener, bin, Image)

  if (full)
    return(ff)
  else
    return(ff$p)
}

set.seed(62)
bad_sessions <- readRDS("data-raw/sess-exclude.rds")
resp <- readRDS("data-raw/resp.rds") %>%
  anti_join(bad_sessions, "SessionID") %>%
  filter(ImgChanged %in% c("Competitor", "Competitor Contrast"),
	 Clicked == "Target")

pog <- readRDS("data-derived/pog-aligned.rds")
## bin up the gaze data into 48 ms bins

pog_bin <- pog %>%
  inner_join(resp %>% select(SessionID, RespID, Speaker, Listener),
	     "RespID") %>%
  mutate(Image = ifelse(is.na(Image), "Blank", Image) %>%
	   factor(levels = c("Target", "Target Contrast",
		    "Competitor", "Competitor Contrast", "Blank")),
	 bin = floor((frame_n + 6L) / 12) * 48L) %>%
  filter(bin >= -912L)

pog_pad <- pog_bin %>%
  group_by(Speaker, Listener, bin) %>%
  summarize(p_ongoing = 1 - mean(pad),
	    .groups = "drop")

orig <- bin_up(pog_bin, TRUE) %>%
  inner_join(pog_pad, c("Speaker", "Listener", "bin"))

bsamp <- replicate(1000L,
		   pog_bin %>% boot_once() %>% bin_up())

boot_ci <- apply(bsamp, 1, quantile, probs = c(.025, .975))

orig$p_min <- boot_ci["2.5%", ]
orig$p_max <- boot_ci["97.5%", ]

saveRDS(pog_bin, "data-derived/pog-bin.rds")
saveRDS(orig, "data-derived/gaze-plot-full.rds")
