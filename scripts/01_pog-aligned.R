## ** Align the POG data with speech events **
## 
## This script aligns each trial with two audio events (onset of
## adjective, =AdjOn=; onset of noun, =NounOn=), and deals with trial
## dropouts.

## The window of data that is kept for plotting extands from -138 frames
## before adjective onset (about 552\nbsp{}ms) until 257 frames after
## noun onset (1028\nbsp{}ms).  These numbers were chosen to allow equal
## numbers of observations in each of 48\nbsp{}ms bins for plotting,
## extending from the first bin at -528\nbsp{}ms before adjective onset
## to the last bin at 1008\nbsp{}ms after noun onset, where each bin is
## numbered by the median sample in the bin.

options(crayon.enabled = FALSE, tidyverse.quiet = TRUE)
suppressPackageStartupMessages(library("dplyr"))
library("tidyr")
library("purrr")
library("readr")

align_eyedata <- function(RespID, data, AdjOn, NounOn, NounOff, Clicked,
			  click_lag,
			  win_begin = -138L, win_end = 257L) {
  ## check that data is sampled at appropriate rate (250 Hz)
  data2 <- filter(data, Msec >= -10000L)
  sampling_rate <- data2$Msec - lag(data2$Msec)
  srates <- sampling_rate[!is.na(sampling_rate)] %>% unique()
  if (length(srates) != 1L) {
    stop(">1 sampling rate for RespID ", RespID, ": ",
	 paste(srates, collapse = ", "))
  }
  stopifnot(srates == 4L)

  ## align data to audio events
  dat <- data2 %>%
    arrange(Msec) %>%
    mutate(frame_adj = floor((Msec - AdjOn) / 4L),
	   frame_n = floor((Msec - NounOn) / 4L),
	   pad = FALSE) %>%
    filter(frame_adj >= win_begin,
	   frame_n <= win_end) %>%
    select(frame_adj, frame_n, Image, pad)

  fill_frames <- win_end - max(dat$frame_n)
  pad <- tibble(frame_adj = seq_len(fill_frames) + max(dat$frame_adj),
		frame_n = seq_len(fill_frames) + max(dat$frame_n),
		Image = rep(Clicked, fill_frames),
		pad = rep(TRUE, fill_frames))

  bind_rows(dat, pad)
}

sfiles <- read_rds("data-raw/soundfiles.rds")
resp <- read_rds("data-raw/resp.rds") %>%
  inner_join(sfiles %>% select(-SessionID, -ItemCellID), c("RespID"))
coding <- read_rds("data-raw/coding.rds") %>%
  select(-Speech, -Comment, -coder)

todo <- dir("data-raw/", "^F[0-9]{5}\\.rds$")
pog_list <- list()

for (i in seq_along(todo)) {
  pog_list[[i]] <- read_rds(file.path("data-raw", todo[i]))
}

pog <- bind_rows(pog_list)

timings <- resp %>%
  inner_join(coding, c("SessionID", "Soundfile")) %>%
  filter(!is.na(NounOn)) %>%
  select(RespID, AdjOn, NounOn, NounOff, click_lag, Clicked)

## throw away trials where mouse clicked before onset of adjective
bad_trials <- filter(timings, click_lag < AdjOn)

if (nrow(bad_trials)) {
  warning("mouse clicked before description onset for RespID(s):",
      paste(bad_trials$RespID, collapse = ", "), "\n")
}

pog_setup <- pog %>%
  nest(data = c(-RespID)) %>%
  inner_join(timings %>%
	       anti_join(bad_trials, "RespID"), "RespID")

pog_aligned <- pog_setup %>%
  mutate(aligned = pmap(., align_eyedata)) %>%
  select(RespID, aligned) %>%
  unnest(c(aligned))

saveRDS(pog_aligned, "data-derived/pog-aligned.rds")
