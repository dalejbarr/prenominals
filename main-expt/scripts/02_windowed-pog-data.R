options(crayon.enabled = FALSE, tidyverse.quiet = TRUE)
suppressPackageStartupMessages(library("dplyr"))
library("tidyr")

bad_sessions <- readRDS("data-raw/sess-exclude.rds")
resp <- readRDS("data-raw/resp.rds") %>%
  anti_join(bad_sessions, "SessionID") %>%
  filter(ImgChanged %in% c("Competitor", "Competitor Contrast"),
    Clicked == "Target")

pog <- readRDS("data-derived/pog-aligned.rds") %>%
  semi_join(resp, "RespID") %>%
  filter(frame_adj >= -100, frame_n <= 50)

pog_n <- pog %>%
  count(frame_adj)

## end window when > 20% of trials have ended
count_cutoff <- pog_n %>%
  `[[`("n") %>%
  max() %>% `*`(.8) %>% ceiling()

final_frame <- pog_n %>%
  filter(n >= count_cutoff) %>%
  `[[`("frame_adj") %>%
  max()

pog_full <- pog %>%
  filter(frame_adj <= final_frame) %>%
  inner_join(resp %>% select(RespID, SessionID, ItemID, Speaker, Listener),
             "RespID") %>%
  select(SessionID, ItemID, RespID, Speaker, Listener,
         frame_adj, frame_n, Image, pad) %>%
  mutate(Image = ifelse(is.na(Image), "Blank", Image) %>%
           factor(levels = c("Competitor", "Target", "Target Contrast",
                    "Competitor Contrast", "Blank"))) %>%
  arrange(SessionID, RespID, frame_adj)

pog_subj <- pog_full %>%
  count(SessionID, Speaker, Listener, frame_adj, Image) %>%
  rename(Y = n) %>%
  complete(frame_adj, Image, nesting(SessionID, Speaker, Listener),
           fill = list(Y = 0)) %>%
  group_by(SessionID, Speaker, Listener, frame_adj) %>%
  mutate(N = sum(Y)) %>% ungroup()

pog_item <- pog_full %>%
  count(ItemID, Speaker, Listener, frame_adj, Image) %>%
  rename(Y = n) %>%
  complete(frame_adj, Image, nesting(ItemID, Speaker, Listener),
           fill = list(Y = 0)) %>%
  group_by(ItemID, Speaker, Listener, frame_adj) %>%
  mutate(N = sum(Y)) %>% ungroup()

saveRDS(pog_subj, "data-derived/pog-window-subj.rds")
saveRDS(pog_item, "data-derived/pog-window-item.rds")
