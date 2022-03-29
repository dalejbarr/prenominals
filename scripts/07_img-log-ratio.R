options(crayon.enabled = FALSE, tidyverse.quiet = TRUE)
suppressPackageStartupMessages(library("dplyr"))
library("ggplot2")

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
	       "#0072B2", "#D55E00", "#CC79A7")

pog_bin <- readRDS("data-derived/pog-lratio-binned.rds")

errbar <- data_frame(x = 124L, xmin = 320L, xmax = 568L,
		     y = .6)

g <- pog_bin %>%
  mutate(condition = paste0("speaker ", tolower(Speaker),
                            "/listener ", tolower(Listener))) %>%
  ggplot(aes(bin, lratio, color = condition)) +
  geom_ribbon(aes(fill = condition, ymin = lmin, ymax = lmax), alpha = .2,
	      color = NA) +
  geom_line() +
  geom_point(aes(shape = condition), size = 2) +
  geom_errorbarh(aes(y = y, xmin = xmin, xmax = xmax), data = errbar,
		 height = .07, inherit.aes = FALSE) +
  geom_text(x = 444, y = .68, label = "Speaker x Listener",
	    inherit.aes = FALSE, size = 3, family = "serif",
	    fontface = "plain") +
  scale_y_continuous(limits = c(-1.5, 1)) +
  labs(x = "time from adjective onset (ms)",
       y = "log (target / competitor)") +
  theme_bw(base_family = "serif") +
  scale_fill_manual(values = cbPalette) +
  scale_color_manual(values = cbPalette) +
  theme(legend.position = "top", legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2),
         shape = guide_legend(nrow = 2))

ggsave("img/log-ratio.pdf",
       plot = g, width = 8, height = 4)
