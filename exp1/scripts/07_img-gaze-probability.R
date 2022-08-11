suppressPackageStartupMessages(library("dplyr"))
library("ggplot2")

dat <- readRDS("data-derived/gaze-plot-full.rds")

col_scale <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7")

g <- ggplot(dat %>% filter(Image != "Blank"),
       aes(bin, p, color = Image, fill = Image)) +
  geom_ribbon(aes(ymin = p_min, ymax = p_max),
	      color = NA, alpha = .2) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_point(aes(shape = Image, size = Image)) +
  geom_line(alpha = .25) +
  facet_wrap(~ Speaker, labeller = label_both) +
  scale_x_continuous(limits = c(-560, 1050)) +
  scale_color_manual(values = col_scale) +
  scale_fill_manual(values = col_scale) +
  scale_shape_manual(values = c(0L, 0L, 1L, 2L)) +
  scale_size_manual(values = c(1.2, 2.5, 1.2, 2.5)) +
  labs(x = "time from noun onset (ms)",
       y = "gaze probability") +
  geom_text(aes(label = lab),
	    data = tibble(Speaker = rep("early", 2),
			  bin = c(-560L, 0L),
			  Image = rep("Target", 2),
			  p = rep(.9, 2), lab = c("\"s m a l l", "A\"")),
	    color = "black", hjust = 0, size = 6, fontface = "italic",
	    family = "serif") +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
	 color = guide_legend(override.aes = list(linetype = 0))) +
  theme_bw(base_family = "serif") +
  theme(panel.border = element_rect(fill = NA, color = 'black', size = .25),
	legend.position = "top", legend.title = element_blank())

ggsave("img/gaze-probability.pdf", g, width = 8, height = 3.5)
