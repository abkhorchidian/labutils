#____________PLATE FORMATS____________
plate_6   <- list(n_rows = 2,  n_cols = 3,  wrap_width = 24)
plate_12  <- list(n_rows = 3,  n_cols = 4,  wrap_width = 24)
plate_24  <- list(n_rows = 4,  n_cols = 6,  wrap_width = 6)
plate_48  <- list(n_rows = 6,  n_cols = 8,  wrap_width = 5)
plate_96  <- list(n_rows = 8,  n_cols = 12, wrap_width = 4)
plate_384 <- list(n_rows = 16, n_cols = 24, wrap_width = 3)

#____________PLATE PLOTS____________
#Renders a plate map. mastermix and sample are vectors ordered left-to-right, top-to-bottom (A1, A2, A3, ... B1, B2, B3, ...).
#Requires ggforce. Well size scales proportionally with the plot — no manual point_size tuning needed.
#Set composite = TRUE when combining plates with patchwork.
plotplate <- function(plate.name, format = plate_6, mastermix, sample, composite = FALSE) {
  plate <- expand.grid(columns = 1:format$n_cols, rows = 1:format$n_rows) |>
    mutate(mastermix = mastermix, sample = sample)
  p <- ggplot(plate, aes(x = columns, y = rows)) +
    ggforce::geom_circle(aes(x0 = columns, y0 = rows, r = 0.48, fill = mastermix), color = "black") +
    geom_text(aes(label = str_wrap(sample, format$wrap_width)), size = 8 / format$n_cols, lineheight = 0.8) +
    scale_y_reverse(breaks = 1:format$n_rows, labels = LETTERS[1:format$n_rows], expand = expansion(add = 0.05)) +
    scale_x_continuous(breaks = 1:format$n_cols, position = "top", expand = expansion(add = 0.05)) +
    scale_fill_brewer(palette = "Set2") +
    theme_bw(base_size=12) +
    theme(panel.grid=element_blank(),
          legend.text=element_text(size=9),
          legend.title=element_text(size=11),
          axis.text = element_text(size = 11),
          plot.margin = margin(0, 0, 0, 0)) +
    guides(fill = guide_legend(override.aes = list(size = 4))) +
    labs(fill = "Master mix", x=NULL, y=NULL, title = plate.name)
  if (composite) {
    p
  } else {
    p + coord_fixed(ratio = 1)
  }
}

#Usage
#plotplate(format = plate_6,
#          mastermix = c("E6", "E6", "E6", NA, NA, NA),
#          sample = c("line_1", "line_2", "line_3", NA, NA, NA))
