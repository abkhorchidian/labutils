#____________PLATE FORMATS____________
plate_6   <- list(n_rows = 2,  n_cols = 3,  point_size = 18, text_size = 5)
plate_12  <- list(n_rows = 3,  n_cols = 4,  point_size = 14, text_size = 4)
plate_24  <- list(n_rows = 4,  n_cols = 6,  point_size = 12, text_size = 3.5)
plate_48  <- list(n_rows = 6,  n_cols = 8,  point_size = 10, text_size = 3)
plate_96  <- list(n_rows = 8,  n_cols = 12, point_size = 10, text_size = 3)
plate_384 <- list(n_rows = 16, n_cols = 24, point_size = 5,  text_size = 1.5)

#____________PLATE PLOTS____________
plotplate <- function(platename, format, columns="columns", rows="rows", mastermix="mastermix", sample_ID="sample_ID", fill_label="Target") {
  ggplot(platename, aes(x = columns, y = rows)) +
    geom_point(aes(fill=.data[[mastermix]]), shape = 21, size = format$point_size) +
    geom_text(aes(label=.data[[sample_ID]]), size = format$text_size) +
    scale_y_reverse(breaks = 1:format$n_rows, labels = LETTERS[1:format$n_rows]) +
    scale_x_continuous(breaks = 1:format$n_cols, position = "top") +
    scale_fill_brewer(palette = "Set2") +
    theme_bw(base_size=20) +
    theme(panel.grid=element_blank(),
          legend.text=element_text(size=9),
          legend.title=element_text(size=11),
          axis.text = element_text(size = 11),
          plot.margin = margin(0, 0, 0, 0)) +
    guides(fill = guide_legend(override.aes = list(size = 4))) +
    labs(fill = fill_label, x=NULL, y=NULL)
}

#Usage
#plotplate(tc_plate1, format = plate_6)
#plotplate(qpcr_plate1, format = plate_96)
#plotplate(qpcr_plate2, format = plate_384, fill_label = "Condition")
