#The aim of the functions in this script is to parse, transform, and display from the .txt located in the 'summary-files' directory of Plasmidsaurus (for the 'whole plasmid sequencing' service) sequencing results.

#----------Parsing the Plasmidsaurus order .txt 'summary-files'.----------
plasmidsaurus.parse <- function(filepath, ID) {
  map2_dfr(filepath, ID, function(fp, id) {
    lines <- readLines(fp)
    nmers <- str_extract_all(lines[1], "\\d+-mer")[[1]]
    tibble(
      sample = id, nmer = nmers,
      moles_pct = as.numeric(str_extract_all(lines[2], "[\\d.]+")[[1]]),
      mass_pct = as.numeric(str_extract_all(lines[3], "[\\d.]+")[[1]]),
      ecoli_contam_pct = as.numeric(str_extract(lines[str_detect(lines, "E\\. coli")], "\\d+\\.\\d+"))
    )
  })
}

#----------Transforms the tibble produced by plasmidsaurus.parse() into a wide-format kable table with grouped column headers per n-mer (for detected concatemers) and E. coli gDNA contamination. ----------
plasmidsaurus.display <- function(summary.tibble) {
  wide <- summary.tibble |>
    pivot_wider(names_from = nmer, values_from = c(moles_pct, mass_pct),
                values_fill = 0, names_glue = "{nmer}_{.value}")
  mer_cols <- str_sort(str_subset(names(wide), "mer"), numeric = TRUE)
  wide <- select(wide, sample, all_of(mer_cols), ecoli_contam_pct)
  nmers <- paste("%", unique(str_extract(mer_cols, "\\d+-mer")))
  
  kable(wide, col.names = str_replace_all(names(wide), c(".*moles.*" = "of moles", ".*mass.*" = "of mass", "ecoli_contam_pct" = "% gDNA"))) |>
    kable_styling() |>
    column_spec(1:ncol(wide), extra_css = "white-space: nowrap; padding: 0 10px !important;") |>
    row_spec(0, extra_css = "white-space: nowrap; padding: 0 10px !important;") |>
    add_header_above(c(" " = 1, setNames(rep(2, length(nmers)), nmers), " " = 1))
}

#Usage
#path <- "data/sequences/plasmids/plasmid_1-3/plasmidsaurus/R8QFHR_summary-files/"
#plasmidsaurus.parse(filepath = file.path(path, c("R8QFHR_1_1.txt", "R8QFHR_2_2.txt", "R8QFHR_3_3.txt")), ID = 1:3) |>
#  plasmidsaurus.summarise()