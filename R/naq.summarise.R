#----------Summarises NucA quantification data for queried samples and renders this summary as a kable table — used for displaying tables in a .qmd----------
naq.summarise <- function(type = c("plasmid", "fragment", "gDNA", "RNA"), ID) {
  result <- NucAqnt.query(type, ID) |>
    transmute(
      sampleID,
      n_qubit,
      `Qubit conc` = sprintf("%.0f \u00b1 %.0f (%.0f%%)", qubit_mean, qubit_sd, qubit_cv),
      qubit_conc_units,
      n_nanodrop,
      A260_A280 = sprintf("%.2f \u00b1 %.2f (%.2f%%)", A260_A280_mean, A260_A280_sd, A260_A280_cv),
      A260_A230 = sprintf("%.2f \u00b1 %.2f (%.2f%%)", A260_A230_mean, A260_A230_sd, A260_A230_cv)
    )
  kable(result) |>
    kable_styling() |>
    column_spec(1:ncol(result), extra_css = "white-space: nowrap; padding: 0 10px !important;") |>
    row_spec(0, extra_css = "white-space: nowrap; padding: 0 10px !important;")
}

#Usage
#query plasmid sample IDs #1 through 6: NucAqnt.summarise("plasmid", 1:6)
#query RNA sample IDs #1 and 3: NucAqnt.summarise("RNA", c(1, 3))
