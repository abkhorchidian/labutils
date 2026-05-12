#----------Queries NucA quantification data by nucleic acid type and sample ID.----------
naq.query <- function(type = c("plasmid", "fragment", "gDNA", "RNA"), ID) {
  type <- match.arg(type)
  readRDS(here::here("! nuc_acid_quant.rds")) |>
    filter(sample.type == type, sampleID %in% ID) |>
    group_by(sample.type, sampleID) |>
    summarise(
      n_qubit = sum(!is.na(qubit.conc)),
      n_nanodrop = sum(!is.na(nanodrop.conc.ng.uL)),
      qubit_mean = mean(qubit.conc, na.rm = TRUE),
      qubit_sd = round(sd(qubit.conc, na.rm = TRUE), 0),
      qubit_conc_units = first(na.omit(qubit.conc.units)),
      qubit_cv = round((qubit_sd / qubit_mean) * 100, 0),
      nanodrop_mean =  mean(nanodrop.conc.ng.uL, na.rm = TRUE),
      A260_A280_mean = round(mean(A260.A280, na.rm = TRUE), 2),
      A260_A280_sd = round(sd(A260.A280, na.rm = TRUE), 2),
      A260_A280_cv = round((A260_A280_sd / A260_A280_mean) * 100, 2),
      A260_A230_mean = round(mean(A260.A230, na.rm = TRUE), 2),
      A260_A230_sd = round(sd(A260.A230, na.rm = TRUE), 2),
      A260_A230_cv = round((A260_A230_sd / A260_A230_mean) * 100, 2),
      .groups = "drop"
    )
}

#Usage
#returns data frame — use for extracting values: NucAqnt.query('plasmid', 2)$qubit_mean
