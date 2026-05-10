libraries <- c("knitr", "kableExtra", "tidyverse", "devtools", "roxygen2", "testthat")
invisible(lapply(libraries, library, character.only=TRUE))

#The aim of the functions in this script is to control and accelerate the process of consolidating and querying experimental data from instruments which quantify nucleic acids (NucAs), from within a Quarto template. These functions enable a user to download, wrangle, consolidate, and query data from the Nanodrop and Qubit 2.0 instruments. Since these instruments export data in a consistently named directory, the data can be detected on connected USB volumes and instrument provenance automatically attributed. Since the instruments also export data in a consistent format, a static data wrangling pipeline can be automatically applied. Processed data are then consolidated as observations in a 'tidy' (Wickham, JOSS, 2019) .rds file. To facilitate instantaneous querying of the NucA content of laboratory samples, data are queried with a sample type and a type-unique ID. All functions in this script share the common prefix 'NucAqnt', shorthand for 'nucleic acid quantitation'.

#----------Auto-detect instrument from connected USB volumes. The Qubit 2.0 always exports data in a folder named with the current year and month, regularly expressed (e.g. "2026 4"). Similarly, the Nanodrop always exports data in a folder name "NanodropOne_AZY1601167". Furthermore, data from these instruments are always pulled with the same, but separate, USB drives. Therefore, when called, this function can auto-detect DNA quantitation data and determine instrument provenance using static directories. ----------
NucAqnt.instrument_detect <- function() {
  qubit_folder <- file.path("/Volumes/USB DISK", paste(format(Sys.Date(), "%Y"), as.integer(format(Sys.Date(), "%m"))))
  nanodrop_folder <- "/Volumes/NO NAME/NanodropOne_AZY1601167"

  qubit_found <- dir.exists(qubit_folder)
  nanodrop_found <- dir.exists(nanodrop_folder)

  if (qubit_found && nanodrop_found) stop("Both Qubit and Nanodrop USBs detected — unplug one or pass readwd and instrument manually")
  if (!qubit_found && !nanodrop_found) stop("No instrument USB detected")

  if (qubit_found) {
    csvs <- list.files(qubit_folder, pattern = "\\.csv$", full.names = TRUE)
    if (length(csvs) != 1) stop("Expected 1 .csv in ", qubit_folder, ", found ", length(csvs))
    return(list(readwd = csvs, instrument = "qubit"))
  }

  csvs <- list.files(nanodrop_folder, pattern = "\\.csv$", full.names = TRUE)
  if (length(csvs) != 1) stop("Expected 1 .csv in ", nanodrop_folder, ", found ", length(csvs))
  list(readwd = csvs, instrument = "nanodrop")
}

#----------Download and wrangle new data.----------
NucAqnt.pull <- function(readwd = NULL, instrument = NULL, ID, ID.type=c("plasmid", "fragment", "gDNA", "RNA"), dilution.factor, cell.number=NULL, purification, elution.vol, elution.buffer) {
  if (is.null(readwd) || is.null(instrument)) {
    detected <- NucAqnt.instrument_detect()
    readwd <- readwd %||% detected$readwd
    instrument <- instrument %||% detected$instrument
  }
  
  #Data compatibility enforcement layer to ensure that only complete observations and correctly formatted observations are deposited.
  instrument <- match.arg(instrument, c("qubit", "nanodrop"))
  ID.type <- match.arg(ID.type)
  ID <- as.integer(ID)
  stopifnot(all(purification %in% c("RNeasy_plus_micro", "RNeasy_plus_mini", "miniprep qiagen", "miniprep NEB", "midiprep qiagen")))
  stopifnot(all(elution.buffer %in% c("water", "EB", "EY")))
  stopifnot(is.numeric(dilution.factor))
  stopifnot(is.numeric(elution.vol))
  if (!is.null(cell.number)) stopifnot(is.numeric(cell.number))
  if (ID.type %in% c("gDNA", "RNA") && is.null(cell.number))
    stop("cell.number is required for gDNA and RNA preparations")

  #Data transformation layer to compatibilise new data with .rds convention.
  if (instrument == "qubit") {
    new_data <- read.csv(readwd, fileEncoding = "latin1", colClasses = "character") |>
      filter(Date.Time != "", !if_any(everything(), ~grepl(">", .))) |>
      arrange(rev(row_number())) |>
      select(Date.Time, Stock.Conc., Units.1) |>
      rename(date.time = Date.Time, qubit.conc = Stock.Conc., qubit.conc.units = Units.1) |>
      mutate(qubit.conc = as.numeric(qubit.conc) * dilution.factor) |>
      mutate(sample.type = ID.type, .before = 1) |>
      mutate(sampleID = ID, .after = sample.type) |>
      mutate(dilution.factor = dilution.factor) |>
      mutate(cell.number = cell.number) |>
      mutate(purification = purification) |>
      mutate(elution.vol.uL = elution.vol) |>
      mutate(elution.buffer = elution.buffer) |>
      mutate(date.time = ymd_hms(date.time))
  } else if (instrument == "nanodrop") {
    new_data <- read.csv(readwd, fileEncoding = "UTF-16LE", sep="\t") |>
      select(Date, Nucleic.Acid.ng.uL., A260.A280, A260.A230) |>
      rename(date.time=Date, nanodrop.conc.ng.uL=Nucleic.Acid.ng.uL.) |>
      mutate(nanodrop.conc.ng.uL = nanodrop.conc.ng.uL * dilution.factor) |>
      mutate(sample.type = ID.type, .before = 1) |>
      mutate(sampleID = ID, .after = sample.type) |>
      mutate(dilution.factor = dilution.factor) |>
      mutate(cell.number = cell.number) |>
      mutate(purification = purification) |>
      mutate(elution.vol.uL = elution.vol) |>
      mutate(elution.buffer = elution.buffer) |>
      mutate(date.time = mdy_hms(date.time))
  }

  #Output is returned in a tabulated format for visual confirmation.
  new_data
}

#Usage (from .qmd)
#NucAqnt.pull(instrument = "qubit", ID.type = "plasmid", ID = rep(1:3, each = 2), dilution.factor = 10) |>
#  NucAqnt.update()

#----------Update or create a .rds file with new data. Deduplicates by date.time — safe to re-render .qmd without double-committing.----------
NucAqnt.update <- function(new_data, writewd = "data/NucAqnt.rds") {
  if (file.exists(writewd)) {
    existing <- readRDS(writewd)
    new_rows <- anti_join(new_data, existing, by = "date.time")
    if (nrow(new_rows) > 0) {
      saveRDS(bind_rows(existing, new_rows), file = writewd)
      }
  } else {
    saveRDS(new_data, file = writewd)
    }
  
  kable(new_data) |>
    kable_styling() |>
    column_spec(1:ncol(new_data), extra_css = "white-space: nowrap; padding: 0 10px !important;") |>
    row_spec(0, extra_css = "white-space: nowrap; padding: 0 10px !important;")
}

#----------Queries NucA quantification data by nucleic acid type and sample ID.----------
NucAqnt.query <- function(type = c("plasmid", "fragment", "gDNA", "RNA"), ID) {
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

#----------Summarises NucA quantification data for queried samples and renders this summary as a kable table — used for displaying tables in a .qmd----------
NucAqnt.summarise <- function(type = c("plasmid", "fragment", "gDNA", "RNA"), ID) {
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