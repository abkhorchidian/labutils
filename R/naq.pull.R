#----------Download and wrangle new data.----------
naq.pull <- function(readwd = NULL, instrument = NULL, ID, ID.type=c("plasmid", "fragment", "gDNA", "RNA"), dilution.factor, cell.number=NULL, purification, elution.vol, elution.buffer) {
  if (is.null(readwd) || is.null(instrument)) {
    detected <- NucAqnt.instrument_detect()
    readwd <- readwd %||% detected$readwd
    instrument <- instrument %||% detected$instrument
  }

  #Data compatibility enforcement layer to ensure that only complete and correctly formatted observations are deposited.
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
