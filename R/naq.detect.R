#' Move raw data from an 'inbox' directory to a repository
#'
#' @returns
#' @export
#'
#' @examples
naq.detect <- function() {
  qubit_folder <- file.path("/Volumes/USB DISK", paste(format(Sys.Date(), "%Y"), as.integer(format(Sys.Date(), "%m"))))
  nanodrop_folder <- "/Volumes/NO NAME/NanodropOne_AZY1601167"

  qubit_found <- dir.exists(qubit_folder)
  nanodrop_found <- dir.exists(nanodrop_folder)
  #' @returns If multiple USB volumes are detected, throws an error message.
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
