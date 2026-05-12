#----------Update or create a .rds file with new data. Deduplicates by date.time — safe to re-render .qmd without double-committing.----------
naq.update <- function(new_data, writewd = "data/NucAqnt.rds") {
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
