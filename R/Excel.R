# Get sheet names (if it fails then force xlsx) (supress prints)
.xlsheets <- function(file) {
  capture.output({
    res <- tryCatch({
      readxl::excel_sheets(file)
    }, error=function(e) {
      # Probably just wrong file extension, try xlsx
      tryCatch(readxl:::xlsx_sheets(file), error=function(e) {
        warning("Unable to read file, please check the extension is correct (e.g. xlsx not xls)")
        NULL
      })
    })
  })
  return(res)
}

# Read (if it fails then force xlsx) (supress prints)
.xldata <- function(file, sheet, ...) {
  capture.output({
    res <- tryCatch(readxl::read_excel(file, sheet, ...), error=function(e) readxl::read_xlsx(file, sheet, ...))
  })
  return(res)
}
