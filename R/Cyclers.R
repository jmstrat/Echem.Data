#' List of supported cyclers and their file extensions
#'
#' @export
#' @rdname cycler_types
cycler_types <- function() {
  list(arbin=c('xlsx','xls'),biologic=c('mpr','mpt'),land=c('xlsx','xls','txt'),ivium=c('idf','txt'),maccor=c('txt'),internal=c('csv'))
}
#' @export
#' @rdname cycler_types
supported_file_extensions <- function() unique(unlist(cycler_types()))


cycler_loader_functions <- function() {
  c(arbin=load.arbin,biologic=load.biologic,land=load.land, ivium=load.ivium, maccor=load.maccor, internal=load.internal)
}

loader_for_cycler <- function(cycler) {
  loader = cycler_loader_functions()[[cycler]]
  if(is.null(loader)) stop('Cannot get loader for "', cycler, '"')
  return(loader)
}

#' Guess cycler for file
#'
#' A very basic function to choose a cycler based on the file extension, implementation may change if more cyclers are added
#'
#' @param file Path to data file
#' @return The cycler type
guess_cycler <- function(file) {
  ext= regexpr("\\.([[:alnum:]]+)$", file)
  ext=ifelse(ext > -1L, substring(file, ext + 1L), "")

  if(ext=='mpt' || ext=='mpr') {
    return('biologic')
  } else if(ext=='xlsx' || ext == 'xls') {
    return('arbin')
  } else if(ext=='idf') {
    return('ivium')
  } else if(ext=='csv') {
    return('internal')
  } else if (ext=='txt') {
    #Could be land or ivium, so read the 1st line
    header_text=readLines(file,n=1)
    header=read.table(sep="\t",header=T,text=header_text,quote="")
    if(identical(names(header),c("time..s","I..mA","E..V")))
      return('ivium')
    else if(names(header)[[1]]=='Today.s.Date.')
      return('maccor')
    return('land')
  }
  return(NA)
}

#' Normalise a string to represent a cycler name
#'
#' @param cycler_type The string to normaliase
#' @return The normalised string
#' @export
cycler_normalise <- function(cycler_type) {
  if(is.na(cycler_type)||length(cycler_type)==0) return(NA)
  cycler_type=tolower(cycler_type)
  cycler_type=gsub("[[:space:]]", "", cycler_type)
  cycler_types=names(cycler_types())
  if(!cycler_type %in% cycler_types) {
    return(NA)
  }
  return(cycler_type)
}
