.cycler_types <- function() {
  c("arbin","biologic","land","ivium",'maccor','internal')
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

.cycler_normalise <- function(cycler_type) {
  if(is.na(cycler_type)||length(cycler_type)==0) return(NA)
  cycler_type=tolower(cycler_type)
  cycler_type=gsub("[[:space:]]", "", cycler_type)
  cycler_types=.cycler_types()
  if(!cycler_type %in% cycler_types) {
    return(NA)
  }
  return(cycler_type)
}
