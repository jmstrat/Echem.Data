#' Read Echem Data
#'
#' This function is used to import echem data.
#' @param echem_file Path to data file
#' @param cycler_type Name of the cycler
#' @return An echem.data.object containing the echem data
#' @export
#' @examples
#' read.echem("/path/to/file.mpr")
#' read.echem("/path/to/file.mpt")
#' read.echem("/path/to/file.xlsx")
#' read.echem("/path/to/file.txt")
#' read.echem("/path/to/file.idf")
read.echem <-function(echem_file,cycler_type=NA) {
  if(is.na(cycler_type)) {
    cycler_type=guess_cycler(echem_file)
  }
  return(.echem_data_cache(echem_file,cycler_type))
}

.read.echem <-function(echem_file,cycler_type)
{
  if(is.null(echem_file)|is.na(echem_file)|echem_file=='')
    return(NA)

  if(!file.exists(echem_file)){
    #file doesn't exist
    stop("Unable to find file to load -- is the path typed correctly?",call. = FALSE)
  }
  cycler_types=.cycler_types()
  cycler_type=.cycler_normalise(cycler_type)

  loader_functions=c(load.arbin,load.biologic,load.land, load.ivium, load.maccor, load.internal)

  if(is.na(cycler_type)) stop(paste0("Unknown cycler type! Known cyclers are: ",paste0(cycler_types,collapse=', ')),call. = FALSE)

  func=loader_functions[[match(cycler_type,cycler_types)]]
  echem=func(echem_file)

  if(all(is.null(echem))) return(NA)
  if(cycler_type=='internal') return(echem) #Attributes already added
  #Add attributes:
  attr(echem,'filepath')<-echem_file
  attr(echem,'filename')<-basename(echem_file)
  attr(echem,'cycler')<-cycler_type

  return(make.echem.data.object(echem))
}