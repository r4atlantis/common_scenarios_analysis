#' Read in a vector of scenario names from file names.
#'
#' @param dir A directory, either relative or full.
#' @param region A character value supplying the region name.
#' @param ignore A string of characters to ignore.
#'
read_scenarios <- function(dir, region,
  ignore = c("CC", "SB", "msy")) {

  dynTF <- grepl("Dyn", region, ignore.case = TRUE)
  if (dynTF) ignore <- union("DynEffort", ignore)

  scens <- unique(gsub("_Catch.txt|_BiomIndx.txt", "",
    dir(dir, pattern = paste0(region, "_"),
    ignore.case = TRUE)))

  scens <- gsub(paste0(region, "_"), "", scens)

  scens <- scens[!grepl(
    paste0("\\.|", paste(ignore, collapse = "|")),
    scens,
    ignore.case = TRUE)]

  if (length(scens) == 0) {
    stop("No scenarios were found.")
  }

  return(scens)
}
