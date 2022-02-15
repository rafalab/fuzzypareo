#' @title Match two data bases based on names and dates of birth permitting differences
#'
#'
#' @param query A data.table with the names to be matched
#' @param target A data.table with the names to be matched to
#' @param cutoff Remove any match with score below this number
#' @param total.max Maximum Number of erros to permit to be considered
#' @param full.max Maximum number of errors permited by full match
#' @param check.truncated Checks if second last name is truncated. If truncated matches only remaining part.
#' @param truncate The columns to truncate. Defaults to second name `am`
#' @param min.n Minimum number of data points needed to fit calibration model
#' @param keep.all.matches if `TRUE` the function returns a list with the final match and a table with all the matches that were considered.
#'
#' @return A data.table with matches
#' @import data.table
#' @importFrom matrixStats rowAnyNAs
#'
#'

#' @export
fuzzy_match <- function(query, target = NULL, cutoff = 0, total.max = 8, full.max= 8,
                        check.truncated = TRUE, truncate = "am", keep.all.matches = FALSE) {

  query <- copy(query)

  ##min.n minium required to fit models
  if(is.null(target)){
    target <- copy(query)
    self_match <- TRUE
  } else{
    target <- copy(target)
    self_match <- FALSE
  }

  req_col_names <- c("id", "original", "full", "pn", "sn", "sn_i", "ap", "am", "genero", "lugar", "dob")
  if(!all(req_col_names %in% names(query)) |
     !all(req_col_names %in% names(target))){
    stop(paste("query and target must include columns named:",
               paste(req_col_names, collapse =", "), "as returned by wrangle_table."))
  }

  if(any(duplicated(query$id)) | any(duplicated(target$id))) stop("id column must be unique in both query and target")

  if(nrow(query) == 0 | nrow(target) ==0) stop("query and target must have 1 or more rows")

  all_matches <- get_all_matches(query, target, total.max = total.max, full.max = full.max,
                                 check.truncated = check.truncated,
                                 truncate = truncate, self.match = self_match)

  if(is.null(all_matches)) return(NULL)

  map <- cleanup_matches(calibrate_matches(all_matches), query, target, self.match = self_match, cutoff = cutoff)

  if(keep.all.matches) return(list(map = map, all_matches=all_matches)) else return(map)

}

