#' @title Match two data bases based on names and dates of birth
#'
#'
#' @param query A data.table with the names to be matched
#' @param target A data.table with the names to be matched to
#'
#' @return A data.table with matches
#' @export
#' @import data.table
#' @importFrom matrixStats rowAnyNAs
#'
#'

perfect_match <- function(query, target = NULL) {
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

  cols <- c("full", "pn", "sn", "sn_i", "ap", "am")
  query <- query[,(cols) := lapply(.SD, as.character), .SDcols = cols]
  target <- target[,(cols) := lapply(.SD, as.character), .SDcols = cols]

  bys <- list("full",
              c("pn", "sn", "ap", "am"),
              c("pn", "sn_i", "ap", "am"),
              c("pn", "ap", "am"))
  n <- length(bys)

  pms <- vector("list", n)
  query_r <- reverse_date(query)
  pms_r <- vector("list", n)

  message("Encontrando pareos exactos.")
  pb <-  txtProgressBar(1, n, style = 3)

  cols <- c("id.x", "id.y", "reverse_dob", "pattern")

  for(i in 1:n){ # in for-loop because we change query and target
    setTxtProgressBar(pb, i)

    the_by <- bys[[i]]
    ind.x <- !matrixStats::rowAnyNAs(as.matrix(query[, ..the_by ]))
    ind.y <- !matrixStats::rowAnyNAs(as.matrix(target[, ..the_by ]))

    pms[[i]] <- merge(query[ind.x], target[ind.y], by = c(the_by, "dob"), all.x = TRUE)

    if(self_match) pms[[i]] <- pms[[i]][id.x<id.y]


    if(nrow(pms[[i]])>0){
      pms[[i]]$reverse_dob <- FALSE
      pms[[i]]$pattern <- paste(the_by, collapse = ":")
      if(identical(the_by, c("pn", "ap", "am"))){
        ##remove if name match but no middle name match or missing
        ind <- pms[[i]]$sn_i.x == pms[[i]]$sn_i.y |
          is.na(pms[[i]]$sn_i.x) |
          is.na(pms[[i]]$sn_i.y)
        pms[[i]] <- pms[[i]][ind]
      }
      pms[[i]] <- pms[[i]][, ..cols]
    }

    ind.x <- !matrixStats::rowAnyNAs(as.matrix(query_r[, ..the_by]))

    pms_r[[i]] <-  merge(query_r[ind.x], target[ind.y], by = c(the_by, "dob"), all.x = TRUE)

    if(self_match) pms_r[[i]] <- pms_r[[i]][id.x<id.y]

    if(nrow(pms_r[[i]])>0){
      pms_r[[i]]$reverse_dob <- TRUE
      pms_r[[i]]$pattern <- paste(the_by, collapse = ":")
      if(identical(the_by, c("pn", "ap", "am"))){
        ##remove if name match but no middle name match or missing
        ind <- pms_r[[i]]$sn_i.x == pms_r[[i]]$sn_i.y |
          is.na(pms_r[[i]]$sn_i.x) |
          is.na(pms_r[[i]]$sn_i.y)
        pms_r[[i]] <- pms_r[[i]][ind]
      }

      pms_r[[i]] <- pms_r[[i]][, ..cols]
    }
  }
  ret <- rbindlist(c(pms, pms_r))
  ret$pattern <- factor(ret$pattern, levels = sapply(bys, paste, collapse=":"))
  ret <- ret[order(pattern, reverse_dob)]
  ret <- unique(ret, by = c("id.x", "id.y"))

  return(ret)
}




