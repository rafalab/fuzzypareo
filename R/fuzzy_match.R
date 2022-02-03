#' Match two data bases based on names and dates of birth
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
#'
#' @return A data.table with matches
#' @export
#' @import data.table
#' @importFrom stats ARMAacf glm poly qnorm fitted.values

fuzzy_match <- function(query, target, cutoff = 0.2,
                        total.max = 8, full.max= 8, check.truncated = TRUE, truncate = "am",
                        min.n = 25) {
  ##min.n minium required to fit models
  query <- copy(query)
  target <- copy(target)
  message("Calculando frecuencias para los nombres.")
  freq <- compute_name_freqs(target)

  cols <- c("full", "pn", "sn", "sn_i", "ap", "am")
  query <- query[,(cols) := lapply(.SD, as.character), .SDcols = cols]
  target <- target[,(cols) := lapply(.SD, as.character), .SDcols = cols]

  by.xs <- list("full",
                c("pn", "sn", "ap", "am"),  ##next 3 are swaps
                c("pn", "sn_i", "ap", "am"),
                c("pn", "ap", "am"),
                c("pn","sn","ap"), ##next 4 names in wrong place
                c("pn","ap","am"),
                c("pn","sn"),
                c("pn","ap"))

  by.ys <- list("full",
                c("pn", "sn", "am", "ap"),
                c("pn", "sn_i", "am", "ap"),
                c("pn", "am", "ap"),
                c("pn","ap","am"),
                c("pn","sn","ap"),
                c("pn","ap"),
                c("pn","sn"))
  n <- length(by.xs)
  swap <- 1:n > 1
  lastname_swap <- c(1:n) %in% 2:4

  pms <- vector("list", n)
  query_r <- reverse_date(query)
  pms_r <- vector("list", n)

  message("Encontrando pareos exactos.")
  pb <-  txtProgressBar( 1, n, style = 3)

  ## We use these to remove perfect matches and stop paring them
  keep_query <- rep(TRUE, nrow(query))
  keep_query_r <- rep(TRUE, nrow(query_r))
  keep_target <- rep(TRUE, nrow(target))

  for(i in 1:n){ # in for-loop because we change query and target
    setTxtProgressBar(pb, i)

    if(lastname_swap[i]){

      ind <- as.character(query$ap) != as.character(query$am)
      ind[is.na(ind)] <- FALSE ##if last names same no point in swapping
      pms[[i]] <- perfect_match_engine(query[ind & keep_query], target[keep_target],
                                       by.x = by.xs[[i]], by.y = by.ys[[i]])
    } else{
      pms[[i]] <- perfect_match_engine(query[keep_query], target[keep_target],
                                       by.x = by.xs[[i]], by.y = by.ys[[i]])
    }

    is_full <- identical(by.xs[[i]], "full") & identical(by.ys[[i]],"full")
    if(!is.null(pms[[i]])){
      if(nrow(pms[[i]])>0){
        pms[[i]]$swap <- swap[i]
        pms[[i]]$reverse_dob <- FALSE
        if(is_full){
          keep_query[query$id %in% unique(pms[[i]]$id.x)] <- FALSE
          keep_target[target$id %in% unique(pms[[i]]$id.y)] <- FALSE
          pms[[i]]$full_match <- TRUE
        } else{
          pms[[i]]$full_match <- FALSE
        }
      } else{
        pms[[i]] <- NULL
      }
    }

    if(lastname_swap[i]){
      ind <- as.character(query_r$ap) != as.character(query_r$am)
      ind[is.na(ind)] <- FALSE
      pms_r[[i]] <- perfect_match_engine(query_r[ind & keep_query_r], target[keep_target],
                                         by.x = by.xs[[i]], by.y = by.ys[[i]])
    } else{
      pms_r[[i]] <- perfect_match_engine(query_r[keep_query_r], target[keep_target],
                                         by.x = by.xs[[i]], by.y = by.ys[[i]])
    }

    if(!is.null(pms_r[[i]])){
      if(nrow(pms_r[[i]])>0){
        pms_r[[i]]$swap <- swap[i]
        pms_r[[i]]$reverse_dob <- TRUE
        if(is_full){
          keep_query_r[query_r$id %in% unique(pms_r[[i]]$id.x)] <- FALSE
          keep_target[target$id %in% unique(pms_r[[i]]$id.y)] <- FALSE
          pms_r[[i]]$full_match <- TRUE
        } else{
          pms_r[[i]]$full_match <- FALSE
        }
      }
      else{
        pms_r[[i]] <- NULL
      }
    }
  }



  pms <- rbindlist(c(pms, pms_r))
  pms$match <- factor("perfect", levels=c("perfect", "fuzzy"))
  pms$truncated <- FALSE
  cols <-c("pn", "sn", "ap", "am")
  for(cn in cols){
    pms[[paste(cn, "freq", sep="_")]] <- freq[[cn]]$freq[ match(pms[[ cn ]], freq[[cn]]$name) ]
  }
  pms$genero_match <- pms$genero.x == pms$genero.y
  pms$lugar_match <- pms$lugar.x == pms$lugar.y
  pms$truncated <- FALSE
  message("\nEncontrando pareos con errores.")


  fms <- fuzzy_match_engine(query[keep_query], target[keep_target],  total.max=total.max, full.max=full.max)
  fms$reverse_dob <- FALSE

  message("\nEncontrando pareos con errores con mes y día invertido.")
  fms_r <- fuzzy_match_engine(query_r[keep_query_r], target[keep_target],  total.max=total.max, full.max=full.max)
  fms_r$reverse_dob <- TRUE

  fms <- rbindlist(list(fms, fms_r))

  fms[, total_dist := rowSums(.SD, na.rm=TRUE), .SDcols = patterns("(pn|sn|ap|am)_dist")]
  cols <- outer(c("full", "pn", "sn", "ap", "am"),
                c(name = "", x = ".x", y = ".y", dist = "_dist", nchar = "_nchar", i_match = "_i_match"), paste0)


  for(i in 1:nrow(cols)){

    nchar.x <- nchar(fms[[ cols[i, "x"] ]])
    nchar.y <- nchar(fms[[ cols[i, "y"] ]])

    ## Because second last name (need to define truncate = "am") is often truncated, we check if one is a substring of the other
    ## if if check.truncated is true we make the distance 0
    fms$truncated <- FALSE
    if(check.truncated){
      if(cols[i,"name"] %in% truncate){
        ind <- which(( substr(fms[[ cols[i, "x"] ]], 1, nchar.y) == substr(fms[[ cols[i, "y"] ]], 1, nchar.y) |
                         substr(fms[[ cols[i, "x"] ]], 1, nchar.x) == substr(fms[[ cols[i, "y"] ]], 1, nchar.x)) & nchar.x != nchar.y)
        cn <- cols[i, "dist"]
        fms[ind, (cn) := 0]
        fms[ind, truncated := TRUE]
      }
    }

    fms[[ cols[i, "nchar"] ]] <-  pmax(nchar.x, nchar.y, na.rm = TRUE)
    if(cols[i]!="sn"){
      fms[[ cols[i, "i_match"] ]] <- substr(fms[[ cols[i, "x"]]], 1, 1) == substr(fms[[ cols[i, "y"]]], 1, 1)
    } else{
      fms$sn_i_match <- fms$sn_i.x == fms$sn_i.y
    }

    if(cols[i]!="full"){
      cn <- cols[i, "name"]
      freq.x <- freq[[cn]]$freq[ match(fms[[ cols[i, "x"] ]], freq[[cn]]$name) ]
      freq.y <- freq[[cn]]$freq[ match(fms[[ cols[i, "y"] ]], freq[[cn]]$name) ]

      fms[[ paste(cn, "freq", sep="_") ]] <- pmax(freq.x, freq.y) #if either is NA don't use, probably misspelled name
    }
  }

  fms$match <- factor("fuzzy", levels=c("perfect", "fuzzy"))
  fms$genero_match <- fms$genero.x == fms$genero.y
  fms$lugar_match <- fms$lugar.x == fms$lugar.y
  fms$full_match <- FALSE

  cols <- c("id.x", "id.y",
            "full_dist", "pn_dist", "sn_dist", "ap_dist", "am_dist",
            "full_nchar", "pn_nchar", "sn_nchar", "ap_nchar", "am_nchar",
            "full_i_match",  "pn_i_match", "sn_i_match", "ap_i_match", "am_i_match",
            "pn_freq","sn_freq", "ap_freq", "am_freq",
            "genero_match", "lugar_match", "match", "full_match",
            "swap", "reverse_dob", "truncated")

  map <- rbindlist(list(pms[,..cols], fms[,..cols]))

  ################
  ################
  ################
  message("\nCalibrando el pareo.")
  map[, prop_match:=as.numeric(NA)]
  map[, score:=as.numeric(NA)]
  map[, pattern:=as.character(NA)]
  map[, full_prop_match := 1-full_dist/full_nchar]
  map$pn_ap_match <- 1-rowSums(map[,c("pn_dist", "ap_dist")],na.rm=TRUE)/
    rowSums(map[,c("pn_nchar", "ap_nchar")],na.rm=TRUE)

  ## keep if 90% match with full name
  map[full_prop_match >= 0.9, `:=`(prop_match = full_prop_match,
                                   score = mean(lugar_match, na.rm=TRUE),
                                   pattern = "full")]

  patterns <- list(c("pn", "sn", "ap", "am"),
                   c("pn", "sn_i", "ap", "am"),
                   c("pn", "ap", "am"),
                   c("pn", "sn", "ap"),
                   c("pn", "sn_i", "ap"),
                   c("pn", "ap"),
                   c("ap", "am"))

  ## what name frequencies to include in model
  ## decided usign EDA
  freqs <- list(c("ap","am"),
                c("ap", "am"),
                c("pn", "ap", "am"),
                c("ap"),
                c("pn", "ap"),
                c("pn", "ap"),
                c("ap","am"))

  ## for names with missing frequencies impute the median
  freq_impute <- function(x){
    x[is.na(x)] <- median(x, na.rm=TRUE)
    return(x)
  }
  cols <- paste(c("pn", "sn", "ap", "am"), "freq", sep="_")
  map[, (cols) := lapply(.SD, freq_impute), .SDcols = cols]

  for(i in seq_along(patterns)){
    p <- patterns[[i]]
    dist_cols <- paste(p, "dist", sep="_")
    dist_cols[p=="sn_i"] <- "sn_i_match"
    nchar_cols <- paste(p[p!="sn_i"], "nchar", sep="_")
    if(!is.null(freqs[[i]])){
      freq_cols <- paste(freqs[[i]], "freq", sep="_")
    } else freq_cols<- NULL

    ind <- !matrixStats::rowAnyNAs(as.matrix(map[, ..dist_cols])) &
      is.na(map[,score]) ## needed columns not NA and not yet scored

    map$prop_match[ind] <- 1 - rowSums(map[ind,..dist_cols])/
      (rowSums(map[ind,..nchar_cols]) + "sn_i"%in%p)

    the_formula <- paste("lugar_match ~ pmax(prop_match,0.6)")
    if(!is.null(freq_cols)){
      the_formula <- paste(the_formula, "+",
                           paste0("log(pmax(", freq_cols, ",10^-4))", collapse = " + "))
    }

    the_formula <- formula(the_formula)

    if(sum(ind) >= min.n){
      fit <- glm(the_formula, family = "binomial", data = map[ind & !is.na(lugar_match)])
      map[ind, score := predict(fit, newdata = map[ind], type = "response")]
    } else{ ## if not enough to fit model, just compute the proportion of matches
      map[ind,  score := mean(lugar_match, na.rm=TRUE)]
    }
    map[ind, pattern := paste(p, collapse=":")]
  }


  map[, score := (score - min(score, na.rm=TRUE)) / max(score - min(score,na.rm=TRUE),na.rm = TRUE)]
  map <- map[!is.na(score)]

  ## Pick the best score for each id.x
  map <- map[map[,.I[score==max(score, na.rm = TRUE)], by = id.x]$V1]
  setnames(map, "match", "match_type")
  map <- merge(merge(map, target, by.x = "id.y", by.y = "id", all.x = TRUE), query, by.x = "id.x", by.y = "id", all.x=TRUE)
  cols <- c("id.x", "id.y", "original.x", "original.y","score",
            "prop_match", "full_prop_match", "pn_ap_match", "lugar_match", "genero_match", "pattern", "match_type", "full_match", "swap", "reverse_dob", "truncated")
  return(map[score>=cutoff, ..cols])
}



