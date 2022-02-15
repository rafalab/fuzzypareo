#' Helper functions for fuzzy match
#'
#' @import data.table
#' @import stringr
#' @importFrom stringdist stringdist stringdistmatrix
#' @importFrom matrixStats rowAnyNAs


fix_names <- function(x){
  levels(x) <-
    str_replace_all(levels(x), c("á" = "a", "é" = "e", "í"= "i", "ó" = "o", "ú" = "u", "ü" = "u", "ñ" = "n",
                                 "ph" = "f", "nn"="n", "tt"="t", "ss"="s")) |>
    str_remove_all("^dr\\s+") |>
    str_remove_all("^lcdo\\s+") |>
    str_remove_all("^sor\\s+") |>
    str_replace_all("(?!^)\\s*-\\s*(?!$)", " ") |>
    str_replace_all("\\s+(jr|junior|ii|iii)(\\s+|$)", "\\2") |>
    str_remove_all("[^a-z\\s]+")

  x <- forcats::fct_recode(x, NULL = "")
  return(x)
}
join_dela <- function(x){
  levels(x) <- str_replace_all(levels(x), "^(y|de|la|las|los|del|lo|di|da|le|st|mc|mac|van|san|saint|dos|el|d|o)(\\s+|$)", "\\1") |>
    str_replace_all("\\s+(y|de|la|las|los|del|lo|di|da|le|st|mc|mac|van|san|saint|dos|el|d|o)(\\s+|$)", " \\1") |>
    str_replace_all("^(dela|delas|delos|delo)(\\s+|$)", "\\1") |>
    str_replace_all("\\s+(dela|delas|delos|delo)(\\s+|$)", " \\1")
  return(x)
}

fct_trim <- function(x){
  levels(x) <- str_trim(levels(x))
  return(x)
}
fct_to_lower <- function(x){
  levels(x) <- str_to_lower(levels(x))
  return(x)
}

fct_to_title <- function(x){
  levels(x) <- str_to_title(levels(x))
  return(x)
}
fct_nchar <- function(x){
  nc<- nchar(levels(x))
  nc[as.numeric(x)]
}


compute_name_freqs <- function(tab){ ## a name must appear at least min times to be considered not a mistake
  pn_freq <- tab[!is.na(pn), .N, by = pn][, freq := as.numeric(N)/as.numeric(sum(N))][,!"N"]
  setnames(pn_freq, "pn", "name")
  pn_freq <- pn_freq[order(freq, decreasing = TRUE)]

  sn_freq <- tab[!is.na(sn), .N, by = sn][, freq := as.numeric(N)/as.numeric(sum(N))][!is.na(sn),!"N"]
  setnames(sn_freq, "sn", "name")
  sn_freq <- sn_freq[order(freq, decreasing = TRUE)]

  a_freq <- tab[!is.na(ap), .N, by = ap][, freq := as.numeric(N)/as.numeric(sum(N))][!is.na(ap),!"N"]
  setnames(a_freq, "ap", "name")
  a_freq <- a_freq[order(freq, decreasing = TRUE)]

  return(list(pn = pn_freq, sn = sn_freq, ap = a_freq, am = a_freq))
}

reverse_date <- function(tab){
  tab <- copy(tab)
  tab[, reverse_dob := as.Date(NA)]
  tab[mday(dob)<=12 & mday(dob)!=month(dob),
      reverse_dob :=
      apply(cbind(year(dob),  mday(dob), month(dob)),1,
           function(x) as.Date(paste(x,collapse="-")))]
  tab[, dob:=reverse_dob]
  tab <- tab[, !"reverse_dob"]
  tab[!is.na(dob),]
}

get_all_matches <-  function(query, target, total.max = 8, full.max= 8,
                             check.truncated = TRUE, truncate = "am", self.match = FALSE) {
  message("Calculando frecuencias de los nombres.")
  freq <- compute_name_freqs(target)

  query <- copy(query)
  target <- copy(target)

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
      if(nrow(pms[[i]])>0 & self.match){ ## remove repeated comparisons
        pms[[i]] <- pms[[i]][id.x!=id.y]
        pms[[i]][, pair := fifelse(id.x < id.y, paste(id.x, id.y, sep=":"), paste(id.y, id.x, sep=":"))]
        pms[[i]] <- unique(pms[[i]], by="pair")
        pms[[i]][, pair:= NULL]
      }
      if(nrow(pms[[i]])>0){
        pms[[i]]$swap <- swap[i]
        pms[[i]]$reverse_dob <- FALSE
        if(is_full & !self.match){
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
      if(nrow(pms_r[[i]])>0 & self.match){
        pms_r[[i]][, pair := fifelse(id.x < id.y, paste(id.x, id.y, sep=":"), paste(id.y, id.x, sep=":"))]
        pms_r[[i]] <- unique(pms_r[[i]], by="pair")
        pms_r[[i]][, pair:= NULL]
      }
      if(nrow(pms_r[[i]])>0){
        pms_r[[i]]$swap <- swap[i]
        pms_r[[i]]$reverse_dob <- TRUE
        if(is_full & !self.match){
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

  if(!is.null(pms)){
    if(nrow(pms)>0){
      pms$match <- factor("perfect", levels=c("perfect", "fuzzy"))
      pms$truncated <- FALSE
      cols <-c("pn", "sn", "ap", "am")
      for(cn in cols){
        pms[[paste(cn, "freq", sep="_")]] <- freq[[cn]]$freq[ match(pms[[ cn ]], freq[[cn]]$name) ]
      }
      pms$genero_match <- pms$genero.x == pms$genero.y
      pms$lugar_match <- pms$lugar.x == pms$lugar.y
      pms$truncated <- FALSE
    } else pms <- NULL
  }
  message("\nEncontrando pareos con errores.")

  fms <- fuzzy_match_engine(query[keep_query], target[keep_target],  total.max=total.max, full.max=full.max,
                            self.match = self.match)

  if(!is.null(fms)) if(nrow(fms)>0) fms$reverse_dob <- FALSE

  message("\nEncontrando pareos con errores con mes y día invertido.")
  fms_r <- fuzzy_match_engine(query_r[keep_query_r], target[keep_target],  total.max=total.max, full.max=full.max)
  if(!is.null(fms_r)){
    if(nrow(fms_r)>0){
      if(self.match){ ## remove repeated comparisons
        fms_r <- fms_r[id.x!=id.y]
        fms_r[, pair := fifelse(id.x < id.y, paste(id.x, id.y, sep=":"), paste(id.y, id.x, sep=":"))]
        fms_r <- unique(fms_r, by="pair")
        fms_r[, pair:= NULL]
      }

      fms_r$reverse_dob <- TRUE
    }
  }

  fms <- rbindlist(list(fms, fms_r))

  if(!is.null(fms)){
    if(nrow(fms)>0){
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
    }
  }

  if(is.null(pms) & is.null(fms)) return(NULL)

  cols <- c("id.x", "id.y",
            "full_dist", "pn_dist", "sn_dist", "ap_dist", "am_dist",
            "full_nchar", "pn_nchar", "sn_nchar", "ap_nchar", "am_nchar",
            "full_i_match",  "pn_i_match", "sn_i_match", "ap_i_match", "am_i_match",
            "pn_freq","sn_freq", "ap_freq", "am_freq",
            "genero_match", "lugar_match", "match", "full_match",
            "swap", "reverse_dob", "truncated")
  if(!is.null(pms)){
    if(nrow(pms) > 0){
      pms <- pms[,..cols]
      setnames(pms, "match", "match_type")
    } else pms <- NULL
  }
  if(!is.null(fms)){
    if(nrow(fms) > 0){
      fms <- fms[,..cols]
      setnames(fms, "match", "match_type")
    } else fms <- NULL
  }

  map <- rbindlist(list(pms, fms))
  return(map)
}

perfect_match_engine <- function(query, target, by=NULL, by.x=NULL, by.y=NULL){

  if(is.null(by.x)) by.x <- by
  if(is.null(by.y)) by.y <- by

  ## remove rows with NAs in the columns we are matching by
  ind.x <- !matrixStats::rowAnyNAs(as.matrix(query[, ..by.x]))
  ind.y <- !matrixStats::rowAnyNAs(as.matrix(target[, ..by.y]))

  ## if no rows left return NULL
  if(length(ind.x)==0L & length(ind.y)==0L) return(NULL)

  ## Now we merge
  map <- merge(query[ind.x], target[ind.y], by.x = c(by.x, "dob"), by.y = c(by.y, "dob"))
  ## this pattern later tells you what kind of match it was

  ## because this is perfect match all no NA columns have 0 distance for full
  ## and all matched if not full
  cols <- c(outer(c("full", "pn", "sn", "ap", "am"), c("dist", "nchar"), paste, sep = "_"))
  map[,(cols) := as.numeric(NA)]

  cols <- c(outer(c("full", "pn", "sn", "ap", "am"), c("i_match"), paste, sep = "_"))
  map[,(cols) := as.logical(NA)]

  if(identical(by.x, "full")){
    map[, full_dist := 0]
    map[, full_i_match := TRUE]
    map[, full_nchar := nchar(full)]
    for(cn in c("pn", "sn", "ap", "am")){
      ind <- !is.na(map[[paste(cn, "x", sep = ".")]]) & !is.na(map[[paste(cn, "y", sep = ".")]])
      map[[paste(cn, "dist", sep = "_")]] <- ifelse(ind, 0, NA)
      if(cn != "sn"){
        map[[paste(cn, "i_match", sep = "_") ]] <- ind
      } else{
        map[[paste(cn, "i_match", sep = "_") ]] <- !is.na(map[["sn_i.x"]]) & !is.na(map[["sn_i.y"]])
      }
      map[[paste(cn, "nchar", sep = "_")]] <-
        pmax(nchar(map[[ paste(cn, "x", sep = ".") ]]),
             nchar(map[[ paste(cn, "y", sep = ".") ]]),
             na.rm = TRUE)
    }
  } else{
    map[, full_dist := stringdist(full.x, full.y)]
    map[, full_nchar := pmin(nchar(full.x), nchar(full.y))]
    cols <- paste(by.x[by.x!="sn_i"], "dist", sep="_")
    map[,(cols) := 0L]
    for(cn in by.x[!by.x%in%"sn_i"]){
      map[[paste(cn, "nchar", sep = "_")]] <- nchar(map[[ cn ]])
    }
    cols <- paste(by.x[!by.x%in%"sn_i"], "i_match", sep="_")
    map[,(cols) := TRUE]
  }

  ## remove and rename variables to keep only the original names
  ## we need this so that all the merged maps can be rbinded
  remove_cols <- intersect(paste0(c("full", "pn", "sn", "sn_i", "ap", "am"), ".y"), names(map))
  map <- map[,!..remove_cols]
  rename_cols <- intersect(paste0(c("full", "pn", "sn", "sn_i", "ap", "am"), ".x"), names(map))
  setnames(map, rename_cols, str_remove(rename_cols, ".x"))
  setcolorder(map, c("id.x",  "id.y", "full", "pn", "sn", "sn_i", "ap", "am", "dob",
                     "lugar.x", "lugar.y", "genero.x", "genero.y",
                     "full_dist", "pn_dist",  "sn_dist", "ap_dist", "am_dist",
                     "full_i_match", "pn_i_match",  "sn_i_match", "ap_i_match", "am_i_match",
                     "full_nchar", "pn_nchar",  "sn_nchar", "ap_nchar", "am_nchar"))
  return(map)
}

fuzzy_match_engine <- function(query, target, total.max = 8, full.max = 8, self.match = FALSE){

  query_index <- split(1:nrow(query), query$dob)
  target_index <- split(1:nrow(target), target$dob)

  common <- intersect(names(query_index), names(target_index))
  if(length(common)==0) return(NULL)
  query_index <-query_index[common]
  target_index <- target_index[common]

  qnames <- paste(names(query), "x", sep=".")
  tnames <- paste(names(target), "y", sep=".")

  pb <-  txtProgressBar( 1, length(common), style = 3)
  fms <- lapply(seq_along(common), function(i){
    setTxtProgressBar(pb, i)

    qind <- query_index[[i]]
    tind <- target_index[[i]]

    if(self.match & length(qind)<2) return(NULL)

    qq <- query[qind]
    tt <- target[tind]
    setnames(qq, qnames)
    setnames(tt, tnames)

    full_dist <- stringdistmatrix(qq$full.x, tt$full.y, method = "lv")

    pn_dist <- stringdistmatrix(qq$pn.x, tt$pn.y, method = "lv")

    if(self.match){ ##don't compare twice
      full_dist[lower.tri(full_dist, diag = TRUE)] <- Inf
      pn_dist[lower.tri(pn_dist, diag = TRUE)] <- Inf ## make pn_dist Inf instead of total so it stays Inf in the last name swap
    }

    pn_dist_nona <- pn_dist
    pn_dist_nona[is.na(pn_dist_nona)] <- 0

    sn_i_dist <- outer(qq$sn.x, tt$sn.y, `!=`)*1
    sn_i_dist[is.na(sn_i_dist)] <- 0

    sn_dist <- stringdistmatrix(qq$sn.x, tt$sn.y, method = "lv")
    sn_dist_nona <- sn_dist
    sn_dist_nona[is.na(sn_dist_nona)] <- sn_i_dist[is.na(sn_dist_nona)]

    ## consider reversed last names
    ap_dist <- stringdistmatrix(qq$ap.x, tt$ap.y, method = "lv")
    ap_dist_nona <- ap_dist
    ap_dist_nona[is.na(ap_dist_nona)] <- 0

    am_dist <- stringdistmatrix(qq$am.x, tt$am.y, method = "lv")
    am_dist_nona <- am_dist
    am_dist_nona[is.na(am_dist_nona)] <- 0

    total <- pn_dist_nona+sn_dist_nona+ap_dist_nona+am_dist_nona

    ## for all the rows of the target that are within 6 errors of the query we keep
    matches <- lapply(1:nrow(total), function(j){
      ind <- which(total[j,]<= total.max | full_dist[j,] <= full.max)
      if(length(ind)==0) return(NULL) else{
        ret <- cbind(qq[j],
                     tt[ind],
                     data.table(full_dist = full_dist[j, ind], pn_dist = pn_dist[j,ind], sn_dist = sn_dist[j,ind], ap_dist = ap_dist[j,ind], am_dist =  am_dist[j,ind]))
        ret$swap <- FALSE
        return(ret)
      }
    })
    nomatch_ind <- sapply(matches, is.null)

    ## if no match look for reverse last name match
    if(any(nomatch_ind)){

      full_dist <- full_dist[nomatch_ind,,drop=FALSE]
      pn_dist <- pn_dist[nomatch_ind,,drop=FALSE]
      sn_dist <- sn_dist[nomatch_ind,,drop=FALSE]

      qq <- qq[nomatch_ind]

      ap_dist <- stringdistmatrix(qq$ap.x, tt$am.y, method = "lv")

      am_dist <- stringdistmatrix(qq$am.x, tt$ap.y, method = "lv")

      total <- pn_dist_nona[nomatch_ind,,drop=FALSE] +
        sn_dist_nona[nomatch_ind,,drop=FALSE]+
        ap_dist + am_dist

      matches2 <- lapply(1:nrow(total), function(j){
        ind <- which(total[j,]<= total.max)
        if(length(ind)==0) return(NULL) else{
          ret <- cbind(qq[j],
                       tt[ind],
                       data.table(full_dist = full_dist[j, ind], pn_dist = pn_dist[j,ind], sn_dist = sn_dist[j,ind], ap_dist = ap_dist[j,ind], am_dist =  am_dist[j,ind]))
          ret$swap <- TRUE
          return(ret)
        }
      })
    } else{
      matches2 <- NULL
    }
    return(rbindlist(c(matches, matches2)))
  })

  fms <- rbindlist(fms)
  return(fms)
}


calibrate_matches <- function(map){

  map <- copy(map)
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

  map[, sn_i_dist := as.numeric(!sn_i_match)]
  map[, sn_i_nchar := as.numeric(!is.na(sn_i_match))]
  for(i in seq_along(patterns)){
    p <- patterns[[i]]
    dist_cols <- paste(p, "dist", sep="_")
    nchar_cols <- paste(p, "nchar", sep="_")
    if(!is.null(freqs[[i]])){
      freq_cols <- paste(freqs[[i]], "freq", sep="_")
    } else freq_cols<- NULL

    ind <- !matrixStats::rowAnyNAs(as.matrix(map[, ..dist_cols])) &
      is.na(map[,score]) ## needed columns not NA and not yet scored

    map$prop_match[ind] <- 1 - rowSums(map[ind,..dist_cols])/(rowSums(map[ind,..nchar_cols]))

    the_formula <- paste("lugar_match ~ pmax(prop_match,0.6)")
    if(!is.null(freq_cols)){
      the_formula <- paste(the_formula, "+",
                           paste0("log(pmax(", freq_cols, ",10^-4))", collapse = " + "))
    }
    if(sum(map[ind & !is.na(lugar_match)]$swap)>=100) the_formula <- paste(the_formula, "swap", sep="+")
    the_formula <- formula(the_formula)

    fit <- try(glm(the_formula, family = "binomial", data = map[ind & !is.na(lugar_match)]), silent=TRUE)
    if(class(fit)[1]=="try-error"){
      warning(paste0("Not enough data to fit model for pattern ",  paste(p, collapse = ":"),
                     ". Returing NA"))
      map[ind, score := NA]
    } else{
      map[ind, score := predict(fit, newdata = map[ind], type = "response")]
    }

    map[ind, pattern := paste(p, collapse = ":")]
  }

  map[, pattern := factor(pattern, levels = sapply(patterns, paste, collapse=":"))]
  map[, score := (score - min(score, na.rm=TRUE)) / max(score - min(score,na.rm=TRUE), na.rm = TRUE)]

  return(map)
}


cleanup_matches <- function(map, query, target, self.match, cutoff = 0){

  message("\nLimpiando pareos.")
  ## map must be output of calibrate map
  map <- copy(map)
  map <- map[!is.na(score)]

  ## Pick the best score for each id.x
  if(!self.match){
    map[order(id.y, !genero_match, !lugar_match, pattern, full_prop_match)]
    map <- map[map[,.I[which.max(score)], by = id.x]$V1]
  } else{
    map[order(id.y, !genero_match, !lugar_match, pattern, full_prop_match)]
    map <- map[map[,.I[which.max(score)], by = id.y]$V1]
    map <- unique(map, by=c("id.x", "id.y"))
  }

  map <- merge(merge(map, query, by.x = "id.x", by.y = "id", all.x = TRUE), target, by.x = "id.y", by.y = "id", all.x=TRUE)
  cols <- c("id.x", "id.y", "original.x", "original.y", "score",
            "prop_match", "full_prop_match", "pn_ap_match", "lugar_match", "genero_match", "pattern", "match_type", "full_match", "swap", "reverse_dob", "truncated")
  return(map[score>=cutoff, ..cols][order(score, decreasing = TRUE)])
}

