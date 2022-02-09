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
  query_index <-query_index[common]
  target_index <- target_index[common]

  pb <-  txtProgressBar( 1, length(common), style = 3)

  qnames <- paste(names(query), "x", sep=".")
  tnames <- paste(names(target), "y", sep=".")

  fms <- lapply(seq_along(common), function(i){
    setTxtProgressBar(pb, i)

    qind <- query_index[[i]]
    tind <- target_index[[i]]

    if(self.match){
      tind <- tind[!tind %in% qind] ##remove same record to not compare to self
      if(length(tind) == 0) return(NULL)
    }

    qq <- query[qind]
    tt <- target[tind]
    setnames(qq, qnames)
    setnames(tt, tnames)

    full_dist <- stringdistmatrix(qq$full.x, tt$full.y, method = "lv")

    pn_dist <- stringdistmatrix(qq$pn.x, tt$pn.y, method = "lv")
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
      ind <- which(total[j,]<= total.max | full_dist[j,] <=  full.max)
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
          ret$swap <- FALSE
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

