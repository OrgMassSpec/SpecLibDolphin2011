MakeTable <- function(x = dolphin2011.meta) {

  x <- x[x$compound != "internal standard", ] # interferes with GetSource

  ## Compound classes and total isomers within each class
  tbl <- as.data.frame(table(x$category.2))
  names(tbl) <- c("class", "total isomers")

  ## Number of manually identified spectra within each class
  x.manual <- x[x$category.3 == "Manual" |
                x$category.3 == "Manual - Congener Group" |
                x$category.3 == "Unknown", ]
  tbl.manual <- as.data.frame(table(x.manual$category.2))
  manual <- tbl.manual$Freq

  ## Class source
  GetSource <- function(cls) {
    tmp <- x[x$category.2 == as.character(cls), ]
    sou <- as.character(tmp$category.1[1])
  }
    
  tbl.sou <- sapply(tbl$class, GetSource)

  ## Halogenation range
  GetBromine <- function(frm) {
    if(!is.na(frm)) {
      tmp <- ListFormula(frm)$Br
    } else {
      tmp <- NA
    }
  }

  x$bromines <- sapply(x$formula, GetBromine)

  GetChlorine <- function(frm) {
    if(!is.na(frm)) {
      tmp <- ListFormula(frm)$Cl
    } else {
      tmp <- NA
    }
  }

  x$chlorine <- sapply(x$formula, GetChlorine)

  GetNumBromines <- function(cmpd.class) {
    tmp <- x[x$category.2 == as.character(cmpd.class), ]
    brs <- unique(tmp$bromine)
    result <- paste(brs[order(brs)], collapse = ", ")
  }

  tbl.br <- sapply(tbl$class, GetNumBromines)

  GetNumChlorines <- function(cmpd.class) {
    tmp <- x[x$category.2 == as.character(cmpd.class), ]
    cls <- unique(tmp$chlorine)
    result <- paste(cls[order(cls)], collapse = ", ")
  }

  tbl.cl <- sapply(tbl$class, GetNumChlorines)

  ## Make table
  results.tbl <- cbind(tbl, manual, tbl.sou, tbl.br, tbl.cl)

  names(results.tbl) <- c("class", "total", "manual", "source", "bromines", "chlorines")
    
  return(results.tbl)

}
