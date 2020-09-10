#' Returns a list of indices as string, list or unique values (like \code{dimnames}).
#'
#' @param x listArray object
#' @param type character: return the indices as string, list or unique values (default: \code{character})
#' 
#' @return Returns the indices as string, list or unique indices. If \code{type} is
#' \describe{
#' \item{\code{type="character"}}{a character vector with the retranslated indices}
#' \item{\code{type="list"}}{as list of lists with the retranslated indices}
#' \item{\code{type="names"}}{as list of lists with the retranslated unique(!) indices like \code{dimnames}}
#' }
#' 
#' @export
#'
#' @examples
#' l <- listArray(matrix(1:9, 3, 3))
#' k <- keys(l)
#' k
#' # access object in listArray
#' pos <- which(k=='3, 2')
#' l[[pos]]
#' #
#' l["test"] <- "test"
#' keys(l, 'c') # as keys(l)
#' keys(l, 'l')
#' keys(l, 'n')
#' # Note that l['test'][3] will deliver NULL since the entry does not exist
keys <- function(x, type="character") {
  stopifnot('listArray' %in% class(x))
  pos <- pmatch(type, c("list", "names"))
  k <- strsplit(names(x), ',', fixed=TRUE)
  k <- lapply(k, function(e) { unserialize(as.raw(strtoi(e, base=16))) })
  if (is.na(pos)) return(sapply(k, function(e) { toString(lapply(e, deparse)) }))   
  if (pos==1) return(k)
  km <- max(sapply(k, length))
  l  <- vector("list", km) 
  for (i in 1:km) {
    li <- unique(lapply(k, index=i, function(e, index) { 
      if (i>length(e)) NULL else e[[i]]
    }))
    l[[i]] <- li[lengths(li)!=0]
  }
  l
}