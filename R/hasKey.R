#' hasKey
#'
#' Checks if a specific index exists
#'
#' @param x listArray object
#' @param ... index to check
#'
#' @return logical: \code{TRUE} index exists, \code{FALSE} index exists not
#' @export
#'
#' @examples
#' l <- listArray()
#' l[1] <- 1
#' hasKey(l, 1)
#' l[2,3] <- "test"
#' hasKey(l, 2, 3)
#' l[2:3] <- "vector"
#' hasKey(l, 2:3)
#' l['iris'] <- iris
#' hasKey(l, iris) # FALSE
#' l[mean] <- mean
#' hasKey(l, mean)
#' # if you have not stored NULL objects in your listArray
#' is.null(l[mean])
#' is.null(l[iris])
hasKey <- function(x, ...) {
  stopifnot('listArray' %in% class(x))
  key(...) %in% names(x)
}