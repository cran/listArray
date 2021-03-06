## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- setup, echo=FALSE-------------------------------------------------------
library(listArray)

## -----------------------------------------------------------------------------
l <- listArray(1)
length(l)
names(l)
l[[1]]
class(l)

## -----------------------------------------------------------------------------
keys(l)

## -----------------------------------------------------------------------------
# unnamed vectors
v <- 1:5
l <- listArray(v)
keys(l)
#
l <- listArray(letters[1:5])
l[1]
# named vector
v <- 1:5
names(v) <- letters[1:5]
l <- listArray(v)
l["a"]

## -----------------------------------------------------------------------------
m <- matrix(1:9, 3, 3)
l <- listArray(m)
l[2,3]  # should be 8

## -----------------------------------------------------------------------------
m <- matrix(1:4, 2, 2)
colnames(m) <- LETTERS[1:2]
l <- listArray(m)
keys(l)

## -----------------------------------------------------------------------------
m <- matrix(1:4, 2, 2)
colnames(m) <- LETTERS[1:2]
l <- listArray(m, use.names=FALSE)
keys(l)

## -----------------------------------------------------------------------------
m <- diag(3)
l <- listArray(m, ignore=0)
keys(l)

## -----------------------------------------------------------------------------
nozeroes <- function(v) { v==0 }
#
m <- diag(3)
l <- listArray(m, ignore=nozeroes)
keys(l)

## -----------------------------------------------------------------------------
e <- listArray(env=TRUE)
class(e)
e[1] <- "hello world"
e[1]
ls(e)

## -----------------------------------------------------------------------------
l <- listArray()
l[0] <- 1
l[0]
l[pi] <- pi
l[pi]
anotherpi <- pi
l[anotherpi]
l[1,-2] <- 3
l[1,-2]

## -----------------------------------------------------------------------------
m <- 1:5
m[1:2]

## -----------------------------------------------------------------------------
l <- listArray(m)
l[1:2]
keys(l)

## ---- error=TRUE--------------------------------------------------------------
m <- matrix(1:4, 2, 2)
m[1,]
l <- listArray(m)
l[1,] # will even throw an error

## -----------------------------------------------------------------------------
identical(1:2, c(1,2)) # delivers FALSE!
# but
m <- matrix(1:9, 3, 3)
m[1:2,2]
m[c(1,2),2]

## -----------------------------------------------------------------------------
l <- listArray()
l[1:3] <- 1
l[c(1,2,3)]
options(listArray.expand=FALSE) # now 1:3 != c(1,2,3)
l <- listArray()
l[1:3] <- 1
l[c(1,2,3)]

