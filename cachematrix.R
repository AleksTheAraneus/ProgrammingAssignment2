## Thank you ever so much for taking your time to check my assignment (of many others
## which may be more worthy of your attention (dramatism))!
## Note that my explanations may be overly detailed and and naive - yes, I am new
## to R and programming in general :)

## The below script is used for calculating and caching the inverse of a matrix.

## The function makeCacheMatrix() takes a matrix as an argument and returns a list
## of handy functions. Importantly, it allows for storage (caching) a calculated
## inverse of the matrix with the x$setinv() function. It works analogously to the 
## function makeVector() from the Assignment 2 description from the Coursera course
## 'R programming" and, to be honest, doesn't differ much from it.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inv) {
        i <<- inv
    }
    getinv <- function() {
        i
    }
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The function cacheSolve() takes a product of makeCacheMatrix() as an argument
## and returns an inverse of the matrix if it is possible. It checks if the result
## has already been calculated; in this case it uses the cached data. Otherwise,
## it calculates the inverse from scratch, calls x$setinv() from makeCacheMatrix()
## to store it, and then returns the result. 

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}