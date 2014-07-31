## ---------------------------------------------------------------------
##  Programming Assignment 2: matrix cache
##
##  https://class.coursera.org/rprog-005/assignment/
## ---------------------------------------------------------------------

## Matrix object that cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    ## JVA - Initialize the inverse property
    i <- NULL

    ## JVA - setter matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## JVA - getter matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## JVA - setter matrix inverse
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## JVA - getter matrix inverse
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## JVA - Return a list of the methods
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## JVA - Begin ---------------------------------------------------------------
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
## JVA - End -----------------------------------------------------------------
cacheSolve <- function(x, ...) {

    ## JVA - Return the inverse of 'x'
    i <- x$getInverse()

    ## JVA - return cache if exits
    if( !is.null(m) ) {
            message("Get from cache")
            return(m)
    }

    ## JVA - otherwise get the matrix from 'x' object
    m <- x$get()

    ## JVA - calculate the inverse using solve
    i <- solve(m) %*% m

    ## JVA - set the inverse to the object
    x$setInverse(i)

    ## JVA - Return the inverse
    i
}

## JVA - Begin - sample test driver
## m<- matrix(c(2,2,3, 3,2,1,  1,0,1), nrow = 3, ncol = 3, byrow = TRUE,
##                dimnames = list(c("f1", "f2", "f3"), c("c1", "c2", "c3")))
## JVA - End
## mdata <- makeCacheMatrix(m)

## ii<- cacheSolve(mdata)
## ii
