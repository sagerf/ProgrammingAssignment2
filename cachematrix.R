## These 2 functions will do the following:
## 1: makeCacheMatrix: creates a matrix that caches its inverse
## 2: cacheSolve: checks if inverse is already in the cache


makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a matrix that caches its inverse
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    ## Next, we set "i" to be the matrix inverse and cache it
    setinv <- function(solve) i <<- solve 
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    ## if "i" the inverse is not null in the cache, i is returned
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Otherwise, the inverse is calculated
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

