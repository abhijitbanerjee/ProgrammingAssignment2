makeCacheMatrix <- function(x = matrix(x <- c(), ncol = sqrt(length(x)))){
        get <- function() x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        setinv <- function(solve) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
        
cachesolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinv(m)
        m
}


