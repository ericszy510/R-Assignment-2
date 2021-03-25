makeCacheMatrix <- function(x = matrix()){
        inv1 <- NULL
        set <- function(y){
                x <<- y
                inv1 <<- NULL
        }
        get <- function() x
        set_inverse <- function(Inverse) inv1 <<- Inverse
        get_inverse <- function() inv1
        list(set= set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


cacheSolve <- function(x, ...){
        inv1 <- x$get_inverse()
        if(!is.null(inv1)){
              message("getting cached data")
              return(inv1)
        }
        abc <- x$get()
        inv1 <- solve(abc, ...)
        x$set_inverse(inv1)
        inv1
}
