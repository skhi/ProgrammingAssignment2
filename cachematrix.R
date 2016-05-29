## Main goal of the function: to return the inverse of matrix


## makeCacheMatrix: the function returns a list of a set of functions: set, get, setInverse and getInverse
## These functions are designed for the user's action

makeCacheMatrix <- function(x = matrix()) {
    
    inv_ <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inv_ <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv_ <<- inverse
    getInverse <- function() inv_
    
    list(set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse )
}


## cacheSolve: the function checks if the argument matrix contains non-NULL inverse matrix. If it does contain,
## the function just returns it. If not, then the function accesses the matrix of the argument and
## calculates the inverse

cacheSolve <- function(x, ...) {
    
    inv_ <- x$getInverse()
    if(!is.null(inv_)) {
        message("getting inverse of matrix , ", deparse(substitute(x)), ", from cached data" )
        return(inv_)
    }
    
    mat_orig <- x$get()
    inv_ <-  solve(mat_orig,...)
    x$setInverse(inv_)
    
    inv_
}