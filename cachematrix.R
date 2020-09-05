## Put comments here that give an overall description of what your
## functions do: My functions together calculate an inverted matrix from a
## matrix argument and cache the inverted matrix. Whenever the second function 
## is called to invert a matrix, the function either returns the cached inverted
## matrix or calculates it.


## Write a short comment describing this function:
## makeCacheMatrix() creates getter & setter functions that get matrix x
## and manipulate x. It then returns a list that effectively points to the
## environment in which x and the functions are defined, allowing cacheSolve to 
## use its functions & variables.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## Write a short comment describing this function: cacheSolve() checks to see
## if an inverted matrix has already been calculated and cached. If so, it 
## returns that. If not, it calculates it, caches it, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        
        if(!is.null(inv)) {
                message("Retrieving cached inverse matrix:")
                return(inv)
        }
        
        else
                data <- x$get()
        inverse <- solve(data)
        x$set_inv(inverse)
        inverse
}
