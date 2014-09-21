#
##  This pair of functions calculates the inverse of a matrix and caches the result
##  to reduce computation time when the inverse is needed again.
##  The inverse is calculated using the solve function in R.
##  We assume that the matrix supplied is always invertible.
##
##  The cache system is initialized in a three step process.
##
##  1. the function definitions are run for makeCacheMatrix() and cacheSolve()
##
##  2. makeCacheMatrix(matrix.A) is called, with the matrix to be inverted as input.
##
##      funcvect1 <- makeCacheMatrix(matrix.A)
##
##     The ouput is a vector of 4 functions: 
##     set(y), get(), setSolve(solve), getSolve(), along with 
##
##  3. when the inverse of matrix.A is wanted, call cacheSolve(funcvect1)
##
##     My_result <- cacheSolve(funcvect1)
##
##     the first invocation will calculate the inverse matrix, and cache it in its
##     internal environment.  Subsequent calls will fetch the cached copy.
##



##   makeCacheMatrix creates the environment for the cache, and 4 functions
##   to set & get the input matrix and
##   to set & get the output (inverted) matrix.
##   These functions are used by the cacheSolve function to implement the cache.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get      <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##   cacheSolve gets the output (the inverted input matrix).
##   The first call, calculates the inverse and caches it.
##   subsequent calls fetch the inverted matrix from the cache.
##   It uses the R solve function, and assumes that it is invertable.
##
##   It Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


# --------------------------------------------------------
# Test solve, cacheSolve

#  mtrx.A <- matrix(1:4, nrow = 2, ncol = 2)
#  mtrx.A

#  solve(mtrx.A)


#  mtrx.B <- matrix(c(1:4,7:11), nrow = 3, ncol = 3)
#  mtrx.B

#  solve(mtrx.B)


#   funcvect1 <- makeCacheMatrix(mtrx.A)
#   cacheSolve(funcvect1)
#   funcvect1$getsolve()
    
#   funcvect2 <- makeCacheMatrix(mtrx.B)
#   cacheSolve(funcvect2)
#   funcvect2$getsolve()

#   debug(cacheSolve)
#   undebug(cacheSolve)
#
#  Thanks to Thiago Balbo for his forum thread illustrating the 
#  scoping and environments for the Caching the Mean of a Vector example.
#  https://class.coursera.org/rprog-007/forum/thread?thread_id=707
#  
#  I was confused by 3 kinds of vectors:
#  1. a numeric data vector, 
#  2. a vector of function names.
#  3. the mean was also a vector of length 1
#