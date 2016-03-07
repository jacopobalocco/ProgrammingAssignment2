## Put comments here that give an overall description of what your
## functions do

# The first function, `makeCacheMatrix` creates a special "matrix", which is
# a list containing a function to
# 
# 1.  set the values in the matrix
# 2.  get the values in the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse


makeCacheMatrix <- function(X = matrix()) {
        INV <- NULL
        #matrix(, nrow(X), ncol(X)) 
        #creating an empty matrix with the same dimensions of X
        set <- function(y) {
                X <<- y
                INV <<- NULL
        }
        get <- function() X
        setinv <- function(solve) INV <<- solve
        getinv <- function() INV
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinv`
# function.


cacheSolve <- function(X, ...) {
        INV <- X$getinv()
        if(!is.null(INV)) {
                message("getting cached data")
                return(INV)
        }
        data <- X$get()
        INV <- solve(data, ...)
        X$setinv(INV)
        INV
}
