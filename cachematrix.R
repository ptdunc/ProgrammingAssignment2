# Below are two functions that together cache the inverse of a
# matrix.

# The first function, makeCacheMatrix, creates a special "matrix"
# object that can cache its inverse. This special "matrix" is a
# list contatining a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
    # Create a matrix that can cache its inverse
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) i <<- inverse
    get.inverse <- function() i
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


# The following function computes the inverse of the special
# "matrix" returned by makeCacheMatrix and retrieves the inverse.
# First, determine whether the inverse has already been calculated.
# If so, get inverse from the cache and skip the computation.
# If not, compute the inverse and set the value of the inverse
# using the set.inverse function.

cacheSolve <- function(x, ...){
    # Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set.inverse(i)
    i
}