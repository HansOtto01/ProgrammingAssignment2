# Below are two functions that are used to create a special object
# that stores a matrix and caches its inverse. The <<- operator is
# used to assign a value to an object in an environment that is
# different from the current environment. 


# The first function, makeCacheMatrix creates a list containing
# functions to set/get the value of the vector and set/get the
# value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
		# input x: invertible matrix
        # return:  list of functions used as input to cacheSolve
        # 		   1. set matrix
        #          2. get matrix
        #          3. set inverse
        #          4. get inverse

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The second function, cacheSolve calculates the inverse of the
# list created with the above function. If the mean has already
# been calculated then cache is used instead of computation.

cacheSolve <- function(x, ...) {

		# input x: see makeCacheMatrix
        # return:  inverse of matrix input to makeCacheMatrix

        i <- x$getinv()
		
		# if inverse has already been calculated
		# then use cached result
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}