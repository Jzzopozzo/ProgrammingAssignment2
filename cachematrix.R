## Put comments here that give an overall description of what your
## functions do

## this is a function which creates a special "matrix", which is really a list containing a function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse of the matrix
# 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set<- function(y)
        {
                x<<- y
                i<<- NULL
        }
        get<- function() x
        setinverse<- function(inverse) i<<- inverse
        getinverse<- function() i
        list(set= set,
             get= get,
             setinverse= setinverse, 
             getinverse= getinverse
        )
}


## The following function calculates the mean of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if(!is.null(m))
        {
                message("Getting cached data...")
                return (m)
        }
        data<- x$get()
        m<- solve(data, ...)
        x$setinverse(m)
        m
}
