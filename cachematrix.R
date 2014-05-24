#***************************************
# makeCacheMatric - this function makes inverse
# It has 4 methods get, set, getInverse and setInverse
#***************************************
makeCacheMatrix <- function (x = matrix()) {
        
        i <- NULL
        # Set() assigns a value to vector        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # Get() retrives a value of the vector
        get <- function() x
        
        # SetInverse() Sets the inverse of a matrix. We used solve function.
        setInverse <- function(solve) i <<- solve
        
        # GetInverse() Gets the inverse of a matrix
        getInverse <- function() i
        
        #Makes a list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

#***************************
# CacheSolve function will cache the funtion 
# and solve only if the
# function is not cached
#**************************

cacheSolve <- function (x) {
        i <- x$getInverse()
        
        # check if null and return the cached data if null
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        #gets the data and assigns it to data
        data <- x$get()
        
        # calculate the inverse
        i <- solve(data)
        
        #sets to inverse
        x$setInverse(i)
        i        
}
