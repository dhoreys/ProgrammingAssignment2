#makeCacheMatrix creates a list similar to makeVector as shown in example
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve function. This assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

###########Please see sample output###########
###########Note for the first time it computes and second time it gets answers from the cache! ###########
######## See the msg "getting cached data" #####

#> mtrx = makeCacheMatrix(x)
#> x = rbind(c(10, -20), c(-20, 10))
#> mtrx = makeCacheMatrix(x)
#> mtrx$get()
#[,1] [,2]
#[1,]   10  -20
#[2,]  -20   10

#> cacheSolve(mtrx)
#[,1]        [,2]
#[1,] -0.03333333 -0.06666667
#[2,] -0.06666667 -0.03333333

#> cacheSolve(mtrx)
#getting cached data.
#[,1]        [,2]
#[1,] -0.03333333 -0.06666667
#[2,] -0.06666667 -0.03333333

