## To prepare a matrix into a "makeCacheMatrix"
## When we call cacheSolve function, it will first check whether the inverse of this matrix has been
## calculated. If so, it will print the cached inverse, otherwise it will compute it and cache it.

## Prepare a matrix into a "makeCacheMatrix" and get it ready for cacheSolve function
makeCacheMatrix <- function(x = numeric()) 
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setinverse <- function (inverse)
        m <<- inverse
    getinverse <- function ()
        m
    list (set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## If the inverse of x has been calculated then it will get the cached data
## Otherwise it will calculate the inverse of x
cacheSolve <- function(x, ...)
{
    m <- x$getinverse()
    if (!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
