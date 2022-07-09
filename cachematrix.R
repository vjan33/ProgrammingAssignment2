## The Functions MakeCacheMean and cacheSolve, cache the inverse of a matrix. 

## makeCacheMatrix is a function that creates an R object that stores a matrix 
## and its inverse. This way the function creates a "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y){   # set() assigns the argument 'y' to the object 'x' in  
    x <<- y             # the parent environment. It also clears any cached  
    i <<- NULL          # value of the inverse 'i' by setting i as NULL.
  }
  
  get <- function() x   # get() retrieves x from the parent environment of 
                        # makeCacheMatrix().
  
  setinverse <- function(solve) i <<- solve
  
  getinverse <- function() i
  
  list(set = set, get = get,      # Naming the list elements allows the use of 
       setinverse = setinverse,   # the operator $ to extract the contents of 
       getinverse = getinverse)   # the matrix.

}


## cachesolve calculates the inverse and prints the inverse of the matrix 
## returned by makeCacheMatrix. In the case that the matrix is unchanged and the 
## inverse has already been calculated, cachesolve retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){                  # if !is.null(i) is TRUE cachesolve() 
    message("getting cached data")  # retrieves the inverse from the cache.
    return(i)                       
  }
  data <- x$get()                   # if !is.null(i) is FALSE cachesolve()
  i <- solve(data, ...)             # computes the inverse and prints it.
  x$setinverse(i)
  i
}

