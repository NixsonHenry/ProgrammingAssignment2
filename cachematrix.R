## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # contiendra l'inverse mise en cache
  set <- function(y) {
    x <<- y      # met ?? jour la matrice
    inv <<- NULL # r??initialise l'inverse
  }
  
  get <- function() x  # retourne la matrice
  setinverse <- function(inverse) inv <<- inverse  # stocke l'inverse
  getinverse <- function() inv     # r??cup??re l'inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of a matrix returned by 
# makeCacheMatrix().
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  # retrieve the inverse if it already exists
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)        # return the cache if available
  }
  
  mat <- x$get()         # otherwise, retrieve the matrix
  inv <- solve(mat, ...) # calculate the inverse
  x$setinverse(inv)      # cache
  inv                    # return the result
}


# Create a matrix
m <- matrix(c(2, 1, 1, 2), nrow = 2)

# Create the special object
cachedMatrix <- makeCacheMatrix(m)

# First calculation ??? calculates and caches
cacheSolve(cachedMatrix)

# Second call ??? uses the cache
cacheSolve(cachedMatrix)

