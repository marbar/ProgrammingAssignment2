
## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {         ##saving matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x          ##getting matrix
  setinverse <- function(inverse) m <<- inverse ##saving inverse matrix
  getinverse <- function() m                    ##getting inverse matrix 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  ##list of functions
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() 	        #query the x matrix's cache         
  if(!is.null(m)){ 		#if there is a cache
  	message("getting cached data")
  		return(m)  		#just return the cache, no computation needed
  	}
  data <- x$get() 		#if there's no cache
  m <- solve(data,...)   	#we actually compute them here
  x$setinverse(m) 		#save the result back to x's cache
  m 	
}
