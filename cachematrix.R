## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL        
  set <- function(y) {                   # set the value of the matrix
    x <<- y
    xinv <<- NULL     
  }
  get <- function() x                    # return the matrix
  set_xinv <- function(inv) xinv <<- inv # set the inverse matrix
  get_xinv <- function() xinv            # get the inverse matrix
  list(set = set, get = get,
       set_xinv = set_xinv,
       get_xinv = get_xinv)    
}

##  calculates the inverse of a matrix created with the above function
cacheSolve <- function(x, ...) {
  m <- x$get_xinv()                      # get the inverse matrix 
  if(!is.null(m)) {                      # if the inversion exist
    message("getting cached data")
    return(m)                            # return the inverse
  }
  data <- x$get()                        # if not, get the matrix 
  m <- solve(data)                       # we calculate the inverse
  x$set_xinv(m)                          # set the inverse
  m                                      # return m
}


