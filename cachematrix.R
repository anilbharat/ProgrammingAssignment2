## Put comments here that give an overall description of what your
## Write a short comment describing this function
# this function works like a class, it creates a list
# of set, get, setInv, getInv as four member function. 
# used " <<- "  assignment operator so that
# these internal variables are not exposed to the
# outside environment. 
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL # this is where the result of inversion is stored
  set <- function(y) {
    x <<- y
    x_inv <<- NULL # it also initialises x_inv to null
  }
  
  get <- function() x # return the input matrix
  set_Inv <- function(inv) x_inv <<- inv # set the inversed matrix
  get_Inv <- function() x_inv # return the inversed matrix

  # x$setInv # to set the inversed matrix
  # x$getInv # to get the inversed matrix
  list(set = set, get = get,
       set_Inv = set_Inv,
       get_Inv = get_Inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$get_Inv()
  if(!is.null(m)) {
    message("getting chached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$set_Inv(m)
  m
         ## Return a matrix that is the inverse of 'x'
  }

# to test this function code
# generate a random square
#runif(9,1,100)
#rnorm(4,1,5)

a_test <- matrix(runif(9,1,4),3,3)

catchtest <- makeCacheMatrix(a_test)

test1 <- cacheSolve(catchtest)

View(test1)
