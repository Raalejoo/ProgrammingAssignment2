## Cache de la Media

cachemean <- function(x, ...) {
  n <- x$getmean()
  if(!is.null(n)) {
    message("Obteniendo datos del cache")
    return(n)
  }
  data <- x$get()
  n <- mean(data, ...)
  x$setmean(n)
  n
}

## Creando el Cache de la Matriz

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Resolviendo el Cache
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
    message("Obteniendo datos del cache")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i
}