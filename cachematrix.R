
makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() {x}
  setInversa <- function(inversa1) {inversa <<- inversa1}
  getInversa <- function() {inversa}
  list(set = set, get = get,
       setInversa = setInversa,
       getInversa = getInversa)
}


cacheSolve <- function(x, ...) {
  inversa <- x$getInversa()
  if(!is.null(inversa)) {
    message("metodo cache para matrix inversa")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setInversa(inversa)
  inversa
}
