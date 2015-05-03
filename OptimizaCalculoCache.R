
makeCacheMatrix <- function(x = matrix()) {
        
        Minv <- NULL 
        set <- function(y) {
                x <<- y
                Minv <<- NULL # inicializa con NULL
        }
        
        get <- function() x #devuelve la matriz de entrada
        setInv <- function(inv) Minv <<- inv # coloca la  matrix invertida
        getInv <- function() Minv # devuelve la matriz invertida
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


cacheSolve <- function(x, ...) {
        m <- x$getInv() # obtiene la matriz inversa del objeto x
        
      if(!is.null(m)) {
	  message("getting cached data")
	  return(m) 
      }
      data <- x$get() 
      m <- solve(data) 
      x$setInv(m) 
      m # devuelve la matriz inversa utilzando las funciones de cache
  }

# Probando ..
# genera un matriz aleatoria 
test <- matrix(runif(16,1,100),4,4)
# generate the makeCacheMatrix object with this matrix
testCached <- makeCacheMatrix(test)
# from now on calculate or retrieve calculated inversion using the cacheSolve function

testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)
testInv <- cacheSolve(testCached)

