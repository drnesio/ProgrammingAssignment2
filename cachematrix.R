## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


# Criar uma matriz de teste
matriz_teste <- matrix(c(5, 2, 3, 4), nrow=2)

# Usar a função makeCacheMatrix para criar o objeto especial
objeto_teste <- makeCacheMatrix(matriz_teste)

# Usar a função cacheSolve para obter a inversa
inversa_teste <- cacheSolve(objeto_teste)

# Resultado
print(inversa_teste)




