## Put comments here that give an overall description of what your
## 
## The matrix CacheMatrix needs 2 input variables A and n. 
## It comes back with a list of 2 elements
##    - Inverse, is the function to calculate the inverse 
##    - Out is the matrix xalculation A*(Inv(A))^n
##    - Original is the matrix A
## The inverse is only calculated when A != 0
############################################################
makeCacheMatrix <- function(x = matrix(),n) {
  out <- x
  if(n!=0){
    Inv <<- solve(A)
    Invf<-function(){
      return(Inv)
    }
    for(i in 1:n){
      out<-out%*%Inv
    }
  }
  else{Invf <-function(){}}
  list(Inverse=Invf,out=out, original = x)
}


## This function gets the inverse of the matrix specified in the previous function. 
## If n=0, the cache is empty, and the inverse should be calculated here.
cacheSolve <- function(x, ...) {
  m <- x$Inverse()
  if(!is.null(m)) {
    message("getting cached data")
  }
  else{
    m<-solve(x$original)
  }
  m
}

A<-matrix(rnorm(25),5,5)
B<-makeCacheMatrix(A,3)
### Inverse already calculated, so cache can be reused
cacheSolve(B)

A2<-matrix(rnorm(25),5,5)
B2<-makeCacheMatrix(A2,0)
### Inverse not yet calculated (n==0), so cache cannot be reused and inverse should be calculated
cacheSolve(B2)
