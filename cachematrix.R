##  function that creates a special "matrix" object that can be cache its inverse
makeCacheMatrix<-function(x = matrix()){ 		## matrix as an argument 
  invM<-NULL			        ## will hold the value for the inverse matrix, NULL as default
  set<-function(y){				## set the value of the matrix
    x<<-y 								## assigns the value of matrix to `x`
    invM<<-NULL 					## if there is a new matrix, resets invM to NULL
  }
  get<-function()x 				## get the value of matrix
  setInvM<-function(inverseMatrix) invM <<- inverseMatrix 	## set the value of inverse 
  getInvM<-function()invM 	## get the value of the inverse
  list(set=set, get=get, setInvM=setInvM, getInvM=getInvM)  ##shows the list of functions
}

## function that solve for the inverse of the special "matrix" from the `makeCacheMatrix` function
## returns inverse from the cache if the inverse of the matrix has been calculated before

cacheSolve<-function(x, ...){
  invM<-x$getInvM()
  if(!is.null(invM)){ 		## checks if invM is not null, 
    message("getting cached data")	## if so skips the computation and gets the inverse from cache
    return(invM) 
  }
  data<-x$get() 	## gets the value of matrix and assign to `data`
  invM<-solve(data, ...) 		## solves for the inverse
  x$setInvM(invM)
  invM ## prints inverse
}