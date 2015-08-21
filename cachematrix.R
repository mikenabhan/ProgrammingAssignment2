#This first function is pretty much a direct copy of the the
#"makeVector" function described by professor peng in the course.

#the first line creates a blank matrix by default and is operated
#by passing through matrix commands either by setting a variable
#and inserting a subset.  or by interacting directly with the function.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
#line 13 sets m to null in the parent enviornment just as we saw
#in the makeVector function
  m<<-NULL
      }
      get<-function() x
      setmatrix<-function(solve) m<<- solve
      getmatrix<-function() m
      list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
#this function checks if m has a value in the global environment
#and it calls upon it.  if null, it uses solve to calculate the
#inverse of the matrix.
cacheSolve <- function(x=matrix(), ...) {
   #calls on the global enviornment to check if m is a value and if so
   #it returns m.  if not it proceeds
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    #where it is solved here using the solve function and passing through
    #other commands using the "..."
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
