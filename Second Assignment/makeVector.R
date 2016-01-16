## Function "makeVector"
#
# This function creates a Special Vector object
# that contains special fields and set/get functions.
# It is a simulation of the Object Oriented logic.

makeVector <- function(x = numeric()){
  # Initialize the vector's mean value
  m <- NULL
  
  # Sets the value of the vector
  set <- function(y){
    x <<- y
    m <<-NULL
  }
  
  # Returns the value of the vector
  get <- function() x
  
  # Sets the value of the vector's mean
  setmean <- function(mean) m <<-mean
  
  # Returns the vector's mean
  getmean <- function() m
  
  # A list with the function names so that we can call them
  # by tying x$<function_name>
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}