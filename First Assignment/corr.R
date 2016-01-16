corr <- function(directory, threshold = 0){
  
  # Import the data frame that contains the number of complete cases for each data set
  df <- complete(directory)
  
  nobs <- df$nobs
  id <- df$id
  
  corr1 <- numeric(0)
  
  for(i in 1:length(nobs)){
    
    if(nobs[i] > threshold){

      # Import the data set
      if(id[i] < 10){
        tempdata <- read.csv(paste(directory,"/00",as.character(id[i]),".csv",sep = ""))
      }else if(id[i] <100){
        tempdata <- read.csv(paste(directory,"/0",as.character(id[i]),".csv",sep = ""))
      }else{
        tempdata <- read.csv(paste(directory,"/",as.character(id[i]),".csv",sep = ""))
      }
      
      # Remove the NAs
      index <- complete.cases(tempdata$sulfate,tempdata$nitrate)
      data <- tempdata[index,]
      sulfate <- data$sulfate
      nitrate <- data$nitrate
      
      # Calculate the correlation
      temp_c <- cor(sulfate,nitrate)
      corr1 <- c(corr1,temp_c)
      print(corr1)
    }
  }
  corr1
}