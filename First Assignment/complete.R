complete <- function(directory, id = 1:332){
  
  for(i in 1:length(id)){
    
#     if(id[i]<10){
#       s <- paste("00",as.character(id[i]),".csv",sep = "")
#     }else if(id[i]<100){
#       s <- paste("0",as.character(id[i]),".csv",sep = "")
#     }else{
#       s <- paste(as.character(id[i]),".csv",sep = "")
#     }
    
    # Trick for loading all the different csv files into a single data frame
    if(id[i] < 10){
      tempdata <- read.csv(paste(directory,"/00",as.character(id[i]),".csv",sep = ""))
    }else if(id[i] <100){
      tempdata <- read.csv(paste(directory,"/0",as.character(id[i]),".csv",sep = ""))
    }else{
      tempdata <- read.csv(paste(directory,"/",as.character(id[i]),".csv",sep = ""))
    }

    comp <- complete.cases(tempdata$sulfate, tempdata$nitrate)
    comp_num <- sum(comp)
    
    if(i == 1){
      name <- s
      name <- id[i]
      cases <- comp_num
    }else{
      name <- c(name,id[i])
      cases <- c(cases,comp_num)
    }
  }
  df <- data.frame(id = name, nobs = cases)
}