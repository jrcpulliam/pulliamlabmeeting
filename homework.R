# homework.R
# Created: 16 April 2013 by JRCP
#

# INSTRUCTIONS
# 1. read a file that has two rows: column names, column data types
#
# 2. based on the input from that file, read another binary file that has those columns with those data types
#
# ADDITIONAL INFO
# - The first file will be ascii text (strictly speaking, the english letter/number character subset of utf-8),
# no specific mimetype (aka, extension). single space between columns, newline between rows 
# - The second file will be binary data, no specific mimetype.

# NOTE that the package stringr is used is any of the data are character strings. To install the package, run
# the following line:
# install.packages(stringr)

read.fun <- function(fn1,fn2){
  # fn1 is the filename for the first file, which has two rows (column names, column data types)
  # Read in the first file:
  
  info <- read.table(fn1,sep=" ",as.is=T,encoding="UTF-8")
  cnames <- as.character(info[1,])
  ctypes <- as.character(info[2,])
  
  dat <- NULL
  
  # Open file 2 for reading
  cc <- file(fn2,"rb")  
  
  repeat{
    temp <- sapply(1:length(cnames),function(ii){
      
      if(grepl("char",ctypes[ii])){
        len <- as.numeric(strsplit(ctypes[1],"character")[[1]][2])
        leido <- readChar(cc,len)
        require(stringr)
        leido <- str_trim(leido)
      }else{    
        leido <- readBin(cc,what=ctypes[ii]) 
      }
      return(leido)
    })
    
    if(all(is.character(temp))){
      dat <- rbind(dat,temp)
    }else{
      # Close file 2
      close(cc)
      
      dat <- suppressWarnings(data.frame(dat))
      names(dat) <- cnames
      
      break      
    }
  }
  
  # browser()
  return(dat) 
}

output <- read.fun("testHeads.o","testbin.o")
