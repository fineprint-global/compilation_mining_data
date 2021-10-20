### Description: This function reads in the last modified/saved file in a given folder
# with a specific pattern (filenamepattern) 
# from a given directory (directory)

my_lastfile <- function(directory, filenamepattern){

  files <- list.files(path = directory, pattern = filenamepattern,full.names = TRUE,recursive = TRUE)
  
  # get the directory names of these (for grouping)
  dirs <- dirname(files)
  
  # find the last file in each directory (i.e. latest modified time)
  lastfile <- tapply(files,dirs,function(v) v[which.max(file.mtime(v))])
  lastfile <- basename(lastfile)
  lastfile <- paste0(directory,lastfile)
  
  return(lastfile)
  
}
