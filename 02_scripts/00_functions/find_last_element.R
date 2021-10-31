# Description of function: 
# Traverses a matrix of a list row-wise from right to left, and gives the first 
# position (from the right) where an item is not "". If the leftmost element in the row 
# is still "", it returns position 1

# Used in: 
# directly in the function split_columns.R
# indirectly in the script 07_georeferencing.R via the function split_columns


find_last_element <- function(list, row, col, max_cols){
  
  if(list[[col]][row,max_cols] != ""){
    return(max_cols)
    
  } else {
    if(max_cols <=1){return(1)}
    check_last_element(list, row, col, max_cols - 1)
  }
}