# Description of function: 
# Splits the columns in a dataframe according to a str_split_operator 
# and creates a output dataframe with unique rows for each column that was split

# Used in: 
# 07_georeferencing.R


# read in function for finding the last element in a matrix (as this function depends on it)
source("./02_scripts/00_functions/find_last_element.R")


split_columns <- function(df_input, str_split_operator) {
  
  # define a copy of the input dataframe to work with inside the function
  df <- df_input
  
  # find the maximum nr. of columns that stringsplit by some specified split operator
  # yields and apply str_split_fixed to all location columns
  max_cols <- 0
  column = ""
  
  for(col in c(1:ncol(df))) {
    cols <- ncol(str_split_fixed(df[,col], str_split_operator, n = Inf))
    print(cols)
    if(cols > max_cols){
      max_cols = cols
      column = col
    }
  }
  
  cat(c("Nr. of Columns: ", max_cols,
        "\nColumn Index: ", column,
        "\nColumn Name: ", colnames(df[column])))
  

  # apply str_split_fixed to all columns with max_cols as n
  location_columns_split <- list()
  
  for(col in c(1:ncol(df))) {
    df_temp <- str_split_fixed(df[,col], str_split_operator, n = max_cols)
    col_name <- colnames(df[col])
    location_columns_split[col_name] <- list(df_temp)
  }
  
  # create dataframe which will store the output
  output <- data.frame(matrix(ncol = ncol(df), nrow = 0))
  colnames(output) <- colnames(df)
  
  for(row in (1:nrow(location_columns_split[[1]]))){ #iterate through rows
    
    #initiate row df
    row_df <- data.frame(matrix(ncol = ncol(df), nrow = 0))
    
    #the iteration over the columns has to be repeated until all locations have been extracted
    #this is ensured by the "runs" variable, which keeps track of how many runs are needed per row
    runs <- 1
    
    while(runs == 1){
      runs = runs - 1
      #print(runs)
      print(c("Row", row))
      
      #initiate iter df
      iter_df <- data.frame(matrix(ncol = ncol(df), nrow = 1))
      colnames(iter_df) <- colnames(df)
      iter_df[1,1] <- df[row,1]
      
      for(col in seq(length(location_columns_split),1,-1)){ #iterate through columns from right to left (location_municipality to country)
        # call function on this row and column 
        max_col_temp <- find_last_element(location_columns_split, row, col, max_cols)
        
        # get location item
        loc_item <- location_columns_split[[col]][row,max_col_temp]
        
        # store it in the iter_df
        iter_df[1,col] <- loc_item
        
        # if max_col_temp is larger than 1, replace location item with "" (as it already was used)
        # this also means that the iteration must happen again for this row
        if(max_col_temp > 1) {
          location_columns_split[[col]][row,max_col_temp] <- ""
          runs <- 1
        }
        
      }
      
      # after one run in the row, append iter_df to row_df
      output <- rbind(output, iter_df)

    }
    
  }
  return(output)
}
