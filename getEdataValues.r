getEdataValues <- function(
  x,
  namePattern  = "Duration"
  , valPattern = "^[1-9]\\d*$" # Non-zero integers
  , valRange   = NULL # a two tuple vector giving the minimum and maximum values
){
  # This function extracts desired values from the columns in a messy
  # edata-converted data frame

  # Select columns that match the name pattern
  nameInclIndex <- grep(namePattern, names(x))
  message(length(nameInclIndex), ' columns have been selected.')

  # Select columns that match the value pattern and range, for each row
  r <- sapply(seq(nrow(x)), function(currRow)
    {

      # Match value pattern
      valInclVal <- grep(valPattern, x[currRow, nameInclIndex], value = T)

      # Match value range
      if (!is.null(valRange))
      {
        indx <- (valInclVal>=valRange[1])&(valInclVal<=valRange[2])
        valInclVal <- valInclVal[indx]
      }

      # If none value matched
      if (length(valInclVal) == 0)

        # return NA
        valInclVal <- NA

      # If more than one values matched
      else if (length(valInclVal) > 1)

        # return them all, and throw warning
        message(length(valInclVal), " values encountered in row ", currRow, ", please check!")

      return(valInclVal)
    }
  )

  return(r)
}
