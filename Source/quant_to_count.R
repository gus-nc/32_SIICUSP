# Make a Data Frame with Quantity rows so that each row represents only one of that Quantity

quant.to.count = function(x, column){
  library(dplyr)
  # Check if Sum of data_filt is equal to what is seen in the used matrices
  ins = sum(x[[column]])
  for (i in 1:dim(x)[1]) {
    amount = x[[column]][i]
    # Check if the amount is greater then 1
    if (amount > 1){
      # Loop to bind a copy of this row to the data frame
      for (amo in 2:amount) {
        x = rbind(x, x[i,])
      }
    }
  }
  x = select(x,-column)
  # Check AGAIN if Sum of data_filt is equal to what is seen in the used matrices
  if (ins != dim(x)[1]) {
    cat(paste("Sum prior to change:", ins, "; Dimension after change", dim(x)[1]))
  }
  return(x)
}
