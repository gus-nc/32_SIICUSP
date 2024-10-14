### Function to import matrices in the appropriate format
# x and e must be the same length

import = function(x, e) {
  if (length(x) != length(e)) {
    stop("Both inputs must be the same length.")
  }
  webs = list()
  rep = 0
  for (i in x) {
    rep = rep + 1
    webs[[envs[rep]]] = t(as.matrix(read.csv(i, header = T, row.names = 1)))
  }
  return(webs)
}

### Function for executing an FDCA

fraction_analysis = function(x, fraction) {
  mat_fdca = list()
  mat_names = names(x)
  for (f in fraction) {
    rep = 0
    for (k in x) {
      k_mod = matrix(0, nrow = dim(k)[1], ncol = dim(k)[2], dimnames = list(c(rownames(k)),c(colnames(k))))
      rep = rep + 1
      
      # If I do this I loose the names information
      # k = apply(k, MARGIN = 2, function(x) sort(x, decreasing = TRUE))
      
      for (j in 1:dim(k)[2]) {
        data = k[,j] # Extract a column to analyze
        total_sum = sum(data)  # Calculate the total sum of the column
        threshold = f * total_sum # Determine the threshold for retaining values
        sorted_data = sort(data, decreasing = TRUE) # Sort the data in descending order
        
        # Initialize variables to store retained values and cumulative sum
        retained_values = c()
        cumulative_sum = 0
        
        # Iterate through the sorted data to retain values
        int_count = 0
        for (value in sorted_data) {
          int_count = int_count + 1
          if (cumulative_sum + value < threshold) {
            retained_values[names(sorted_data)[int_count]] = value
            cumulative_sum = cumulative_sum + value
          } 
          else { # Iterate one time to include thye category tha surpass the treshold
            retained_values[names(sorted_data)[int_count]] = value
            cumulative_sum = cumulative_sum + value
            break
          }
        }
        # Store only the retained_values in the new matrix
        for (i in 1:length(retained_values)) {
          k_mod[names(retained_values[i]),j] = retained_values[i] 
        }
        mat_fdca[[mat_names[rep]]][[as.character(f)]] = k_mod
      }
    }
  }
  # Return retained values and their sum
  return(mat_fdca)
}

### Function for calculating Nestedness NODF for 1 matrix
Nestedness_NODF = function(x) {
  
  NODFc_temp = 0 # Create a temporary object for NODFc
  for (i in 1:(ncol(x)-1)) { # for every col i:n-1
    for (j in (i+1):ncol(x)) { # and for every col i+1:n
      if(sum(x[,i]) <= sum(x[,j])) { # if F(ci) <= F(cj)
        N_paired = 0 # The Nested Value for this pair of cols
      }
      else {
        PO = 0 # number of 1's in ci = cj, if cj != 0?
        N_j = sum(x[,j] == 1) # Cells > 0 in cj
        for (r in 1:nrow(x)) { # for every row in t cols sum PO
          if (x[r,i] == 1 && x[r,j] == 1){
            PO = PO+1
          }
        }
        N_paired = PO/N_j # The Nested Value for this pair of cols
      }
      NODFc_temp = NODFc_temp + N_paired
    }
  }
  NODFc = NODFc_temp*100
  # NODFr values for rows
  NODFr_temp = 0 # Create a temporary object for NODFr
  for (i in 1:(nrow(x)-1)) { # for every col i:n-1
    for (j in (i+1):nrow(x)) { # and for every col i+1:n
      if(sum(x[i,]) <= sum(x[j,])) { # if F(ci) <= F(cj)
        N_paired = 0 # The Nested Value for this pair of cols
      }
      else {
        PO = 0 # number of 1's in ci = cj, if cj != 0?
        N_j = sum(x[j,] == 1)  #Cells > 0 in cj
        for (c in 1:ncol(x)) { # for every row in t cols sum PO
          if (x[i,c] == 1 && x[j,c] == 1){
            PO = PO+1
          }
        }
        N_paired = PO/N_j # The Nested Value for this pair of cols
      }
      NODFr_temp= NODFr_temp + N_paired
    }
  }
  NODFr = NODFr_temp*100
  # NODF
  m = dim(x)[1] # Number of rows
  n = dim(x)[2] # Number of cols
  NODF_obs = 2*(NODFc+NODFr)/ # The final average of NODF for the env matrix
    (m*(m-1)+(n*(n-1)))
  return(c(NODFc, NODFr, NODF_obs))
}
