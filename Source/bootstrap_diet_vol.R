# Create sampled diets from the total of prey items consumed by the community
# Respects total volume per prey 
# and frequency of items per predator

bootstrap.diet.vol = function(x_freq, pool_data, nsim = 10){
  library(dplyr)
  # Input x_freq as a matrix of frequency of items per interaction
  cs = colSums(x_freq)
  # Create an empty matrix as template for the array
  empty_m_model =  apply(x_freq, c(1, 2), function(x) 0)
  # Create an empty array
  empty_array = array(NA, dim = c(dim(x_freq)[1], dim(x_freq)[2], 0))
  
  for (n in 1:nsim) { # Repeat for a given number of simulations
    # available_prey is a 2 column data frame with classification of prey and volume measure
    available_prey = 1:dim(pool_data)[1]
    temp_m = empty_m_model
    
    for (sp in 1:length(cs)) { # Repeat for every original predator
      rns = sample(available_prey, cs[sp]) # Sample without replacement
      available_prey = available_prey[!available_prey %in% rns] # Remove sampled prey from the pool
      
      # Count for each predator the total volume consumed for each prey type
      rboot = pool_data[rns,] %>% group_by(Class.inferior) %>% 
        summarise(sum = sum(Ind.Volume))
      
      for (i in 1:dim(rboot)[1]) { # for each prey type drawn
        temp_m[rboot$Class.inferior[i],sp] = rboot$sum[i] # Store sum in a matrix
      }
    }
    
    var_name = paste0("sim_", n) # name for matrix of dim n
    assign(var_name, temp_m) 
    
    # Add the matrix to the array
    empty_array = array(c(empty_array, get(var_name)), 
                        dim = c(dim(x_freq)[1], 
                                dim(x_freq)[2],
                                n))
  }
  return(empty_array)
}
