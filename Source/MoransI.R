setwd("/Users/anc/Documents/Science/Code/Projeto_IC/Rmd") 
library("spdep")



moran.hypothesis = function(dist.matrix, valuevector, nsim = 999, alternative = "greater"){
  # Order Matrix Alphabetically
  dist.matrix = dist.matrix[order(rownames(dist.matrix)), 
                            order(colnames(dist.matrix))]
  
  # Inverse Distance Matrix
  inv.dist.matrix = 1/dist.matrix
  # Change diagonals from inf to 0
  diag(inv.dist.matrix) = 0
  
  # Convert and Standardized
  lw = mat2listw(inv.dist.matrix, style = "W")
  
  # Check the Moran I value
  I = moran(valuevector, listw =  lw, n = length(valuevector), S0 = Szero(lw))[1]
  
  # Perform an hypothesis test
  an_h = moran.test(valuevector, lw, alternative = alternative)
  an_mc = moran.mc(valuevector, lw, nsim = nsim, alternative = alternative)
  
  result = list(I, an_h, an_mc)
  return(result)
} 


# Import data
dist = as.matrix(read.csv("Data/distance_matrix.csv", header=T, dec= ".", row.names = 1, check.names = FALSE))

### Prey
# Limit to sites with metric available 
dist_py = dist[rownames(dist) %in% rownames(py_sh_df),
      colnames(dist) %in% rownames(py_sh_df)]

# All sites
py_sh_moran = moran.hypothesis(dist.matrix = dist_py, valuevector = py_sh_df$sh)
py_abund_moran = moran.hypothesis(dist.matrix = dist_py, valuevector = py_abund_df$abund)

# Only Atlantic Forest
dist_py_m = dist_py[grep("M",rownames(dist_py)), 
                    grep("M",colnames(dist_py)) ]

py_sh_moran = moran.hypothesis(dist.matrix = dist_py_m, 
                               valuevector = py_sh_df[grep("M",rownames(py_sh_df)),]$sh, 
                               )
py_abund_moran = moran.hypothesis(dist.matrix = dist_py_m, 
                                  valuevector = py_abund_df[py_abund_df[["Env"]] == "Atlantic Forest", ]$abund, 
                                  )

# Only Eucalyptus
dist_py_e = dist_py[grep("E",rownames(dist_py)), 
                    grep("E",colnames(dist_py)) ]

py_sh_moran = moran.hypothesis(dist.matrix = dist_py_m, 
                               valuevector = py_sh_df[grep("E",rownames(py_sh_df)),]$sh, 
                               )
py_abund_moran = moran.hypothesis(dist.matrix = dist_py_m, 
                                  valuevector = py_abund_df[py_abund_df[["Env"]] == "Eucalyptus", ]$abund, 
                                  )

### Anuran
# Limit to sites with metric available 
dist_an = dist[rownames(dist) %in% rownames(an_sh_df),
               colnames(dist) %in% rownames(an_sh_df)]

# All sites
an_sh_moran = moran.hypothesis(dist.matrix = dist_an, valuevector = an_sh_df$sh)
an_abund_moran = moran.hypothesis(dist.matrix = dist_an, valuevector = an_abund_df$abund)

# Only Atlantic Forest
dist_an_m = dist_an[grep("M",rownames(dist_an)), 
                    grep("M",colnames(dist_an)) ]

an_sh_moran_af = moran.hypothesis(dist.matrix = dist_an_m, 
                               valuevector = an_sh_df[grep("M",rownames(an_sh_df)),]$sh, 
                               )

# Only one Significant
an_abund_moran_af = moran.hypothesis(dist.matrix = dist_an_m, 
                                  valuevector = an_abund_df[grep("M",rownames(an_abund_df)),]$abund 
                                  )
# Only one Significant


# Only Eucalyptus
dist_an_e = dist_an[grep("E",rownames(dist_an)), 
                    grep("E",colnames(dist_an)) ]

an_sh_moran_eu = moran.hypothesis(dist.matrix = dist_an_e, 
                               valuevector = an_sh_df[grep("E",rownames(an_sh_df)),]$sh, 
                               )


an_abund_moran_eu = moran.hypothesis(dist.matrix = dist_an_e, 
                                  valuevector = an_abund_df[grep("E",rownames(an_abund_df)),]$abund 
                                  )

