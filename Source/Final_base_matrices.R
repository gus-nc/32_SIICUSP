###-----------------------------------------------------------------------------
### Produce the final matrices that are going to be used for analysis
###-----Set a Directory---------------------------------------------------------
setwd("C:/Users/nunes/Documentos/Code/Projeto_IC") # Laptop
setwd("D:/Drive/Other computers/Meu laptop/Code/Projeto_IC") # PC
setwd("/Users/anc/Library/CloudStorage/GoogleDrive-nunesaugustousp@usp.br/Other computers/Meu laptop/Code/Projeto_IC")
###
#Read all the interaction network matrices into a list
links_eu_vol = t(as.matrix(read.csv("Diet_E_Vol_Xincomp.csv", header = T, row.names = 1)))
links_mt_vol = t(as.matrix(read.csv("Diet_M_Vol_Xincomp.csv", header = T, row.names = 1)))
links_eu_freq = t(as.matrix(read.csv("Diet_E_Freq_Xincomp.csv", header = T, row.names = 1)))
links_mt_freq = t(as.matrix(read.csv("Diet_M_Freq_Xincomp.csv", header = T, row.names = 1)))

webs = list(links_eu_vol, links_mt_vol, links_eu_freq, links_mt_freq) # create a list w/ the matrices
envs = c("vol_eu", "vol_mt", "freq_eu", "freq_mt") # names of each env and type combination
names(webs) = envs # name the list

# Filter the Eucalyptus matrices
cols_to_remove_eu = which(colSums(webs$freq_eu) < 5) # Only for freq bc it reveals the discrete number of preys
# Min of 5 prey items p/ species
if (length(cols_to_remove_eu) > 0) { # Check if is a int(0)
  webs$vol_eu = webs$vol_eu[, -cols_to_remove_eu] # Remove columns
  webs$freq_eu = webs$freq_eu[, -cols_to_remove_eu] # Remove columns
}
rows_to_remove_eu = which(rowSums(webs$freq_eu) == 0) # Check if there are items without a predator
if (length(rows_to_remove_eu) > 0) { # Check if is a int(0)
  webs$vol_eu = webs$vol_eu[-rows_to_remove_eu,] # Remove lines
  webs$freq_eu = webs$freq_eu[-rows_to_remove_eu,] # Remove lines
}
# Filter the Atlantic Forest matrices
cols_to_remove_mt = which(colSums(webs$freq_mt) < 5) # Only for freq bc it reveals the discrete number of preys
# Min of 5 prey items
if (length(cols_to_remove_mt) > 0) { # Check if is a int(0)
  webs$vol_mt = webs$vol_mt[, -cols_to_remove_mt] # Remove columns
  webs$freq_mt = webs$freq_mt[, -cols_to_remove_mt] # Remove columns
}
rows_to_remove_mt = which(rowSums(webs$freq_mt) == 0) # Check if there are items without a predator
if (length(rows_to_remove_mt) > 0) { # Check if is a int(0)
  webs$vol_mt = webs$vol_mt[-rows_to_remove_mt,] # Remove lines
  webs$freq_mt = webs$freq_mt[-rows_to_remove_mt,] # Remove lines
}

unwanted_prey = c("Apenas substrato", "Arthropoda", "Semente", "Semente de capim", "Vazio", "Insecta", 
                  "Não identificado","Orthoptera", "Diptera", "Arachnida", "Pterygota", "Larva", "Hemiptera", "Mollusca")

for (env in envs) {
  webs[[env]] =  webs[[env]][!rownames(webs[[env]]) %in% unwanted_prey,]
}

###----- write the .csv files --------------------------------------------------
for (env in envs) {
  write.csv(webs[[env]], paste( "Data/", env, "_diet_matrix_final.csv", sep = ""), quote = FALSE, fileEncoding = "UTF-8")
}

