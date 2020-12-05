library(magrittr)
library(tidyverse)
library(kohonen)
library(RColorBrewer)
library(rpart.plot)
Zeo <- read.csv("ZeoliteDB.csv", header = TRUE)
data <- Zeo[,4:17]
map_dimension <- 10
n_iterations <- 2000
recalculate_som <- T
# convert vector classes into a class matrix
numerics <- summarise_all(data, is.numeric) %>% as.logical()
factors <- names(data) %>% .[!numerics]
mineral_list <- list()
distances <- vector()

for (fac in factors) {
  mineral_list[[fac]] <- kohonen::classvec2classmat(data[[fac]])
  distances <- c(distances, 'tanimoto')
}
# create som grid and save it as som_zeolite.Rdata
som_grid <- kohonen::somgrid(xdim = map_dimension, ydim = map_dimension, topo = "hexagonal")

if(recalculate_som == F & file.exists('som_zeolite.Rdata') == T){ 
  load('som_zeolite.Rdata')
} else{
  m <- kohonen::supersom(mineral_list, grid = som_grid, rlen = n_iterations, alpha = 0.05, whatmap = c(factors, 'numerics'),
                         dist.fcts = distances)
  save(m, file = 'som_zeolite.Rdata')
}
Changes <- as.data.frame(m$changes)
Changes$Iteration <- seq(1,n_iterations)
#complete code can be find in SOM_Zeolite_Code.Rmd RMarkdown file.