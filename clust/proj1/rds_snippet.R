# the rds object is read into memory exactly the way it was written
# the boston object is a ggmap raster map object that will be ready to print

# read it in and assign to whatever object name you desire
boston <- readr::read_rds("clust/data/boston.rds")

# then it can be used to plot with ggmap
ggmap::ggmap(boston)

# if you library load tidyverse, readr, or ggmap then you do not need the library:: direct call
