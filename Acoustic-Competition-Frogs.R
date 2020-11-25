# Acoustic Competition in Frogs
# author: Lara S. Burchardt
# collaborators: A. Filer, B. van Rendsburg (University of Queensland)
# project to compare call rates between WSF and ESF, when in competition


# 00: Load Packages --------------------------------------------------------------------------

library(tidyverse)

# 01: Load Data ------------------------------------------------------------------------------

# IOI Data

ioi <- read_delim("all_IOIs_WSF_ESF.csv", delim = ";")


#02: Stats -----------------------------------------------------------------------------------

# WSF iois: 1298
# ESF iois: 876
# WSF when ESF present iois: 847
# ESF when WSF present iois: 945 

# 03: Histograms -----------------------------------------------------------------------------

#quick and dirty histograms with 'hist'

hist(ioi$`WSF `)
hist(ioi$ESF)
hist(ioi$WSF_ESF_present)
hist(ioi$ESF_WSF_present)


