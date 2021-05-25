library(pacman)
p_load(ipumsr, tidyverse)

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("cps_00006.xml")
data <- read_ipums_micro(ddi)

data <- data %>% 
  as_factor()

saveRDS(data, "cps_data.rds")

