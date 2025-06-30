# SPC plotting

# libraries
library(tidyverse)
library(lubridate)

#set working directory
setwd("~/Documents/R-Repositories/YaharaCl")

# load master file
yahara_spc = read_csv('data/Yahara_Buoys_master_20250527.csv') |> 
  mutate(year = year(date_time)#, 
         #depths = as.numeric(depths)
         ) |> 
  filter(spc > 200)

# plot spc
ggplot(yahara_spc, aes(date_time, spc, color = depths)) + 
  geom_path() + 
  facet_wrap(vars(site), scales = "free") +
  theme_bw()


# plot temperature
ggplot(yahara_spc, aes(date_time, temp_c, color = depths)) + 
  geom_path() + 
  facet_wrap(vars(site)) + 
  theme_bw()
