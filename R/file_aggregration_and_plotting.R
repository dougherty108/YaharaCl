##### cond buoy data plotting #########
# library
library(tidyverse)
library(lubridate)
library(zoo)
library(MetBrewer)

setwd("~/Documents/R-Repositories/yahara chain buoys")

# names list for naming plots and organizing
files <- list.files("Data/2023 Data")
sites <- str_extract(files, "[^_]+")
depth <- str_extract(files, "[:digit:][:punct:]*[:digit:]") 
mylist <- list()

setwd("~/Documents/R-Repositories/yahara chain buoys/Data/2023 Data")
for (i in 1:length(files)) {
  deep <- read_csv(files[[i]], skip = 1)
  colnames(deep)[2:4] <- c("date_time", "conductivity", "temp_c")
  deep <- deep[, 2:4]
  deep <- deep |> 
    mutate(date_time = mdy_hms(date_time), 
           spc = (conductivity/(1-((25-temp_c)*0.019))), 
           depths = depth[[i]], 
           site = sites[[i]]) 
  mylist[[i]] <- deep
}

df <- do.call("rbind",mylist) #|> #combine all vectors into a matrix
# filter(date_time < "2025-04-30 00:00:00") 

setwd("~/Documents/R-Repositories/yahara chain buoys")

master <- read_csv("Data/Yahara_Buoys_master_20250509.csv")

new_master <- rbind(master, df)

write_csv(new_master, "Data/Yahara_Buoys_master_2025052y.csv")


ggplot(new_master, aes(date_time, spc, color = depths)) + 
  geom_path() + 
  facet_wrap(vars(site))

ggplot(df, aes(date_time, spc, color = depths)) + 
  geom_line() + 
  ggtitle("SPC") + 
  facet_wrap(vars(site), scales = "free") + 
  theme_bw()


# filter out obviously high outliers
df2 <- df |> 
  mutate(roll_mean_spc = rollmean(spc, 
                                  k = 384, 
                                  fill = NA))

# plot up now that everything is loaded and formatted properly
ggplot(df2, aes(date_time, roll_mean_spc, color = depths)) + 
  geom_path(size = 0.75) + 
  ggtitle("rolling mean of SPC 2025",
          subtitle = "4 day rolling interval") + 
  facet_wrap(~site) + 
  #scale_color_brewer(palette = "Paired") + 
  theme_bw(base_size = 20)

###### load master buoy sheet
setwd("~/Documents/R-Repositories/yahara chain buoys")
master_sheet = read_csv('Data/Yahara_Buoys_master_20250509.csv')

item <- ggplot(master_sheet, aes(date_time, spc, color = depths)) + 
  geom_path() + 
  facet_wrap(~site) + 
  theme_bw()

ggsave("plots/all_data.png")



