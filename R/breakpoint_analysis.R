### breakpoint analysis
# using the Petit test

# set working directory
setwd("~/Documents/R-Repositories/YaharaCl")

#load libraries
library(tidyverse)
library(segmented)

# load file
master_list_orig <- read_csv("data/Yahara_Buoys_master_20250527.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  drop_na() |> 
  filter(depths == 2, 
         site == "Mendota")

#visualize values for normality
ggplot(master_list, aes(x = spc)) + 
  geom_histogram() + 
  theme_bw() + 
  facet_wrap(vars(site, depths), scales = "free")

# looks pretty normal to me, if you look at all the sites together. 
# when you break it up by site, looks a lot different. 

# Fit a linear model
spc_lm = lm(spc ~ date_numeric, data = master_list)

davies_results = davies.test(spc_lm)
davies_results

bp_numeric <- davies_results$statistic[[1]]
bp_date <- as.POSIXct(bp_numeric, origin = "1970-01-01")  # use POSIXct if your input is POSIXct

paste("Davies Test suspects the breakpoint is on", format(bp_date, "%Y-%m-%d"))

spc_lm = lm(spc ~ date_time, data = master_list)

spc_seg = segmented(obj = spc_lm, seg.Z = date_time)

# figured out why you can't use this on the entire data set, because the loggers have duplicate
# date data


# Load your dataset
spc_data <- master_list 


# Ensure relevant columns exist
spc_data <- spc_data |> 
  mutate(across(c(site, depths), as.factor))

# Group by site and depth
breakpoint_results <- spc_data |>
  group_by(site, depths) |>
  nest() |>
  mutate(
    # Fit initial linear model
    lm_fit = map(data, ~ lm(spc ~ date_numeric, data = .x
                            )),
    
    # Try fitting segmented model and find breakpoints
    seg_fit = map2(lm_fit, data, ~ {
      tryCatch(
        segmented(.x, seg.Z = ~date_numeric),
        error = function(e) NULL
      )
    }),
    
    
    # Extract breakpoints (if model succeeded)
    breakpoints = map(seg_fit, ~ {
      if (!is.null(.x)) {
        bp <- tryCatch(
          as.numeric(.x$psi[, "Est."]),
          error = function(e) NA
        )
        return(bp)
      } else {
        return(NA)
      }
    })
  )

# Unnest results to view
breakpoint_summary <- breakpoint_results |>
  dplyr::select(site, depths, breakpoints) |>
  unnest(cols = breakpoints)

print(breakpoint_summary)


