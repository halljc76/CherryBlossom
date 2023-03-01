# setwd("./final") # Set if wd not the 'final' subdirectory 
source("./dc.R")
source("./liestal.R")
source("./vanc.R")
source("./kyoto.R")

files <- c("liestal_preds.csv", "dc_preds.csv", "vanc_preds.csv")
kyoto <- read.csv("kyoto_preds.csv")
for (f in files) {
  kyoto <- kyoto %>% left_join(read.csv(f), by = c("year"))
}

write.csv(kyoto,"../cherry-predictions.csv",row.names = F)
