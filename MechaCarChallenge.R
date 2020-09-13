library(tidyverse) 
library(ggpubr)
library(dplyr)

# Import data
mpg_table <- read.csv(file='MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
susp_table <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F)

# Review data in mpg_table
head(mpg_table)

# Create matrix to inspect potential correlation
mpg_matrix <- as.matrix(mpg_table[,c("vehicle_length", "vehicle_weight", "spoiler_angle", "ground_clearance", "AWD","mpg")])
cor(mpg_matrix)

mpg_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,mpg_table)
mpg_lm
summary(mpg_lm)


# Suspension
susp_summary_by_lot <-  susp_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI = mean(PSI), Median_PSI = median(PSI), Variance = var(PSI), Standard_Deviation = sd(PSI))
susp_summary_by_lot

susp_summary_all_data <-  susp_table %>% summarize(Mean_PSI = mean(PSI), Median_PSI = median(PSI), Variance = var(PSI), Standard_Deviation = sd(PSI))
susp_summary_all_data

#t-test
t.test(susp_table$PSI,mu=1500)

#t-test on subsets
susp_lot1 <- subset(susp_table, Manufacturing_Lot == "Lot1")
susp_lot2 <- subset(susp_table, Manufacturing_Lot == 'Lot2')
susp_lot3 <- subset(susp_table, Manufacturing_Lot == "Lot3")

t.test(susp_lot1$PSI,mu=1500)
t.test(susp_lot2$PSI,mu=1500)
t.test(susp_lot3$PSI,mu=1500)


