library(tidyverse) 
library(ggpubr)
library(dplyr)


# Import data
mpg_table <- read.csv(file='MechaCar_mpg.csv', check.names = F, stringsAsFactors = F)
susp_table <- read.csv(file='Suspension_Coil.csv', check.names = F, stringsAsFactors = F)


# Multiple linear regression analysis of fuel efficiency
# Review data in mpg_table
head(mpg_table)

# Create matrix to inspect potential correlation
mpg_matrix <- as.matrix(mpg_table[,c("vehicle_length", "vehicle_weight", "spoiler_angle", "ground_clearance", "AWD","mpg")])
cor(mpg_matrix)

# Multiple linear reression on all variables
mpg_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,mpg_table)
mpg_lm
summary(mpg_lm)

# Multiple linear regression on variables with slope not near zero and significan p-value
mpg_lm2 <- lm(mpg ~ vehicle_length + ground_clearance ,mpg_table)
mpg_lm2
summary(mpg_lm2)


# Suspension Summary Statistics and t-test
# Summary of all data
susp_summary_all_data <-  susp_table %>% summarize(Mean_PSI = mean(PSI), Median_PSI = median(PSI), Variance = var(PSI), Standard_Deviation = sd(PSI))
susp_summary_all_data

#Summary by Manufacturing_Lot
susp_summary_by_lot <-  susp_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI = mean(PSI), Median_PSI = median(PSI), Variance = var(PSI), Standard_Deviation = sd(PSI))
susp_summary_by_lot
 
#t-test
t.test(susp_table$PSI,mu=1500)

#t-test on subsets
susp_lot1 <- subset(susp_table, Manufacturing_Lot == "Lot1")
susp_lot2 <- subset(susp_table, Manufacturing_Lot == 'Lot2')
susp_lot3 <- subset(susp_table, Manufacturing_Lot == "Lot3")

t.test(susp_lot1$PSI,mu=1500)
t.test(susp_lot2$PSI,mu=1500)
t.test(susp_lot3$PSI,mu=1500)

#t-test on samples omitting Lot 3
susp_lot1_2_table <- subset(susp_table, Manufacturing_Lot != "Lot3")
susp_summary_lots_1_2 <- susp_lot1_2_table %>% summarize(Mean_PSI = mean(PSI), Median_PSI = median(PSI), Variance = var(PSI), Standard_Deviation = sd(PSI))
susp_summary_lots_1_2
t.test(susp_lot1_2$PSI,mu=1500)