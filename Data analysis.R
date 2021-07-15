#To ascertain whether valley features (width or length) can be used to get precise forecasts of catchment area.
#Data: fjords
#Author: David

setwd("C:/Users/david/GitHub/Whether_valley_features _can_be_used_ for_forecasts_of_catchment_area") #let R pointing to the right place
fjords <- read.csv("fjords.csv", stringsAsFactors = TRUE) #read the csv file and assign it to fjords

#Analysis tasks1
#Visualise how area changes with length and width for each region.

#seperate the dataset by the fjords in each region (New Zealand (NZ) & British Columbia (BC))
NZ_fjords_df <- subset(fjords, region=="NZ")
BC_fjords_df <- subset(fjords, region=="BC")

#NZ length/width vs area
NZ_fjords_df <- subset(NZ_fjords_df, width < 2.5) #ignore the outliers 
plot(area ~ length, data = NZ_fjords_df, xlab = "Length (km)", ylab = "Area (km?)", main = "Comparison of Area and Length for the fjords in New Zealand")
plot(area ~ width, data = NZ_fjords_df, xlab = "Width (km)", ylab = "Area (km?)", main = "Comparison of Width and Length for the fjords in New Zealand")

#BC length/width vs area
plot(area ~ length, data = BC_fjords_df, xlab = "Length (km)", ylab = "Area (km?)", main = "Comparison of Area and Length for the fjords in British Columbia")
plot(area ~ width, data = BC_fjords_df, xlab = "Width (km)", ylab = "Area (km?)", main = "Comparison of Width and Length for the fjords in British Columbia")

#Analysis tasks2
#Conduct a simple linear regression (lr) for each region and valley characteristic separately.
#Are the same variables (width and/or length) significant in the models for BC and NZ?
#Note that you are only expected to do a simple linear regression so do not fit models 
#that use both length and width as predictor variables.

layout(matrix(c(1, 2, 3, 4), 2, 2))


#Analysis tasks4
#Based on the original plots, and also on the residual diagnostics from the simple models, 
#investigate models that use transformations of the variables and/or polynomial models.
#Which transformations do you suggest (if any) and is the transformation the same for both regions?
library(MASS)
boxcox(NZ_area_length_lr)
boxcox(NZ_area_width_lr)
boxcox(BC_area_length_lr)
boxcox(BC_area_width_lr)

#lambda = 0 is the best for NZ_area_length_lr?

#NZ simple linear regression
NZ_area_length_lr <- lm(area ~ length, data = NZ_fjords_df)
summary(NZ_area_length_lr)
plot(NZ_area_length_lr)

NZ_area_width_lr <- lm(area ~ width, data = NZ_fjords_df)
summary(NZ_area_width_lr)
plot(NZ_area_width_lr)

#NZ log plot for Area with length
NZ_log_area_length_lr <- lm(log(area) ~ length, NZ_fjords_df)
plot(area ~ length, data = NZ_fjords_df, xlab = "Length (km)", ylab = "Area (km?)", main = "Comparison of Area and Length for the fjords in New Zealand")
abline(NZ_log_area_length_lr, col = "blue")
summary(NZ_log_area_length_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(NZ_log_area_length_lr)

#NZ log of log plot for Area and length
NZ_log_area_length_lr <- lm(log(area) ~ log(length), NZ_fjords_df)
plot(area ~ length, data = NZ_fjords_df, xlab = "Length (km)", ylab = "Area (km?)", main = "Comparison of Area and Length for the fjords in New Zealand")
abline(NZ_log_area_length_lr, col = "blue")
summary(NZ_log_area_length_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(NZ_log_area_length_lr)

#NZ log plot for Area with Width
NZ_log_area_width_lr <- lm(log(area) ~ width, NZ_fjords_df)
plot(area ~ width, data = NZ_fjords_df, xlab = "Width (km)", ylab = "Area (km?)", main = "Comparison of Area and Width for the fjords in New Zealand")
abline(NZ_log_area_width_lr, col = "blue")
summary(NZ_log_area_width_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(NZ_log_area_width_lr)

#NZ log of log plot for Area and Width
NZ_log_area_width_lr <- lm(log(area) ~ log(width), NZ_fjords_df)
plot(area ~ width, data = NZ_fjords_df, xlab = "Width (km)", ylab = "Area (km?)", main = "Comparison of Area and Width for the fjords in New Zealand")
abline(NZ_log_area_width_lr, col = "blue")
summary(NZ_log_area_width_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(NZ_log_area_width_lr)

#NZ log plot for length
NZ_log_area_length_lr <- lm(area ~ log(length), NZ_fjords_df)
plot(area ~ length, data = NZ_fjords_df, xlab = "Length (km)", ylab = "Area (km?)", main = "Comparison of Area and Length for the fjords in New Zealand")
abline(NZ_log_area_length_lr, col = "blue")
summary(NZ_log_area_length_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(NZ_log_area_length_lr)

#NZ log plot for Width
NZ_log_area_width_lr <- lm(area ~ log(width), NZ_fjords_df)
plot(area ~ width, data = NZ_fjords_df, xlab = "Width (km)", ylab = "Area (km?)", main = "Comparison of Area and Width for the fjords in New Zealand")
abline(NZ_log_area_width_lr, col = "blue")
summary(NZ_log_area_width_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(NZ_log_area_width_lr)

#lambda = 0 is the best for BC_area_length_lr?

#BC simple linear regression
BC_area_length_lr <- lm(area ~ length, data = BC_fjords_df)
summary(BC_area_length_lr)
plot(BC_area_length_lr)

BC_area_width_lr <- lm(area ~ width, data = BC_fjords_df)
summary(BC_area_width_lr)
plot(BC_area_width_lr)

#BC log plot for Area with Length
BC_log_area_length_lr <- lm(log(area) ~ length, BC_fjords_df)
plot(area ~ length, data = BC_fjords_df, xlab = "Length (km)", ylab = "Area (km?)", main = "Comparison of Area and Length for the fjords in British Columbia")
abline(BC_log_area_length_lr, col = "blue")
summary(BC_log_area_length_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(BC_log_area_length_lr)

#BC log of log plot for Area and Length
BC_log_area_length_lr <- lm(log(area) ~ log(length), BC_fjords_df)
plot(area ~ length, data = BC_fjords_df, xlab = "Length (km)", ylab = "Area (km?)", main = "Comparison of Area and Length for the fjords in British Columbia")
abline(BC_log_area_length_lr, col = "blue")
summary(BC_log_area_length_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(BC_log_area_length_lr)

#BC log plot for Area with Width
BC_log_area_width_lr <- lm(log(area) ~ width, BC_fjords_df)
plot(area ~ width, data = BC_fjords_df, xlab = "Width (km)", ylab = "Area (km?)", main = "Comparison of Area and Width for the fjords in British Columbia")
abline(BC_log_area_width_lr, col = "blue")
summary(BC_log_area_width_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(BC_log_area_width_lr)

#BC log of log plot for Area and Width
BC_log_area_width_lr <- lm(log(area) ~ log(width), BC_fjords_df)
plot(area ~ width, data = BC_fjords_df, xlab = "Width (km)", ylab = "Area (km?)", main = "Comparison of Area and Width for the fjords in British Columbia")
abline(BC_log_area_width_lr, col = "blue")
summary(BC_log_area_width_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(BC_log_area_width_lr)

#BC log plot for length
BC_log_area_length_lr <- lm(area ~ log(length), BC_fjords_df)
plot(area ~ length, data = BC_fjords_df, xlab = "Length (km)", ylab = "Area (km?)", main = "Comparison of Area and Length for the fjords in British Columbia")
abline(BC_log_area_length_lr, col = "blue")
summary(BC_log_area_length_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(BC_log_area_length_lr)

#BC log plot for Width
BC_log_area_width_lr <- lm(area ~ log(width), BC_fjords_df)
plot(area ~ width, data = BC_fjords_df, xlab = "Width (km)", ylab = "Area (km?)", main = "Comparison of Area and Width for the fjords in British Columbia")
abline(BC_log_area_width_lr, col = "blue")
summary(BC_log_area_width_lr)
layout(matrix(c(1, 2, 3, 4), 2, 2))
plot(BC_log_area_width_lr)

#Analysis tasks5
#Propose a "best" model for each region. 
#How well does this model fit? How good is its predictive accuracy?
