# Ecology and Evolution of Birds

setwd("/Users/sydneywhite/Documents/School/birds")
# install.packages("lubridate")
# install.packages("gamair")
# install.packages("gml")
# install.packages("lme4")
# install.packages("DHARMa")
# install.packages("MASS")

#library(gamair)
library(lubridate)
library(mgcv)
library(lme4)
library(DHARMa)
library(MASS)

### Data Processing
# Load Data
observations_raw <- read.csv("observations.csv")

# Format Data
observations_raw$Location <- as.factor(observations_raw$Location)
observations_raw$date <- as.Date(observations_raw$date, format = "%B %d, %Y") 
observations_raw$time <- hm(observations_raw$time)

observations <- observations_raw
rownames(observations) <- observations$flock_id
observations <- observations[observations$remove == FALSE, ] # remove data points 

# Remove unneccessary columns
observations$flock_id <- NULL 
observations$remove <- NULL
observations$datetime <- NULL
observations$temp <- NULL
observations$precipitation <- NULL
observations$wind <- NULL

# Summary of the Data
summary(observations)
pairs(observations[, c(-1, -2)]) # the negative 1 and 2 remove the first and second column before plotting

# x-axis (predictor/independent variable): ringed_plover_count
# y-axis (response/dependent): sanderling_count
# random effect: Location

### Fit a GLMM
# First try to fit the GLMM using the Poisson distribution since we're working with count data

# Poisson model
glmm_poisson <- glmer(
  sanderling_count ~ ringed_plover_count + (1 | Location),
  data = observations,
  family = poisson(link = "log"))
summary(glmm_poisson)
# Note the large dispersion of residuals
# Check the underlying assumptions of the GLMM by simulating residuals and reviewing the QQ plot
glmm_poisson_residuals <- simulateResiduals(glmm_poisson)
plot(glmm_poisson_residuals)
# Note that in the QQ plot (left graph) points should fit the red line
# Instead, we see a large deviation from the red line and thus try to fit the data using a negative binomial

# Negative binomial
glmm_nb <- glmer.nb(
  sanderling_count ~ ringed_plover_count + (1 | Location), #random intercept for each location
  data = observations)
summary(glmm_nb)
# Residuals are smaller compared to the poisson model
# Check the underlying assumptions of the GLMM by simulating residuals and reviewing the QQ plot
glmm_nb_residuals <- simulateResiduals(glmm_nb)
plot(glmm_nb_residuals)
# The data in the QQ plot now matches the straight line and thus we choose the Negative Binomial (it satisfies underlying assumptions while Poisson does not)

### Fit a null model
# Null model (used for comparison)
nb_null <- glmer.nb(
  sanderling_count ~ 1 + (1 | Location), 
  data = observations)
summary(nb_null)

# plot(observations$ringed_plover_count,observations$sanderling_count)
pred <- data.frame(ringed_plover_count=seq(min(observations$ringed_plover_count), max(observations$ringed_plover_count), by = 1))
pred$glmm_nb <- predict(glmm_nb, newdata = pred, type = "response", re.form = NA)
pred$null <- predict(nb_null, newdata = pred, type = "response", re.form = NA)
# lines(pred$ringed_plover_count, pred$glmm_nb, lwd = 2)
# lines(pred$ringed_plover_count, pred$null, lwd = 2)

library(ggplot2)
ggplot(observations, aes(x = ringed_plover_count, y = sanderling_count)) +
  geom_point(alpha = 0.5) +
  geom_line(data = pred, aes(x = ringed_plover_count, y = glmm_nb, color = "Full Model")) +
  geom_line(data = pred, aes(x = ringed_plover_count, y = null, color = "Null Model")) +
  scale_color_manual(
    name = "Models",
    values = c("Full Model" = "blue", "Null Model" = "orange")) + 
  theme_minimal(base_size = 18) + 
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold")) +
  labs(title = "Observed data with Fitted Model Predictions",
       x = "Ringed Plover Count",
       y = "Sanderling Count")

# Likelihood-Ratio-Test to check if our model is significantly better than the null model
anova(nb_null, glmm_nb, test = "LRT")
# p-value tells, if we can reject the null model (H0: Null model is the true model)
# p-value = 0.869  -> Can't reject the null hypothesis and we should thus stick with the null model

