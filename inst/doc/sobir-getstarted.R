## ----setup, echo = F----------------------------------------------------------

knitr::opts_chunk$set(message = F, error = F)


## ----install github version, comment=F, eval=F--------------------------------
#  
#  library("devtools")
#  devtools::install_github("C4EcoSolutions/sobir")
#  

## ----Load libraries-----------------------------------------------------------

library(sobir)
library(tidyr)
library(dplyr)
library(ggplot2)


## ----Define simulated data----------------------------------------------------

set.seed(1)
dat_sim = tibble(x = rnorm(200, mean = 0, sd = 1),
             y = rnorm(200, mean = 0, sd = 1)) %>%
  mutate(bound = 2*x+2,
         beyond_bound = ifelse(bound < y, TRUE, FALSE))

# Define the points that are within and beyond the artificial boundary line
dat_beyond = dplyr::filter(dat_sim, beyond_bound == TRUE)
dat_within = dplyr::filter(dat_sim, beyond_bound == FALSE)


## ----Visualise simulated data, fig.width=5, fig.height=5----------------------

# Visualise the simulated data with the points beyond the boundary removed
dat_within %>%
  ggplot(aes(x = x, y = y)) +
  geom_abline(slope = 2, intercept = 2, linetype = 2, col = "grey") +
  geom_point() +
  geom_point(data = dat_beyond, shape = 1, col = "lightgrey") +
  geom_smooth(method = "lm", col = "blue", se = F) +
  annotate(geom = "text", x = -1.5, y = 1.5, label = "no-data\nzone") +
  lims(x = c(-3,3),
       y = c(-3,3)) +
  theme_bw() 


## ----Extract and visualise artificial boundary points-------------------------

# Extract boundary points (bpts object)
bpts_within = extract_bpts(dat_within$x, dat_within$y)

# Plot the boundaries
bpts_plot(bpts_within, xlab = "x", ylab = "y") 


## ----Run the analysis on artificial data--------------------------------------

# Run the permuation test
set.seed(1)
perm_within = perm_area(dat_within$x, dat_within$y, nsim = 100, boundary = "topl")

# Plot the results
perm_plot(perm_within, histogram = T)


## ----Run the analysis on random data------------------------------------------

# Run the permuation test
set.seed(1)
perm_random = perm_area(dat_sim$x, dat_sim$y, nsim = 100, boundary = "topl")

# Plot the results
perm_plot(perm_random, histogram = T)


## ----Import Sankaran data-----------------------------------------------------

data("WoodyAfrica",package = "sobir")


## ----Extract and visualise Sankaran boundary points---------------------------

# Extract boundary points (bpts object)
bpts_sankaran = extract_bpts(WoodyAfrica$MAP, WoodyAfrica$Cover)

# Plot the boundaries
bpts_plot(bpts_sankaran, xlab = "MAP (mm)", ylab = "Woody Cover (%)") 


## ----Run the Sankaran analysis------------------------------------------------

# Run the permuation test
set.seed(1)
perm_sankaran = perm_area(WoodyAfrica$MAP, WoodyAfrica$Cover, nsim = 100, boundary = "topl")

# Plot the results
perm_plot(perm_sankaran, histogram = T)


## ----Import Mills data--------------------------------------------------------

data("WoodyTowoomba", package = "sobir")


## ----Extract and visualise Mills boundary points------------------------------

WoodyTowoomba = WoodyTowoomba %>%
   mutate(MnCu = Mn/Cu)

# Extract boundary points (bpts object)
bpts_mills_mncu = extract_bpts(WoodyTowoomba$MnCu, WoodyTowoomba$TreeNum)

# Plot the boundaries
bpts_plot(bpts_mills_mncu, xlab = "Soil Mn/Cu", ylab = "Tree abundance") 


## ----Run the Mills analysis---------------------------------------------------

# Run the permuation tests
set.seed(1)
perm_mills_topl = perm_area(WoodyTowoomba$MnCu, WoodyTowoomba$TreeNum, nsim = 100, boundary = "topl")
perm_mills_botr = perm_area(WoodyTowoomba$MnCu, WoodyTowoomba$TreeNum, nsim = 100, boundary = "botr")

# Plot the results
perm_plot(perm_mills_topl, histogram = T)
perm_plot(perm_mills_botr, histogram = T)


