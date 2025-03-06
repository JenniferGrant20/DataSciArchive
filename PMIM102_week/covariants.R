# ------------------------------------------------------------------------------
# covariants.R
# ------------------------------------------------------------------------------
# Covariants project.
# Create a map of the world with Covid-19 cases and variant distribution
# Main file.
# ------------------------------------------------------------------------------
# Pete Arnold
# 5 December 2021
# ------------------------------------------------------------------------------
library(tidyverse)      # For most things.

# ------------------------------------------------------------------------------
# Step 1: Background libraries etc.
# ------------------------------------------------------------------------------
# We will use the 'rnaturalearth' library to create a base dataset (names, codes
# and map polygons) to produce a m,ap of the world.
# See https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
# You will likely need to install the following packages:
# install.packages(c(
#    "cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf",
#    "rnaturalearth", "rnaturalearthdata", "rgeos"
#))
library(rnaturalearth)
library(rnaturalearthdata)
# 'ne_countries' provides 64 variables which is not easy to review, so we'll
# select the useful ones. We will need to make sure we have the correct country
# code(s) - i.e. the one(s) used in the Covid-19 data files.
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    select(name, iso_a2, geometry, pop_est)
# 
# We will plot this using ggplot's sf geom.
# https://ggplot2-book.org/maps.html
theme_set(theme_bw())
ggplot(data = world) +
geom_sf()
# Test it with a plot of population.
ggplot(data = world) +
geom_sf(aes(fill = pop_est)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Task 1: Install git if you need to.

# Task 2: Get a github account and get permission to my world repository.

# Task 3a: Get covid data for countries.
# https://covid19.who.int/WHO-COVID-19-global-data.csv
source('covid.R')
covid_world <- get_covid_data(world)

# Check that this is OK - produce a plot (as above) for the Covid cases and/or
# deaths.

# Task 3b: Get data about covid variants
# https://www.gisaid.org/hcov19-variants/
source('R/variants.R')
# Step 1: Add one variant at a time, e.g.
covid_variants <- covid_world
covid_variants <- add_variant(covid_variants, 'Count_A',
    'data/countrySubmissionCountAlpha.csv')
covid_variants <- add_variant(covid_variants, 'Count_G',
    'data/countrySubmissionCountGamma.csv')
covid_variants <- add_variant(covid_variants, 'Count_D',
    'data/countrySubmissionCountDelta.csv')
covid_variants <- add_variant(covid_variants, 'Count_M',
    'data/countrySubmissionCountMu.csv')
covid_variants <- add_variant(covid_variants, 'Count_O',
    'data/countrySubmissionCountOmicron.csv')

# Step 2: Add them all in one data structure.
variants <- c(
    'Count_A' = 'data/countrySubmissionCountAlpha.csv',
    'Count_G' = 'data/countrySubmissionCountGamma.csv',
    'Count_D' = 'data/countrySubmissionCountDelta.csv',
    'Count_M' = 'data/countrySubmissionCountMu.csv',
    'Count_O' = 'data/countrySubmissionCountOmicron.csv')
covid_variants <- add_variants(covid_world, variants)

# Task 3c: Create a map.
source('R/plot.R')
# For cases.
plot_cases <- create_map(covid_variants, 'Case_rate')
print(plot_cases)
# For the latest variant.
plot_omicron <- create_map(covid_variants, 'Count_O')
print(plot_omicron)

# Task 3d: Create a plot with six maps - cases, deaths, alpha, gamma, delta and
#         omicron.
source('R/layout.R')
layout <- layout_maps(covid_variants,
    c('Case_rate', 'Death_rate', 'Count_A', 'Count_G', 'Count_D', 'Count_O'),
    'Covid cases, deaths and variant distributions (totals for the pandemic)')
print(layout)




