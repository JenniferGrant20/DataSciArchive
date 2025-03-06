# ------------------------------------------------------------------------------
# layout.R
# ------------------------------------------------------------------------------
# Covariants project.
# Plot several maps of the world with covid data choropleths.
# ------------------------------------------------------------------------------
# Pete Arnold
# 5 December 2021
# ------------------------------------------------------------------------------
library(patchwork)

# Plot a set of maps of the world for the data specified.
# Parameters:
# @param    df                  data-frame with country name, code and map data.
# @param    list_of_variables   vector containing the names of the variables to
#                               plot.
# @param    title               string containing the title text for the page.
# Return value:
# @return                       The ggplot object containing the maps.
# Typical usage:
# print(layout_maps(covid_variants,
#    c('Case_rate', 'Death_rate', 'Count_A', 'Count_G', 'Count_D', 'Count_O'),
#    'Covid cases, deaths and variant distributions (totals for the pandemic)'))
layout_maps <- function(df, list_of_variables, title){
    # Write some code to:
    # 1. Plot maps, one for each of the specified variables.
    # NB You may need to create the first map explicitly before you can add the
    #    remainder of the list (just a technicality).
    # 2. Add the layout and titles.
    # 3. Return the plots.
}

