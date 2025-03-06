# git module

library(tidyverse)

# Background: Create a base dataset and blank map of the world.
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
#install.packages(c(
#    "cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf",
#    "rnaturalearth", "rnaturalearthdata", "rgeos"
#))
library(rnaturalearth)
library(rnaturalearthdata)
# ne_countries provides 64 variables which is not easy to review, so we'll
# select the useful ones.
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    select(name, iso_a2, geometry, pop_est)
# Determine the basic form of the code for the output.
theme_set(theme_bw())
ggplot(data = world) +
geom_sf()
# Test is with a plot of population.
ggplot(data = world) +
geom_sf(aes(fill = pop_est)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Task 1: Install git if you need to.

# Task 2: Get a github account and get permission to my world repository.

# Task 3a: Get covid data for countries.
# https://covid19.who.int/WHO-COVID-19-global-data.csv

covid <- read_csv('data/WHO-COVID-19-global-data.csv')

covid_latest <- covid %>%
    group_by(Country_code) %>% filter(row_number()==n()) %>% ungroup()

covid_latest <- covid_latest %>%
    mutate(Country=ifelse(Country=='The United Kingdom', 'United Kingdom', Country))

covid_world <- world %>%
    left_join(covid_latest, by=c('iso_a2' = 'Country_code'))

covid_world <- covid_world %>%
    mutate(Case_rate=Cumulative_cases/pop_est,
        Death_rate=Cumulative_deaths/pop_est)

ggplot(data =covid_world) +
geom_sf(aes(fill = Death_rate)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Task 3b: Get data about covid variants
# https://www.gisaid.org/hcov19-variants/

alpha <- read_csv('data/countrySubmissionCountAlpha.csv')
gamma <- read_csv('data/countrySubmissionCountGamma.csv')
delta <- read_csv('data/countrySubmissionCountDelta.csv')
mu <- read_csv('data/countrySubmissionCountMu.csv')
omicron <- read_csv('data/countrySubmissionCountOmicron.csv')

alpha <- alpha %>% select(Country, Count_A=`Total #Alpha 202012/01 GRY (B.1.1.7+Q.*)`) %>%
    mutate(Country=ifelse(Country=='USA', 'United States of America', Country))
gamma <- gamma %>% select(Country, Count_G=`Total #Gamma GR/501Y.V3 (P.1+P.1.*)`) %>%
    mutate(Country=ifelse(Country=='USA', 'United States of America', Country))
delta <- delta %>% select(Country, Count_D=`Total #Delta GK (B.1.617.2+AY.*)`) %>%
    mutate(Country=ifelse(Country=='USA', 'United States of America', Country))
mu <- mu %>% select(Country, Count_M=`Total #Mu GH (B.1.621+B.1.621.1)`) %>%
    mutate(Country=ifelse(Country=='USA', 'United States of America', Country))
omicron <- omicron %>% select(Country, Count_O=`Total #GR/484A (B.1.1.529)`) %>%
    mutate(Country=ifelse(Country=='USA', 'United States of America', Country))

covid_variants <- covid_world %>%
    left_join(alpha, by='Country') %>%
    left_join(gamma, by='Country') %>%
    left_join(delta, by='Country') %>%
    left_join(mu, by='Country') %>%
    left_join(omicron, by='Country')

# Plan for the function to do this.
covid_variants <- covid_world
variant_file <- 'data/countrySubmissionCountMu.csv'
variant_name <- 'Count_M'
variants_df <- covid_variants
variant <- read_csv(variant_file) %>%
    select(Country, 2) %>%
    mutate(Country=ifelse(Country=='USA', 'United States of America', Country))     
names(variant)[2] <- variant_name
covid_variants <- covid_variants %>%
    left_join(variant, by='Country')

# A function to do this.
add_variant <- function(df, variant_name, file_name){
    variant <- read_csv(file_name) %>%
        select(Country, 2) %>%
        mutate(Country=ifelse(Country=='USA', 'United States of America', Country))     
    names(variant)[2] <- variant_name
    df <- df %>%
        left_join(variant, by='Country')
    return(df)
}
covid_variants <- covid_world
covid_variants <- add_variant(covid_variants, 'Count_M', 'data/countrySubmissionCountMu.csv')
covid_variants <- add_variant(covid_variants, 'Count_A', 'data/countrySubmissionCountAlpha.csv')
covid_variants <- add_variant(covid_variants, 'Count_G', 'data/countrySubmissionCountGamma.csv')
covid_variants <- add_variant(covid_variants, 'Count_D', 'data/countrySubmissionCountDelta.csv')
covid_variants <- add_variant(covid_variants, 'Count_M', 'data/countrySubmissionCountOmicron.csv')

# A function to call all those functions.
add_variants <- function(df, variants){
    for (i in 1:length(variants)){
        df <- add_variant(df, names(variants)[i], variants[i])
    }
    return(df)
}

# And what we end up with.
variants <- c(
    'Count_A' = 'data/countrySubmissionCountAlpha.csv',
    'Count_G' = 'data/countrySubmissionCountGamma.csv',
    'Count_D' = 'data/countrySubmissionCountDelta.csv',
    'Count_M' = 'data/countrySubmissionCountMu.csv',
    'Count_O' = 'data/countrySubmissionCountOmicron.csv')
covid_variants <- add_variants(covid_world, variants)

# Task 3c: Create a map.
create_map <- function(df, variable){
    ggplot(data=df) +
    geom_sf(aes(fill=eval(sym(variable)))) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt", name=variable)
}
plot_cases <- create_map(covid_variants, 'Case_rate')
plot_cases

# Task 3d: Create a plot with six maps - cases, deaths, alpha, gamma, delta and
#         omicron.
library(patchwork)
plot_cases <- ggplot(data=covid_variants) +
geom_sf(aes(fill = Case_rate)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")
plot_deaths <- ggplot(data=covid_variants) +
geom_sf(aes(fill = Death_rate)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")
plot_a <- ggplot(data=covid_variants) +
geom_sf(aes(fill = Count_A)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")
plot_g <- ggplot(data=covid_variants) +
geom_sf(aes(fill = Count_G)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")
plot_d <- ggplot(data=covid_variants) +
geom_sf(aes(fill = Count_D)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")
plot_o <- ggplot(data=covid_variants) +
geom_sf(aes(fill = Count_O)) +
scale_fill_viridis_c(option = "plasma", trans = "sqrt")
plot <- plot_cases + plot_deaths + plot_a + plot_g + plot_d + plot_o +
    plot_layout(ncol=2)
print(plot)

layout_maps <- function(df){
    plot_cases <- create_map(df, 'Case_rate')
    plot_deaths <- create_map(df, 'Death_rate')
    plot_alpha <- create_map(df, 'Count_A')
    plot_gamma <- create_map(df, 'Count_G')
    plot_delta <- create_map(df, 'Count_D')
    plot_omicron <- create_map(df, 'Count_O')
    plot <- plot_cases + plot_deaths + plot_a + plot_g + plot_d + plot_o +
        plot_layout(ncol=2)
    return(plot)
}
print(layout_maps(covid_variants))

layout_maps <- function(df, list_of_variables, title){
    plots <- create_map(df, list_of_variables[1])
    for (i in 2:length(list_of_variables)){
        plots <- plots + create_map(df, list_of_variables[i])
    }
    plots <- plots + plot_layout(ncol=2) +
         plot_annotation(title=title)
    return(plots)
}
print(layout_maps(covid_variants,
    c('Case_rate', 'Death_rate', 'Count_A', 'Count_G', 'Count_D', 'Count_O'),
    'Covid cases, deaths and variant distributions (totals for the pandemic)'))




