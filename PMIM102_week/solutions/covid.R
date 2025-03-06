# ------------------------------------------------------------------------------
# covid.R
# ------------------------------------------------------------------------------
# Covariants project.
# Add cases/deaths data.
# ------------------------------------------------------------------------------
# Pete Arnold
# 5 December 2021
# ------------------------------------------------------------------------------

# Get the Covid-19 case and death data for countries around the world.
# Parameters:
# @param    map_data    data-frame with country name, code and map data.
# Return value:
# @return   covid_data  data-frame with map_data + covid case and death data.
get_covid_data <- function(map_data){
    # Write some code to:
    # 1. Read the Covid-19 case and death data.
    #    You may be able to do this directly by accessing the URL or you may
    #    need to download a CSV file and read that.
    # 2. Process the data to get the values you need ...
    #    ... the latest data?
    # 3. Tidy any problems with the data.
    # 4. Join the Covid-19 data with the map data.
    # 5. I would suggest you also create the rate of Covid-19 cases and deaths.
    # 6. Return the finished data frame.
    covid <- read_csv('data/WHO-COVID-19-global-data.csv')
    covid_latest <- covid %>%
        group_by(Country_code) %>% filter(row_number()==n()) %>% ungroup()
    covid_latest <- covid_latest %>%
        mutate(Country=ifelse(Country=='The United Kingdom',
            'United Kingdom', Country))
    covid_world <- map_data %>%
        left_join(covid_latest, by=c('iso_a2' = 'Country_code'))
    covid_world <- covid_world %>%
        mutate(Case_rate=Cumulative_cases/pop_est,
            Death_rate=Cumulative_deaths/pop_est)
    return(covid_world)
}

