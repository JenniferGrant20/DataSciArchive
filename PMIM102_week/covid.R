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
  
  covid_data = read.csv("WHO-COVID-19-global-data.csv")
  
    # 2. Process the data to get the values you need ...
    #    ... the latest data?
  
  date = max(covid_data$Date_reported)
  
    # 3. Tidy any problems with the data.
  
  covid_data = filter(covid_data, Date_reported == date)
  covid_data = select(covid_data, name = Country, new_cases = New_cases)
  
    # 4. Join the Covid-19 data with the map data.
  
  covid_data = merge(covid_data, map_data)
  
    # 5. I would suggest you also create the rate of Covid-19 cases and deaths.
  
  
  
    # 6. Return the finished data frame.
  
  return(covid_data)
  
}

