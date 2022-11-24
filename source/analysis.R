library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#
#What year had the highest amount of black incarceration 
high_year <- incarceration_df %>%
  group_by(max(year)) %>% 
  filter(year > 0) %>% 
  summarise(high_black = n()) %>% 
  filter(high_black == max(high_black)) %>% 
  return(year)

#What state had the highest amount of black incarceration in 2018?
recent_state <- incarceration_df %>% 
  group_by(state, county_name, year, black_jail_pop) %>%
  filter(year == "2018") %>% 
  filter(black_jail_pop > 0) %>% 
  summarise(max(black_jail_pop, na.rm = TRUE)) %>% 
  return(state)

#What is the highest amount of incarceration of each race in 2018? 
avg_aapi <- incarceration_df %>% 
  filter(year == "2018") %>% 
  summarise(avg_aapi = mean(aapi_jail_pop, na.rm= TRUE))

avg_black <- incarceration_df %>% 
  filter(year == "2018") %>% 
  summarise(avg_black= mean(black_jail_pop, na.rm= TRUE))

avg_latinx <- incarceration_df %>% 
  filter(year == "2018") %>% 
  summarise(avg_latinx = mean(latinx_jail_pop, na.rm= TRUE))

avg_native <- incarceration_df %>% 
  filter(year == "2018") %>% 
  summarise(avg_native = mean(native_jail_pop, na.rm= TRUE))

avg_white <- incarceration_df %>% 
  filter(year == "2018") %>% 
  summarise(avg_white = mean(white_jail_pop, na.rm= TRUE))

avg_other <- incarceration_df %>% 
  filter(year == "2018") %>% 
  summarise(avg_other = mean(other_race_jail_pop, na.rm= TRUE))

avg_race <- data.frame(avg_aapi, avg_black, avg_latinx, avg_native, avg_white, avg_other)

# What is the highest amount of incarceration of each race in 2018?
max_black <- incarceration_df %>% 
  group_by(state, county_name, year, black_jail_pop) %>%
  filter(year == "2018") %>% 
  filter(black_jail_pop > 0) %>% 
  summarise(high_black = max(black_jail_pop, na.rm = TRUE)) %>% 
  select(high_black)

max_latinx <- incarceration_df %>% 
  group_by(state, county_name, year, latinx_jail_pop) %>%
  filter(year == "2018") %>% 
  filter(latinx_jail_pop > 0) %>% 
  summarise(high_latinx = max(latinx_jail_pop, na.rm = TRUE)) %>% 
  select(high_latinx)

max_native <- incarceration_df %>% 
  group_by(state, county_name, year, native_jail_pop) %>%
  filter(year == "2018") %>% 
  filter(native_jail_pop > 0) %>% 
  summarise(high_native = max(native_jail_pop, na.rm = TRUE)) %>% 
  select(high_native)

max_white <- incarceration_df %>% 
  group_by(state, county_name, year, white_jail_pop) %>%
  filter(year == "2018") %>% 
  filter(white_jail_pop > 0) %>% 
  summarise(high_white = max(white_jail_pop, na.rm = TRUE)) %>% 
  select(high_white)

max_aapi <- incarceration_df %>% 
  group_by(state, county_name, year, aapi_jail_pop) %>%
  filter(year == "2018") %>% 
  filter(aapi_jail_pop > 0) %>% 
  summarise(high_aapi = max(aapi_jail_pop, na.rm = TRUE)) %>% 
  select(high_aapi)

max_other <- incarceration_df %>% 
  group_by(state, county_name, year, other_race_jail_pop) %>%
  filter(year == "2018") %>% 
  filter(other_race_jail_pop> 0) %>% 
  summarise(high_other = max(other_race_jail_pop, na.rm = TRUE)) %>% 
  select(high_other)


# Comparing the amount of black incarcerated populations and the amount of dcrp throughout time 
black_dcrp_comp <- function(){
  death_comp <- incarceration_df %>% 
    group_by(state, year, total_jail_pop_dcrp, black_jail_pop) %>% 
    filter(year > "2000") %>% 
    summarise(number_black_jail = sum(black_jail_pop, na.rm = TRUE), .groups = NULL) %>% 
    summarise(number_black_dcrp = sum(total_jail_pop_dcrp, na.rm = TRUE), .groups = NULL) %>% 
    return(death_comp)
}
black_dcrp_data <- black_dcrp_comp()

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_prison_pop <- function() {
  prison_pop_year <- incarceration_df %>% 
    group_by(year) %>% 
    summarise(prison_population = sum(total_prison_pop, na.rm= TRUE))
return(prison_pop_year)   
}


# This function ... <todo:  update comment>
plot_prison_pop_for_us <- function()  {
  plot_prison_pop <- ggplot(get_year_prison_pop()) + 
    geom_col(mapping = aes(year, prison_population)) +
    labs(
      x= "Year",
      y= "Total Jail Population",
      title= "Growth of U.S Prison Population"
    )
  return(plot_prison_pop)   
} 
plot <- plot_prison_pop_for_us()
ggplotly(plot)



## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
get_jail_pop_by_states <- function(state) {
  jail_pop_states <- incarceration_df %>% 
    group_by(state,year) %>% 
    filter(state %in% state) %>% 
    summarise(total_population = sum(total_jail_pop, na.rm= TRUE), .groups = NULL) %>% 
  return(jail_pop_states) 
}
incarceration_df_pop_by_states <- get_jail_pop_by_states()

plot_jail_pop_by_states <-function(state) {
  plot_pop <- ggplot(get_jail_pop_by_states(state)) +
    geom_line(mapping = aes(x= year, y= total_population, color= state)) +
  labs(
    x= "Year",
    y= "Total Prison Population",
    title= "Growth of Jail Population in Each State (1970-2018)"
  )
  return(plot_pop)
}
plot2 <- plot_jail_pop_by_states(state) 
ggplotly(plot2)

# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Male and Female jail population comparison

# Data
gender_comp <- function() {
  gender_compare <- incarceration_df %>%
    group_by(state, year, female_jail_pop, male_jail_pop) %>% 
    filter(state %in% state) %>% 
    filter(year > "2010") %>% 
    summarise(female_jail_data = sum(female_jail_pop, na.rm= TRUE),.groups= NULL) %>% 
    summarise(male_jail_data = sum(male_jail_pop, na.rm = TRUE), .groups = NULL) %>% 
  return(gender_compare)
}
comparing_gender <- gender_comp()

# Plot
gender_plot <- function() {
  gender_comp_plot <- ggplot(gender_comp()) +
    geom_point(mapping = aes(x= male_jail_data, y = female_jail_pop, color = state ))+
    labs(
      x = "Population of Males in Jail",
      y = "Population of Females in Jail", 
      title= "Populations of Genders in Jail in each State Between 2010-2018"
    )
  return(gender_comp_plot)
}

plot3 <- gender_plot()
plot3
# Paragraph 
# When analyzing the differences in gender incarceration in each state, we can 
# observe that there is an overall higher population of people who identify as male who
# who are incarcerated compared to females. The highest value being 15000 males 
# incarcerated in one state compared to 2000 females incarcerated in one state. 
# We can also see that in states such as Arizona, is where there is the highest 
# populations of incarcerated males and females. 
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
#Number of people detained by ICE in 2018 by state 
ice_2018 <- incarceration_df %>% 
  group_by(state) %>% 
  filter(year == "2018") %>% 
  summarise(total_ice = sum(total_jail_from_ice, na.rm = TRUE))

# Map 

state_map <- map_data("state")
  state_abbr <- data.frame(state.abb, state.name)
  ice_2018 <- 
    left_join(ice_2018, state_abbr, by= c("state" = "state.abb"))
  
ice_2018 <- ice_2018 %>% 
    mutate(region = tolower(state.name))

  state_shape <- left_join(state_map, ice_2018)
  

ice_map <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x= long, y= lat, group = group, fill= total_ice)) +
  scale_fill_continuous(low = 'blue', high = 'white', labels = 
                          scales:: label_number_si()) + 
  coord_map() +
  labs(
    title = "Number of People Incarcerated by ICE in 2018",
    fill = "People Incarcerated"
  )

ice_map
ggplotly(ice_map)


#----------------------------------------------------------------------------#

## Load data frame ---- 






