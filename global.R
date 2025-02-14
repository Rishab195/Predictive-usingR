library(dplyr) #for mutate
library(plotly)
library(ggplot2) # Graph geometricpoints
library(ggtext)  #rich text formatting
#install.packages("maps")
library(maps)
my_data<-USArrests

str(my_data)
#Assigning row names to object
states=rownames(my_data)
my_data=my_data %>%
  mutate(State=states)

c1=my_data %>%
  select(-"State") %>%
  names()

c2 = my_data %>% 
  select(-"State", -"UrbanPop") %>% 
  names()

#Top 5 states with high rates
my_data %>%
  select(State,Rape) %>%
  arrange(desc(Rape)) %>%
  head(5)

#Top 5 states with low rates
my_data %>%
  select(State,Rape) %>%
  arrange(Rape) %>%
  head(5)

state_map<-map_data("state")

#Convert state to lower case
my_data1=my_data %>%
  mutate(State=tolower(State))


merged =right_join(my_data1, state_map,  by=c("State" = "region"))
str(merged)

#Add State Abrevations
st = data.frame(abb = state.abb, stname=tolower(state.name), x=state.center$x, y=state.center$y)
str(st)

new_join = left_join(merged, st, by=c("State" = "stname"))
