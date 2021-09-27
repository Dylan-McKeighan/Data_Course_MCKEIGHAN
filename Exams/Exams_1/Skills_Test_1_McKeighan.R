# I. Read the cleaned_covid_data.csv file into an R data frame.
library(tidyverse)

df <- read_csv(file = "cleaned_covid_data.csv")

# II. Subset the data set to just show states that begin with “A” and save this as an object called A-states.
A_states <- df %>% 
  filter(substr(Province_State, 1, 1) == "A")

# III. Create a plot of that subset showing Active cases over time, with a separate facet for each state. 
A_states %>% 
  ggplot(aes(x=Last_Update, y=Active)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~Province_State, scales = "free")

# IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate
state_max_fatality_rate <- df %>% 
  filter(!is.na(Case_Fatality_Ratio)) %>% 
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))

# V. Use that new data frame from task IV to create another plot
state_max_fatality_rate %>% 
  ggplot(aes(x= reorder(Province_State, -Maximum_Fatality_Ratio), y=Maximum_Fatality_Ratio)) +
  geom_col() +
  labs(x="Region") +
  theme(axis.text.x = element_text(angle = 90))

# Extra Credit: VI. Using the FULL data set, plot cumulative deaths for the entire US over time
df %>% 
  group_by(Last_Update) %>% 
  summarize(Cum_Deaths = sum(Deaths)) %>% 
  ggplot(aes(x=Last_Update, y=Cum_Deaths)) +
  geom_point() +
  labs(x = "Time", y = "Cumulative Deaths") +
  theme_minimal()
            