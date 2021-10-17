library(tidyverse)
options(scipen = 999)
theme_set(theme_minimal(base_size = 15) +
          theme(legend.key.height = unit(0.8, 'cm'),
                legend.key.width = unit(0.8, 'cm'),
                axis.title = element_text(size = 16)))


#### Task 1 ####
# Load the landdata-states.csv file into R 
states <- read_csv(file = "landdata-states.csv")
glimpse(states)

# Re-create the graph shown in "fig1.png"
states %>% 
  ggplot(aes(x=Year, y=Land.Value, color = region)) +
  geom_smooth(size = 1.5) +
  labs(y="Land Value (USD)", color = "Region") 

# Export it to your Exam_2 folder as LASTNAME_Fig_1.jpg (note, that's a jpg, not a png)
ggsave("MCKEIGHAN_Fig_1.jpg")


#### Task 2 ####
# Write some code to show which state(s) are found in the "NA" region 
outcasts <- states %>% 
  filter(is.na(region))

unique(outcasts$State) # the only "states" are DC


#### Task 3 ####
# Tidy the data in unicef-u5mr.csv
df <- read_csv("unicef-u5mr.csv")

glimpse(df)

df <- df %>% 
  pivot_longer(c(starts_with("U")), 
               names_to = "year", 
               values_to = "mortality_rate",
               names_prefix = "U5MR.")

df$year <- as.integer(df$year)

#### Task 4 ####
# Re-create the graph shown in fig2.png
df %>% 
  ggplot(aes(x=year, y=mortality_rate, color = Continent)) +
  geom_point(size = 2.5) +
  labs(x = "Year", y = "MortalityRate") +
  scale_x_continuous(breaks = c(1960, 1980, 2000))

#Export it to your Exam_2 folder as LASTNAME_Fig_2.jpg (note, that's a jpg, not a png)
ggsave("MCKEIGHAN_Fig_2.jpg")



#### Task 5 ####
# Re-create the graph shown in fig3.png
df2 <- df %>% 
  filter(!is.na(mortality_rate)) %>% 
  group_by(Continent, year) %>% 
  summarize(avg_rate = mean(mortality_rate))

df2 %>% 
  ggplot(aes(x= year, y = avg_rate, color = Continent, group = Continent)) +
  geom_line(size = 3) +
  labs(x = "Year", y = "Mean Mortality Rate (deaths per 1000 live births)") +
  scale_x_continuous(breaks = c(1960, 1980, 2000)) 

# Export it to your Exam_2 folder as LASTNAME_Fig_3.jpg (note, that's a jpg, not a png  
ggsave("MCKEIGHAN_Fig_3.jpg")


#### Task 6 ####
#  Re-create the graph shown in fig4.png
df3 <- df %>% 
  filter(!is.na(mortality_rate))

df3$births <- rep(1000, 10244)

df3 <- df3 %>% 
  mutate(rate = mortality_rate/births)


df3 %>% 
  ggplot(aes(x = year, y = rate))+
  geom_point(size = 0.4, color = "blue", alpha = 0.7) +
  facet_wrap(~ Region) +
  scale_x_continuous(breaks = c(1960, 1980, 2000)) +
  theme(strip.background = element_rect(color = "black")) +
  labs(x = "Year", y="Mortality Rate") +
  theme(text = element_text(size = 10),
        axis.title = element_text(size = 10))

ggsave("MCKEIGHAN_Fig_4.jpg")
