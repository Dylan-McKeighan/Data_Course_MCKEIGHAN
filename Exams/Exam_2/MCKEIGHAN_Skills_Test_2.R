library(tidyverse)
options(scipen = 999)

#### Task 1 ####
# Load the landdata-states.csv file into R 
states <- read_csv(file = "landdata-states.csv")

# Re-create the graph shown in "fig1.png"
states %>% 
  ggplot(aes(x=Year, y=Land.Value, color = region)) +
  geom_smooth(size=2) +
  theme_minimal() +
  labs(y="Land Value (USD)", color = "Region") +
  theme(axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

# Export it to your Exam_2 folder as LASTNAME_Fig_1.jpg (note, that's a jpg, not a png)
ggsave("MCKEIGHAN_Fig_1.jpg")

#### Task 2 ####
# Write some code to show which state(s) are found in the "NA" region 
outcasts <- states %>% 
  filter(is.na(region))

unique(outcasts$State)

#### Task 3 ####
# Tidy the data in unicef-u5mr.csv
df <- read_csv("unicef-u5mr.csv")

glimpse(df)

df <- df %>% 
  pivot_longer(c(starts_with("U")), 
               names_to = "year", 
               values_to = "mortality_rate",
               names_prefix = "U5MR.")

#### Task 4 ####
# Re-create the graph shown in fig2.png
df %>% 
  ggplot(aes(x=year, y=mortality_rate, color = Continent)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(x = "Year", y = "MortalityRate") +
  scale_x_discrete(breaks = c(1960, 1980, 2000)) +
  theme(axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) 

#Export it to your Exam_2 folder as LASTNAME_Fig_2.jpg (note, that's a jpg, not a png)
ggsave("MCKEIGHAN_Fig_2.jpeg")

#### Task 5 ####
# Re-create the graph shown in fig3.png
df2 <- df %>% 
  filter(!is.na(mortality_rate)) %>% 
  group_by(Continent, year) %>% 
  summarize(avg_rate = mean(mortality_rate))

df2 %>% 
  ggplot(aes(x= year, y = avg_rate, color = Continent, group = Continent)) +
  geom_line(size = 3) +
  theme_minimal() +
  labs(x = "Year", y = "Mean Mortality Rate (deaths per 1000 live births)") +
  scale_x_discrete(breaks = c(1960, 1980, 2000)) +
  theme(axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) 
# Export it to your Exam_2 folder as LASTNAME_Fig_3.jpg (note, that's a jpg, not a png  
ggsave("MCKEIGHAN_Fig_3.jpeg")

#### Task 6 ####
#  Re-create the graph shown in fig4.png
df3 <- df %>% 
  filter(!is.na(mortality_rate)) %>% 
  group_by(Region, year) %>% 
  summarize()

df3 %>% 
  ggplot(aes(x=year, y=freq)) +
  geom_point(color = "blue") +
  facet_wrap(~ Region) +
  theme_minimal()
