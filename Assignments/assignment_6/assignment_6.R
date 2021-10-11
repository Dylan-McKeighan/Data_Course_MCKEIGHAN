library(tidyverse)
library(janitor)
library(gganimate)

# Load the data
dat <- read_csv("BioLog_Plate_Data.csv")

# Visualizing data
glimpse(dat)

# Clean column names
names(dat) <- make_clean_names(names(dat))

# Convert into tidy (long) form 
dat <- dat %>% 
  pivot_longer(cols = c("hr_24", "hr_48", "hr_144"),
               names_to = "hours",
               values_to = "absorbance")

dat <- dat %>% 
mutate(source = case_when(sample_id == "Clear_Creek" ~ "Water",
                          sample_id == "Waste_Water" ~ "Water",
                          TRUE ~ "Soil"))

dat <- dat %>% 
  mutate(hrs = case_when(hours == "hr_24" ~ 24,
                         hours == "hr_48" ~ 48,
                         hours == "hr_144" ~ 144))

#dat$hours[dat$hours == "hr_24"] <- 24
#dat$hours[dat$hours == "hr_48"] <- 48
#dat$hours[dat$hours == "hr_144"] <- 144
#dat$hours <- as.numeric(dat$hours)

dat %>% 
  filter(dilution == 0.100) %>% 
  ggplot(aes(x=hrs, y=absorbance, color=source)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~substrate) +
  theme_minimal() +
  labs(x="Time", y="Absorbance", color="Type")

ggsave("plot1.jpeg")

dat2 <- dat %>% 
  filter(substrate == "Itaconic Acid") %>% 
  group_by(dilution, hrs, sample_id) %>% 
  summarize(Mean_absorbance = mean(absorbance))

plot2 <- dat2 %>% 
  ggplot(aes(x=hrs, y=Mean_absorbance, color = sample_id)) +
  geom_line() +
  facet_wrap(~ dilution) +
  labs(x="Time", colors = "Sample ID") +
  theme_minimal() +
  transition_reveal(hrs)

animate(plot2)

animate(plot2, fps = 10, width = 750, height = 450)
anim_save("plot2.gif")
