library(tidyverse)
library(janitor)

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

dat %>% 
  filter(substrate == "Itaconic Acid") %>% 
  group_by(hrs, dilution) %>% 
  summarize(Mean_absorbance = mean(absorbance)) %>% 
  View()

dat %>% 
  filter(substrate == "Itaconic Acid") %>% 
  group_by(hrs) %>% 
  mutate(Mean_absorbance = mean(absorbance)) %>% 
  View()
  
group_by(hrs, dilution) %>% 
  summarize(Mean_absorbance = mean(absorbance)) %>% 
  View()

dat %>% 
  select(sample_id, substrate, rep, dilution, absorbance, hrs) %>% 
  filter(substrate == "Itaconic Acid")
  
dat %>% 
  filter(substrate == "Itaconic Acid") %>% 
  ggplot(aes(x=hrs, y=absorbance, color=sample_id)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~dilution) +
  theme_minimal()
