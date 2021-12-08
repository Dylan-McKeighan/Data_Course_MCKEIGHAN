library(tidyverse)
library(broom)

theme_set(theme_minimal())

# Task I
df <- read_csv("FacultySalaries_1995.csv")

clean <- df %>% 
  select(ends_with("Salary"),"FedID", "Tier", "State") %>% 
  pivot_longer(starts_with("Avg"), names_to = "Rank", values_to = "Salary", names_prefix = "Avg") %>% 
  mutate(Rank = str_remove_all(Rank, "ProfSalary")) %>% 
  filter(Tier != "VIIB")

ggplot(clean, aes(x=Rank, y=Salary, fill=Rank)) +
  geom_boxplot() +
  facet_wrap(~Tier) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave("McKeighan_Fig_1.jpg")

# Task II
mod <- clean %>% 
  aov(Salary ~ State + Tier + Rank, data=.)

sink("Salary_ANOVA_Summary.txt")
summary(mod)
sink(NULL)

# Task III
df <- read_csv("Juniper_Oils.csv")
chems <- c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene","compound 1","alpha-chamigrene","widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")

clean <- df %>% 
  pivot_longer(chems, names_to = "Compound", values_to = "Concentration")

# Task IV
clean %>% 
  ggplot(aes(x=YearsSinceBurn, y=Concentration)) +
  geom_smooth() +
  facet_wrap(~Compound, scales = "free_y")

# Task V
mod <- glm(data = clean,
           formula = Concentration ~ Compound*YearsSinceBurn)
tidy(mod) %>% 
  filter(p.value < 0.05)
