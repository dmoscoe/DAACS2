library(tidyverse)
raw <- read_csv("viz 211113 1034.csv")

raw %>%
  mutate("label" = str_c(`School`, `Explanatory variables`, `Response variable`, sep = " ")) %>%
  mutate("Super Learner" = `sl accuracy` - `naive accuracy`) %>%
  mutate("Random Forest" = `rf accuracy` - `naive accuracy`) %>%
  select(c(label, `Super Learner`, `Random Forest`)) %>%
  arrange(`Super Learner`) %>%
  mutate("sort" = 1:length(`Super Learner`)) %>%
  pivot_longer(cols = c(`Super Learner`, `Random Forest`), names_to = "Model type") %>%
  ggplot(aes(x = value, y = reorder(label, sort), color = `Model type`)) +
  geom_point(size = 3) +
  theme_test() +
  theme(
    panel.grid.major.y = element_line(colour = "gray60", linetype = "dashed"),
    panel.grid.major.x = element_line(colour = "gray60", linetype = "dashed")
  ) +
  labs(x = "Model accuracy in excess of naive accuracy",
       y = "Model",
       title = "Super Learner and Random Forest Accuracy In Excess of Naive Accuracy")
