library(dplyr)
library(ggplot2)
library(stringr)

#  skill level C and D codes
skill_cd <- c(
  "14 ", "15 ", "34 ", "44 ", "64 ", "65 ", "74 ", "75 ", "84 ", "94 ", "95 ",
  "66 ", "67 ", "86 ", "96 "
)
regex_skill <- paste0("^(", paste(skill_cd, collapse = "|"), ")")

# 2011 
# calculate low skill by gender and birthplace for 2011
df_skill_2011 <- df_2011 %>%
  select(birthplace, gender, education, matches(regex_skill)) %>%
  mutate(low_skill = rowSums(across(where(is.numeric)))) %>%
  select(birthplace, gender, education, low_skill)

df_filtered_2011 <- df_skill_2011 %>%
  filter(
    education %in% c("Bachelor's degree or higher", "Total - Highest certificate, diploma or degree")
  )

# calculate overqualification rate by gender and birthplace for 2011
df_overqualification_gender_2011 <- df_filtered_2011 %>%
  group_by(birthplace, gender) %>%
  summarise(
    low_skill_bachelor = low_skill[education == "Bachelor's degree or higher"],
    low_skill_total = low_skill[education == "Total - Highest certificate, diploma or degree"],
    .groups = "drop"
  ) %>%
  mutate(
    overqualification_rate = 100 * low_skill_bachelor / low_skill_total,
    year = 2011
  ) %>%
  filter(!is.na(overqualification_rate))

# 021 
df_skill_2021 <- df_2021 %>%
  select(birthplace, gender, education, matches(regex_skill)) %>%
  mutate(low_skill = rowSums(across(where(is.numeric)))) %>%
  select(birthplace, gender, education, low_skill)

df_filtered_2021 <- df_skill_2021 %>%
  filter(
    education %in% c("Bachelor’s degree or higher", "Total - Highest certificate, diploma or degree")
  )

df_overqualification_gender_2021 <- df_filtered_2021 %>%
  group_by(birthplace, gender) %>%
  summarise(
    low_skill_bachelor = low_skill[education == "Bachelor’s degree or higher"],
    low_skill_total = low_skill[education == "Total - Highest certificate, diploma or degree"],
    .groups = "drop"
  ) %>%
  mutate(
    overqualification_rate = 100 * low_skill_bachelor / low_skill_total,
    year = 2021
  ) %>%
  filter(!is.na(overqualification_rate))

# combine
overqual_gender <- bind_rows(df_overqualification_gender_2011, df_overqualification_gender_2021) %>%
  filter(!grepl("Total", birthplace), !grepl("Total", gender))

# plot
p_overqual_gender <- ggplot(overqual_gender, aes(x = birthplace, y = overqualification_rate, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~year) +
  ylim(0, 50) +
  labs(
    title = "Overqualification Rate by Gender and Birthplace (2011 vs 2021)",
    x = "Birthplace", y = "Overqualification Rate (%)"
  ) +
  theme_minimal(base_family = "CMU Serif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_overqual_gender)

ggsave("overqual_rate_by_gender_birthplace.pdf", device = "pdf")

