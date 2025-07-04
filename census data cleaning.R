rm(list = ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(showtext)
library(sysfonts)
# Add Computer Modern font (adjust path as needed)
# For many systems, you may need to download and provide the path to cmunrm.ttf
font_add("CMU Serif", regular = "/Users/matthewnicholson/Downloads/computer-modern/cmunrm.ttf")
showtext_auto()
theme_set(theme_minimal(base_family = "CMU Serif"))
#load in our data
df_2011 <- read.csv("datatable2011.csv")
df_2021 <- read.csv("datatable2021.csv")

#fix names of birthplace
mapping <- data.frame(
  birthplace_2011 = c("Total - Place of birth of respondent", "Born in Canada", 
                      "Born outside Canada", "Africa", "Western Africa", 
                      "Eastern Africa", "Northern Africa", "Central Africa", 
                      "Southern Africa", "Africa, n.i.e."),
  birthplace_2021 = c("Total - Place of birth", "Born in Canada", 
                      "Born outside Canada", "Africa", "Western Africa", 
                      "Eastern Africa", "Northern Africa", "Central Africa", 
                      "Southern Africa", "Africa, n.i.e.")  # NA for "Africa, n.i.e."
)

#fix names of gender
mapping2 <- data.frame(
  gender_2011 = c("Total - Sex", "Male", "Female"),
  gender_2021 = c("Total - Gender", "Men+", "Women+")
)

#tidy the data 

    df_2011 <- df_2011 %>% 
      rename(gender = Sex..3.) %>% 
      rename(birthplace = Place.of.birth) %>% 
      rename(labour = Selected.charact) %>% 
      filter(!str_detect(labour, "Total")) %>% 
      rename(counts = Canada) %>% 
      rename(education = Highest.certif) %>% 
      mutate(year = as.numeric(2011)) %>% 
      mutate(counts = as.numeric(counts)) %>% 
      pivot_wider(names_from = labour, values_from = counts, 
                  values_fill = list(counts = 0)) %>% 
      rename(low_income = "In low income in 2010 based on after-tax low-income measure (LIM-AT)") %>% 
      mutate(birthplace = as.factor(birthplace)) %>% 
      mutate(across(where(is.character), trimws)) 

  
    
    df_2021 <- df_2021 %>% 
      rename(gender = Gender..3.) %>%  # different naming for 2021
      rename(birthplace = Place.of.Birth) %>% 
      rename(labour = Selected.charact) %>% 
      filter(!str_detect(labour, "Total")) %>% 
      rename(counts = Canada.20000....4.3..) %>% 
      mutate(counts = as.numeric(counts)) %>% 
      pivot_wider(names_from = labour, values_from = counts, 
                  values_fill = list(counts = 0)) %>% 
      rename(low_income = "Prevalence of low income (LIM-AT) (%)") %>% 
      rename(overqual = `Overqualification rate (based on skill level C and D)`) %>% 
      rename(education = Highest.certific) %>% 
      mutate(year = as.numeric(2021)) %>% 
      mutate(birthplace = as.character(birthplace)) %>% 
      mutate(across(where(is.character), trimws)) %>% 
      mutate(education = ifelse(is.na(education),"  Bachelor's degree or higher",
                                education)) #this is to fix the NA's by coercion error

#fix naming of birthplace for 2011 
df_2011 <- df_2011 %>% 
  left_join(mapping, by = c("birthplace" = "birthplace_2011")) %>% 
  mutate(birthplace = coalesce(birthplace_2021, birthplace)) %>% 
  select(-birthplace_2021) %>% 
  mutate(birthplace = as.character(birthplace)) %>%
  mutate(across(where(is.character), trimws)) %>%
  filter(!(birthplace == "Africa, n.i.e."))



#fix naming for gender for 2011
df_2011 <- df_2011 %>%
  left_join(mapping2, by = c("gender" = "gender_2011")) %>% 
  mutate(gender = coalesce(gender_2021, gender)) %>% 
  select(-gender_2021) %>% 
  mutate(gender = as.factor(gender))

df_2021 <- df_2021 %>% 
  mutate(birthplace = as.factor(birthplace)) %>% 
  mutate(across(where(is.character), trimws))

# combine, may not actually prove useful
df_comb <- bind_rows(df_2011, df_2021)


#convert chr var to factors
employ_stats_2011 <- df_2011 %>% 
  select(1:4, 9:11, 232) %>% 
  filter(gender == "Total - Gender",
         education == "Total - Highest certificate, diploma or degree") 
         
         # birthplace == c("Born in Canada", "Africa", "Western Africa", 
         #                 "Eastern Africa", "Northern Africa", "Central Africa", 
         #                 "Southern Africa"),
         # )

# use grep("year", colnames(df_2021)) to get the col index for year or others
employ_stats_2021 <- df_2021 %>% 
  select(1:3, 205, 8:10, 157, 201) %>% 
  filter(gender == "Total - Gender",
         education == "Total - Highest certificate, diploma or degree")

# Combine the two dataframes
employ_stats_2011 <- employ_stats_2011 %>% 
  mutate(year = as.factor(year))
employ_stats_2021 <- employ_stats_2021 %>% 
  mutate(year = as.factor(year))

combined_employ_stats <- bind_rows(employ_stats_2011, employ_stats_2021)

# Remove the totals if desired
combined_employ_stats <- combined_employ_stats %>%
  filter(!grepl("Total", birthplace))

#plot employment rate by birthplace for 2011 and 2021
p1 <- ggplot(combined_employ_stats, 
       aes(x = birthplace, y = `Employment rate (%)`, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(0,80) +
  labs(title = "Employment Rate by Birthplace (2011 vs 2021)",
       x = "Birthplace",
       y = "Employment Rate (%)") +
  theme_minimal(base_family = "CMU Serif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank())
plot(p1)

ggsave("employ_rate.png", plot = last_plot(), device = "png")

#plot unemployment rate by birthplace for 2011 and 2021
p2 <- ggplot(combined_employ_stats, 
       aes(x = birthplace, y = `Unemployment rate (%)`, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(0,20) +
  labs(title = "Unemployment Rate by Birthplace (2011 vs 2021)",
       x = "Birthplace",
       y = "Unemployment Rate (%)") +
  theme_minimal(base_family = "CMU Serif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(
    panel.grid.major = element_blank()
  )
plot(p2)
ggsave("unemploy_rate.png", plot = last_plot(), device = "png")

#plot participation rate by birthplace for 2011 and 2021

p3 <- ggplot(combined_employ_stats, 
       aes(x = birthplace, y = `Participation rate (%)`, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(0,90) +
  labs(title = "Participation Rate by Birthplace (2011 vs 2021)",
       x = "Birthplace",
       y = "Participation Rate (%)") +
  theme_minimal(base_family = "CMU Serif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major = element_blank()) 
plot(p3)
ggsave("participation_rate.png", plot = last_plot(), device = "png")


p4 <- ggplot(combined_employ_stats,
       aes(x = birthplace, y = low_income, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(0,40) +
  labs(title = "Low-income Rate by Birthplace (2011 vs 2021)",
       x = "Birthplace",
       y = "Participation Rate (%)") +
  theme_minimal(base_family = "CMU Serif")
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major = element_blank()) +
plot(p4)
ggsave("lowincome_rate.png", plot = last_plot(), device = "png")


#plot overqualification rate
combined_employ_stats_sub <- combined_employ_stats %>% 
  filter(year == 2021)
p5 <- ggplot(combined_employ_stats_sub,
       aes(x = birthplace, y = overqual, fill = "2021")) +
  geom_bar(stat = "identity", position = "stack", show.legend = F) +
  ylim(0,65) +
  annotate('text', x = 5, y = 60, label="*Data unavailable for 2011", size = 2) +
  labs(title = "Overqualification Rate by Birthplace (2021)",
       x = "Birthplace",
       y = "Overqualification Rate (%)") +
  theme_minimal(base_family = "CMU Serif") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid.major = element_blank())
plot(p5)
ggsave("overqual_rate.png", plot = last_plot(), device = "png")


##caclulate  overqualification measure for 2011 and 2021

#for 2011

# Skill C and D code prefixes and regex
skill_cd <- c(
  "14 ", "15 ", "34 ", "44 ", "64 ", "65 ", "74 ", "75 ", "84 ", "94 ", "95 ",
  "66 ", "67 ", "86 ", "96 "
)
regex_skill <- paste0("^(", paste(skill_cd, collapse = "|"), ")")

 #still whitespace in names for some reason so trimming again
  names(df_2011) <- trimws(names(df_2011))
  
  df_skill_2011 <- df_2011 %>%
    select(c(birthplace, gender, education, matches(regex_skill))) %>%
    mutate(row_sum = rowSums(across(where(is.numeric)))) %>%
    rename("low_skill" = "row_sum") %>%
    select(birthplace, gender, education, low_skill)
  
  df_filtered_2011 <- df_skill_2011 %>%
    filter(
      gender == "Total - Gender",
      education %in% c("Bachelor's degree or higher", "Total - Highest certificate, diploma or degree")
    )
  
  df_overqualification_2011 <- df_filtered_2011 %>%
    group_by(birthplace, gender) %>%
    reframe(
      overqualification_rate = 100 * low_skill[education == "Bachelor's degree or higher"] /
        low_skill[education == "Total - Highest certificate, diploma or degree"]
    ) 
  
  df_overqualification_2011 <- df_overqualification_2011 %>% 
    mutate(year = 2011)
  
# For 2021
  #still whitespace in names for some reason so trimming again
  names(df_2021) <- trimws(names(df_2021))
  
  df_skill_2021 <- df_2021 %>%
    select(c(birthplace, gender, education, matches(regex_skill))) %>%
    mutate(row_sum = rowSums(across(where(is.numeric)))) %>%
    rename("low_skill" = "row_sum") %>%
    select(birthplace, gender, education, low_skill)
  
  df_filtered_2021 <- df_skill_2021 %>%
    filter(
      gender == "Total - Gender",
      education %in% c("Bachelor’s degree or higher", "Total - Highest certificate, diploma or degree") 
    )
  
  df_overqualification_2021 <- df_filtered_2021 %>%
    group_by(birthplace, gender) %>%
    reframe(
      overqualification_rate = 100 * low_skill[education == "Bachelor’s degree or higher"] /
        low_skill[education == "Total - Highest certificate, diploma or degree"]
    ) 
  
  df_overqualification_2021 <- df_overqualification_2021 %>%
    mutate(year = 2021)


  
#plot REAL overqualification
  combined_overqual <- bind_rows(df_overqualification_2011, df_overqualification_2021) 
  combined_overqual <- combined_overqual %>% 
    mutate(year = as.factor(year)) #need this or ggplot won't dodge correctly and tries to create a continuous scale
  
  p6 <- ggplot(combined_overqual,
               aes(x = birthplace, y = overqualification_rate, fill = year)) +
    geom_bar(stat = "identity", position = "dodge") +
    ylim(0,40) +
    labs(title = "Overqualification Rate by Birthplace (2011 vs. 2021)",
         x = "Birthplace",
         y = "Overqualification Rate (%)") +
    theme_minimal(base_family = "CMU Serif")
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid.major = element_blank())
  plot(p6)
  ggsave("overqual_rate_2011.png", plot = last_plot(), device = "png")

  
  
## not plot stats with gender
  
  #convert chr var to factors
  gender_stats_2011 <- df_2011 %>% 
    select(1:4, 9:11, 232) %>% 
    filter(
           education == "Total - Highest certificate, diploma or degree") 
  
  # birthplace == c("Born in Canada", "Africa", "Western Africa", 
  #                 "Eastern Africa", "Northern Africa", "Central Africa", 
  #                 "Southern Africa"),
  # )
  
  # use grep("year", colnames(df_2021)) to get the col index for year or others
  gender_stats_2021 <- df_2021 %>% 
    select(1:3, 205, 8:10, 157, 201) %>% 
    filter(
           education == "Total - Highest certificate, diploma or degree")
  
  
  combined_gender_stats <- bind_rows(gender_stats_2011, gender_stats_2021)
  # Filter to remove totals and keep only gender breakdowns
  employment_gender <- combined_gender_stats %>%
    filter(gender != "Total - Gender") %>%
    filter(!grepl("Total", birthplace)) %>%
    mutate(year = as.factor(year))
  
  # Plot
  p_emp_gender <- ggplot(employment_gender, 
                         aes(x = birthplace, y = `Employment rate (%)`, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~year) +
    ylim(0, 80) +
    labs(title = "Employment Rate by Gender and Birthplace (2011 vs 2021)",
         x = "Birthplace",
         y = "Employment Rate (%)") +
    theme_minimal(base_family = "CMU Serif") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_emp_gender)
  ggsave("employment_by_gender.png", plot = p_emp_gender, device = "png")
 
   # unemployment rate plot
  p_unemp_gender <- ggplot(employment_gender, 
                           aes(x = birthplace, y = `Unemployment rate (%)`, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~year) +
    ylim(0, 20) +
    labs(title = "Unemployment Rate by Gender and Birthplace (2011 vs 2021)",
         x = "Birthplace", y = "Unemployment Rate (%)") +
    theme_minimal(base_family = "CMU Serif") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_unemp_gender)
  ggsave("unemployment_by_gender.png", plot = p_unemp_gender, device = "png")
  
  # participation rate plot
  p_partic_gender <- ggplot(employment_gender,
                            aes(x = birthplace, y = `Participation rate (%)`, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~year) +
    ylim(0, 90) +
    labs(title = "Participation Rate by Gender and Birthplace (2011 vs 2021)",
         x = "Birthplace", y = "Participation Rate (%)") +
    theme_minimal(base_family = "CMU Serif") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_partic_gender)
  ggsave("participation_by_gender.png", plot = p_partic_gender, device = "png")
  
  #low income plot
  p_lowinc_gender <- ggplot(employment_gender,
                           aes(x = birthplace, y = low_income, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~year) +
    ylim(0, 40) +
    labs(title = "Low-Income Rate by Gender and Birthplace (2011 vs 2021)",
         x = "Birthplace", y = "Participation Rate (%)") +
    theme_minimal(base_family = "CMU Serif") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p_lowinc_gender)
  ggsave("lowincome_by_gender.png", plot = p_lowinc_gender, device = "png")
  
  #run overqual by gender
  
  source('Overqualification by gender.R')