rm(list = ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

#fix names of birthplace
mapping <- data.frame(
  birthplace_2011 = c("Total - Place of birth of respondent", "Born in Canada", 
                      "Born outside Canada", "Africa", "Western Africa", 
                      "Eastern Africa", "Northern Africa", "Central Africa", 
                      "Southern Africa", "Africa, n.i.e."),
  birthplace_2021 = c("Total - Place of birth", "Born in Canada", 
                      "Born outside Canada", "Africa", "Western Africa", 
                      "Eastern Africa", "Northern Africa", "Central Africa", 
                      "Southern Africa", NA)  # NA for "Africa, n.i.e."
)
#fix names of gender
mapping2 <- data.frame(
  gender_2011 = c("Total - Sex", "Male", "Female"),
  gender_2021 = c("Total - Gender", "Men+", "Women+")
)

data <- c("datatable2011.csv", "datatable2021.csv")

for(i in seq_along(data)) {
  # get year from filename
  year <- substr(data[i], 10, 13)  
  
  # read file
  df <- read.csv(data[i])
  
  
  
  df <- df %>% 
    mutate_if(is.character, ~ iconv(., from = "UTF-8", to = "UTF-8"))
  
  
  
  # rename vars
  if (year == "2011") {
    df <- df %>% 
      rename(gender = Sex..3.) %>% 
      rename(birthplace = Place.of.birth) %>% 
      rename(labour = Selected.charact) %>% 
      filter(!str_detect(labour, "Total")) %>% 
      rename(counts = Canada) %>% 
      rename(education = Highest.certif) %>% 
      mutate(year = as.numeric(year)) %>% 
      mutate(counts = as.numeric(counts)) %>% 
      pivot_wider(names_from = labour, values_from = counts, 
                  values_fill = list(counts = 0)) %>% 
      rename(low_income = "In low income in 2010 based on after-tax low-income measure (LIM-AT)") %>% 
      mutate(birthplace = as.factor(birthplace)) %>% 
      mutate(across(where(is.character), trimws)) 

  
    
    # #trying to remove the non printable characters
    # df$labour <- gsub("[^[:print:]]", "", df$labour)
    
  } else if (year == "2021") {
    df <- df %>% 
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
      mutate(year = as.numeric(year)) %>% 
      mutate(birthplace = as.factor(birthplace)) %>% 
      mutate(across(where(is.character), trimws)) %>% 
      mutate(education = ifelse(is.na(education),"  Bachelor's degree or higher",
                                education)) #this is to fix the NA's by coercion error
    
    
    # #trying to remove the non printable characters
    # df$labour <- gsub("[^[:print:]]", "", df$labour)
    
    
  }
  
  # give df a year
  assign(paste0("df_", year), df)
}

#fix naming of birthplace for 2011 
df_2011 <- df_2011 %>% 
  left_join(mapping, by = c("birthplace" = "birthplace_2011")) %>% 
  mutate(birthplace = coalesce(birthplace_2021, birthplace)) %>% 
  select(-birthplace_2021) %>% 
  mutate(birthplace = as.factor(birthplace)) %>% 
  mutate(across(where(is.character), trimws)) %>% 
  filter(!(birthplace == "      Africa, n.i.e."))



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
ggplot(combined_employ_stats, 
       aes(x = birthplace, y = `Employment rate (%)`, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Employment Rate by Birthplace (2011 vs 2021)",
       x = "Birthplace",
       y = "Employment Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot unemployment rate by birthplace for 2011 and 2021
ggplot(combined_employ_stats, 
       aes(x = birthplace, y = `Unemployment rate (%)`, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Unemployment Rate by Birthplace (2011 vs 2021)",
       x = "Birthplace",
       y = "Unemployment Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot participation rate by birthplace for 2011 and 2021

ggplot(combined_employ_stats, 
       aes(x = birthplace, y = `Participation rate (%)`, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Participation Rate by Birthplace (2011 vs 2021)",
       x = "Birthplace",
       y = "Participation Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_employ_stats,
       aes(x = birthplace, y = low_income, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Low-income Rate by Birthplace (2011 vs 2021)",
       x = "Birthplace",
       y = "Participation Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot overqualification rate
combined_employ_stats_sub <- combined_employ_stats %>% 
  filter(year == 2021)
ggplot(combined_employ_stats_sub,
       aes(x = birthplace, y = overqual, fill = "2021")) +
  geom_bar(stat = "identity", position = "stack", show.legend = F) +
  annotate('text', x = 5, y = 60, label="*Data unavailable for 2011", size = 2) +
  labs(title = "Overqualification Rate by Birthplace (2021)",
       x = "Birthplace",
       y = "Overqualification Rate (%)") +
  theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  

