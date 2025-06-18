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
      rename(counts = Canada) %>% 
      rename(education = Highest.certif) %>% 
      mutate(year = as.numeric(year)) %>% 
      mutate(counts = as.numeric(counts)) %>% 
      mutate(birthplace = as.factor(birthplace)) %>% 
      mutate(birthplace = as.factor(birthplace)) %>% 
      mutate(across(where(is.character), trimws)) %>% 
      filter(!str_detect(labour, "Total")) %>% 
      pivot_wider(names_from = labour, values_from = counts, 
                  values_fill = list(counts = 0))
    
    #trying to remove the non printable characters
    df$labour <- gsub("[^[:print:]]", "", df$labour)
    
  } else if (year == "2021") {
    df <- df %>% 
      rename(gender = Gender..3.) %>%  # different naming for 2021
      rename(birthplace = Place.of.Birth) %>% 
      rename(labour = Selected.charact) %>% 
      rename(counts = Canada.20000....4.3..) %>% 
      rename(education = Highest.certific) %>% 
      mutate(year = as.numeric(year)) %>% 
      mutate(counts = as.numeric(counts)) %>% 
      mutate(birthplace = as.factor(birthplace)) %>% 
      mutate(across(where(is.character), trimws)) %>% 
      filter(!str_detect(labour, "Total")) %>% 
      pivot_wider(names_from = labour, values_from = counts, 
                  values_fill = list(counts = 0)) %>% 
      mutate(education = ifelse(is.na(education),"  Bachelor's degree or higher",
                                education)) #this is to fix the NA's by coercion error
    
    
    #trying to remove the non printable characters
    df$labour <- gsub("[^[:print:]]", "", df$labour)
    
    
  }
  
  # give df a year
  assign(paste0("df_", year), df)
}

#fix naming of birthplace for 2011 
df_2011 <- df_2011 %>% 
  left_join(mapping, by = c("birthplace" = "birthplace_2021")) %>% 
  mutate(birthplace = coalesce(birthplace_2021,birthplace)) %>% 
  select(-birthplace_2021)

#double check proper naming of birthplace in 2021
df_2021 <- df_2021 %>% 
  left_join(mapping, by = c("birthplace" = "birthplace_2021")) %>% 
  mutate(birthplace = coalesce(birthplace_2021, birthplace)) %>% 
  select(-birthplace_2021)


# combine, may not actually prove useful
df_comb <- bind_rows(df_2011, df_2021)

#convert chr var to factors
employ_stats <- df_comb %>% 
  select(1:4, 9:11) %>% 
  filter(as.integer(birthplace) == 4:9,
         education == "Total - Highest certificate, diploma or degree")
# filter(as.integer(birthplace) == 4:9,
#        ))

df2 <- df_comb %>% 
  select(1:4, 9:11) %>% 
  filter(birthplace