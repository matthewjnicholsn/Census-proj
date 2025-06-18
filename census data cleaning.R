rm(list = ls())
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)



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
      filter(!str_detect(labour, "Total"))
    
    #trying to remove the non printable characters
    df$labour <- gsub("[^[:print:]]", "", df$labour)
    df <- df %>% 
      pivot_wider(names_from = labour, values_from = counts, 
                  values_fill = list(counts = 0))
    
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
      filter(!str_detect(labour, "Total"))
    
    #trying to remove the non printable characters
    df$labour <- gsub("[^[:print:]]", "", df$labour)
    df <- df %>% 
      pivot_wider(names_from = labour, values_from = counts, 
                  values_fill = list(counts = 0)) %>% 
      mutate(education = ifelse(is.na(education),"  Bachelor's degree or higher",
                                education)) #this is to fix the NA's by coercion error
    
    
    
  }
  
  # give df a year
  assign(paste0("df_", year), df)
}
# combine, may not actually prove useful
df_comb <- bind_rows(df_2011, df_2021)

#get some summary tables
employ_stats <- df_comb %>% 
  select(1:4, 9:11) %>% 
  filter(as.integer(birthplace) == 4:9,
         education == "Total - Highest certificate, diploma or degree")
# filter(as.integer(birthplace) == 4:9,
#        ))