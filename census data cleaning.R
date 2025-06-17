rm(list = ls())
library(tidyr)
library(dplyr)
library(ggplot2)



data <- c("datatable2011.csv", "datatable2021.csv")

for(i in seq_along(data)) {
  # get year from filename
  year <- substr(data[i], 10, 13)  
  
  # read file
  df <- read.csv(data[i])
  
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
      mutate(birthplace = as.factor(birthplace))
  } else if (year == "2021") {
    df <- df %>% 
      rename(gender = Gender..3.) %>%  # different naming for 2021
      rename(birthplace = Place.of.Birth) %>% 
      rename(labour = Selected.charact) %>% 
      rename(counts = Canada.20000....4.3..) %>% 
      rename(education = Highest.certific) %>% 
      mutate(year = as.numeric(year)) %>% 
      mutate(counts = as.numeric(counts)) %>% 
      mutate(birthplace = as.factor(birthplace))
  }
  
  # give df a year
  assign(paste0("df_", year), df)
}
# combine, may not actually prove useful
df_comb <- bind_rows(df_2011, df_2021)

#convert chr var to factors


#get some summary stats
employ_2011 <- df_2011 %>% 
  filter(birthplace == "7",
         gender == "Total - Sex",
         labour == "Employment rate (%)",
         education == "Total - Highest certificate, diploma or degree")
employ_2021 <- df_2021 %>% 
  filter(birthplace == "    Africa",
           gender == "Total - Gender",
           labour == "Employment rate (%)",
           education == "Total - Highest certificate, diploma or degree")
employ_canada_2011 <- df_2011 %>% 
  filter(birthplace == "  Born in Canada",
         gender == "Total - Sex",
         labour == "Employment rate (%)",
         education == "Total - Highest certificate, diploma or degree")
employ_canada_2021 <- df_2021 %>% 
  filter(birthplace == "  Born in Canada",
         gender == "Total - Gender",
         labour == "Employment rate (%)",
         education == "Total - Highest certificate, diploma or degree")
employ_stats <- bind_rows(employ_2021, employ_2011, employ_canada_2011, employ_canada_2021)

employ_ac_2011 <- df_2011 %>% 
  filter(
         birthplace == c("    Africa","      Western Africa", "      Eastern Africa", 
                         "      Northern Africa", "      Central Africa", "      Southern Africa" ),
         labour == "Employment rate (%)",
         education == "Total - Highest certificate, diploma or degree"
  )



