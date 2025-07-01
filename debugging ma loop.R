
# List your dataframes and output names
dfs <- list(df_2011, df_2021)
output_names <- c("df_overqualification_2011", "df_overqualification_2021")

# Skill C and D code prefixes and regex
skill_cd <- c(
  "14 ", "15 ", "34 ", "44 ", "64 ", "65 ", "74 ", "75 ", "84 ", "94 ", "95 ",
  "66 ", "67 ", "86 ", "96 "
)
regex_skill <- paste0("^(", paste(skill_cd, collapse = "|"), ")")


  names(df_2021) <- trimws(names(df_2021))
  
  df_skill_2021 <- df_2021 %>%
    select(c(birthplace, gender, education, matches(regex_skill))) %>%
    mutate(row_sum = rowSums(across(where(is.numeric)))) %>%
    rename("low_skill" = "row_sum") %>%
    select(birthplace, gender, education, low_skill)
  
  df_filtered <- df_skill_2021 %>%
    filter(
      birthplace == c("Born in Canada", "Born outside Canada", "Africa", "Western Africa", "Eastern Africa",
                      "Northern Africa", "Central Africa","Southern Africa"),
      gender == "Total - Gender",
      education == c("Bachelor’s degree or higher", "Total - Highest certificate, diploma or degree")
    )
  
  df_overqualification <- df_filtered %>%
    group_by(birthplace, gender) %>%
    reframe(
      overqualification_rate = 100 * low_skill[education == "Bachelor's degree or higher"] /
        low_skill[education == "Total - Highest certificate, diploma or degree"]
      # ,
      # .groups = "drop"
    )
  