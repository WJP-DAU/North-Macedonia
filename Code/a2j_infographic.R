## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            North Macedonia Country Report - A2J Section
##
## Author(s):         Santiago Pardo (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 28th, 2022
##
## This version:      November 28th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## A2J Tables and Plots                                                                                       ----
## 
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Tables                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

a2j_tables.fn <- function(a2j_section) {
  
  a2j_section.df <- a2j_section %>%
    mutate(             
      fin_security          =
        if_else(fin == 1 | fin == 2, "Financially Insecure",
                if_else(fin == 3 | fin == 4 | fin == 5, "Financially Secure", NA_character_))
    )
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 1:  Legal problems                                                                            ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  problems_section <- a2j_section.df %>%
    group_by(country, fin_security) %>%
    summarise(legal_problem = mean(legal, na.rm = T)) %>%
    ungroup() %>%
    drop_na() %>%
    filter(legal_problem > 0) %>%
    mutate(legal_problem = paste0(round(legal_problem*100,0), "%")) 
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 2:  Capability                                                                             ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  legal_section <- a2j_section.df %>%
    filter(legal == 1) %>%
    select(country, q41b, q41c, q41d, fin_security) %>%
    mutate(
      across(!c(country,fin_security),
             ~ case_when(
               .x == 1 ~ 1,
               .x == 2 ~ 1,
               .x == 3 ~ 0,
               .x == 4 ~ 0,
               .x == 99 ~ NA_real_
             )
      )
      ) %>%
    group_by(country, fin_security) %>%
    summarise(get_information = mean(q41b, na.rm = T),
              get_expert      = mean(q41c, na.rm = T),
              confidence      = mean(q41d, na.rm = T)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(get_information = paste0(round(get_information*100,0), "%"),
           get_expert = paste0(round(get_expert*100,0), "%"),
           confidence = paste0(round(confidence*100,0), "%"))
  
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 3:  Sources of help                                                                          ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  help_section <- a2j_section.df %>%
    filter(legal == 1) %>%
    select(country, q24, fin_security) %>%
    mutate(sources_help = case_when(
      q24 == 1  ~ 1,
      q24 == 0  ~ 0,
      q24 == 99 ~ NA_real_
    )) %>%
    group_by(country, fin_security) %>%
    summarise(source_help = mean(sources_help, na.rm = T)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(source_help =  paste0(round(source_help*100,0), "%"))
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 4:  Problem status                                                                          ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  "%!in%" <- compose("!", "%in%")
  
  status_section <- a2j_section.df %>%
    filter(country %!in% c("Honduras", "Panama", "Guatemala", "Belize", "El Salvador")) %>%
    filter(legal == 1) %>%
    select(country, q34_merge, fin_security) %>%
    mutate(q34_merge       = if_else(q34_merge != 99, q34_merge, NA_real_)) %>%
    mutate(fully_resolved  = if_else(q34_merge == 4, 1, 0),
           problem_persist = if_else(q34_merge == 3, 1, 0)) %>%
    group_by(country, fin_security) %>%
    summarise(fully_resolved  = mean(fully_resolved, na.rm = T),
              problem_persist = mean(problem_persist, na.rm = T)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(fully_resolved =  paste0(round(fully_resolved*100,0), "%"),
           problem_persist = paste0(round(problem_persist*100,0), "%"))
  
  # Create q34 merge
  
  status_section_CA <- a2j_section.df %>%
    filter(country %in% c("Honduras", "Panama", "Guatemala", "Belize", "El Salvador")) %>%
    filter(legal == 1) %>%
    select(country, q30, q34, fin_security) %>%
    mutate(q30       = if_else(q30 == 99,  NA_real_, q30),
           q34       = if_else(q34 == 99, NA_real_, q34)) %>%
    mutate(fully_resolved  = if_else(q30 == 4 | q34 == 4, 1, 0, 0),
           problem_persist = if_else(q30 == 3 | q34 == 3, 1, 0, 0)) %>%
    group_by(country, fin_security) %>%
    summarise(fully_resolved  = mean(fully_resolved, na.rm = T),
              problem_persist = mean(problem_persist, na.rm = T)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(fully_resolved =  paste0(round(fully_resolved*100,0), "%"),
           problem_persist = paste0(round(problem_persist*100,0), "%"))
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 5:  Process                                                                              ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  process_section <- a2j_section.df %>%
    mutate(process = if_else(q34_merge == 3 | q34_merge == 4, 1, 0)) %>%
    filter(process == 1) %>%
    filter(legal == 1) %>%
    select(country, q36a, q37b, q37c, q37d, q34_merge, q28, fin_security) %>%
    mutate(fair = case_when(
      q36a == 1 ~ 1,
      q36a == 0 ~ 0,
      q36a == 99 ~ NA_real_
    ),
    difficult_cost       = if_else(q37d == 3 | q37d == 4, 1, 
                                   if_else(q37d == 1 | q37d == 2, 0, NA_real_)),
    financial_difficulty = if_else(q37c == 1 & difficult_cost == 1, 1, 0),
    time                 = if_else(q37b >= 0 & q28 == 1, q37b, NA_real_)) %>%
    group_by(country, fin_security) %>%
    summarise(fair                 = mean(fair, na.rm = T),
              time                 = mean(time, na.rm = T),
              financial_difficulty = mean(financial_difficulty, na.rm = T)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(fair =  paste0(round(fair*100,0), "%"),
           financial_difficulty = paste0(round(financial_difficulty*100,0), "%"))
  
  process_section_CA <-  a2j_section.df %>%
    filter(legal == 1) %>%
    filter(country %in% c("Honduras", "Panama", "Guatemala", "Belize", "El Salvador")) %>%
    select(country, q30, q34, q36a, EXP22_q43, q37c, q37d, q34_merge, q28, fin_security) %>%
    mutate(q30       = if_else(q30 == 99,  NA_real_, q30),
           q34       = if_else(q34 == 99, NA_real_, q34)) %>%
    mutate(fully_resolved  = if_else(q30 == 4 | q34 == 4, 1, 0, 0),
           problem_persist = if_else(q30 == 3 | q34 == 3, 1, 0, 0)) %>%
    mutate(process = if_else(fully_resolved == 1 | problem_persist == 1, 1, 0)) %>%
    filter(process == 1) %>%
    mutate(fair = case_when(
      q36a == 1 ~ 1,
      q36a == 0 ~ 0,
      q36a == 99 ~ NA_real_
    ),
    difficult_cost       = if_else(q37d == 3 | q37d == 4, 1, 
                                   if_else(q37d == 1 | q37d == 2, 0, NA_real_)),
    financial_difficulty = if_else(q37c == 1 & difficult_cost == 1, 1, 0),
    time                 = if_else(EXP22_q43 == 99, NA_real_, EXP22_q43),
    time                 = if_else(time > 2 & q28 == 1, 1, 0)) %>%
    group_by(country, fin_security) %>%
    summarise(fair                 = mean(fair, na.rm = T),
              time                 = mean(time, na.rm = T),
              financial_difficulty = mean(financial_difficulty, na.rm = T)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(fair =  paste0(round(fair*100,0), "%"),
           financial_difficulty = paste0(round(financial_difficulty*100,0), "%"),
           timeMoreSixMonths = paste0(round(time*100,0), "%"))
  
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 6:  Hardships.                                                                              ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  hardships_section <- a2j_section.df %>%
    filter(legal == 1) %>%
    select(country, q42a, q42b, q42c, q42d, fin_security) %>%
    mutate(
      across(!c(country,fin_security),
             ~ case_when(
               .x == 1 ~ 1,
               .x == 0 ~ 0,
               .x == 99 ~ NA_real_)))
  
  # Any Hardship
  
  hardships_any <- hardships_section %>% 
    mutate(hardships =
             if_else(
               q42a == 1 |
                 q42b == 1 |
                 q42c == 1 |
                 q42d == 1 ,
               1, 0
             )
    ) %>%
    group_by(country, fin_security) %>%
    summarise(any_hardship = mean(hardships, na.rm = T)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(any_hardship = paste0(round(any_hardship*100,0), "%"))
  
  # Hardships divided
  
  hardships <- hardships_section %>%
    group_by(country, fin_security) %>%
    summarise(health         = mean(q42a, na.rm = T),
              interpersonal  = mean(q42b, na.rm = T),
              economic       = mean(q42c, na.rm = T),
              drugs          = mean(q42d, na.rm = T)) %>%
    ungroup() %>%
    drop_na() %>%
    mutate(health         = paste0(round(health*100,0), "%"),
           interpersonal  = paste0(round(interpersonal*100,0), "%"),
           economic       = paste0(round(economic*100,0), "%"),
           drugs          = paste0(round(drugs*100,0), "%"),
    ) 
  
  # Join both
  
  hardships_section <- merge(x = hardships_any, y = hardships, by = "country")
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Saving data                                                                              ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  list_of_datasets <- list("Section 1" = problems_section, 
                           "Section 2" = legal_section,
                           "Section 3" = help_section, 
                           "Section 4" = status_section,
                           "Section 5" = process_section,
                           "Section 6" = hardships_section)
  
  # dir.create(file.path("Outputs", 
  #                      paste0("tables")))
  if (!file.exists("Outputs")) {
    dir.create(file.path("Outputs", 
                         paste0("tables")))
  } 
  openxlsx::write.xlsx(list_of_datasets,
                       file = paste0("Outputs/A2J_tables_fin.xlsx"))
  
}

a2j_section <- legal_assigment(master_data.df)
a2j_tables <- a2j_tables.fn(a2j_section = a2j_section)

