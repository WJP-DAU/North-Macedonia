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
## A2J Tables and Plots                                                                                     ----
## 
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Tables                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

a2j_tables.fn <- function(a2j_section) {
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 1:  Legal problems                                                                         ----
  ###
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  problems_section <- a2j_section %>%
    group_by(country) %>%
    summarise(legal_problem = mean(legal, na.rm = T)) %>%
    ungroup() %>%
    filter(legal_problem > 0) %>%
    mutate(legal_problem = paste0(round(legal_problem*100,0), "%")) 
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 2:  Capability                                                                             ----
  ###
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  legal_section <- a2j_section %>%
    filter(legal == 1) %>%
    select(country, q41b, q41c, q41d) %>%
    mutate(
      across(!country,
             ~ case_when(
               .x == 1 ~ 1,
               .x == 2 ~ 1,
               .x == 3 ~ 0,
               .x == 4 ~ 0,
               .x == 99 ~ NA_real_
             )
      )) %>%
    group_by(country) %>%
    summarise(get_information = mean(q41b, na.rm = T),
              get_expert      = mean(q41c, na.rm = T),
              confidence      = mean(q41d, na.rm = T)) %>%
    ungroup() %>%
    mutate(get_information = paste0(round(get_information*100,0), "%"),
           get_expert = paste0(round(get_expert*100,0), "%"),
           confidence = paste0(round(confidence*100,0), "%"))
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 3:  Sources of help                                                                        ----
  ###
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  help_section <- a2j_section %>%
    filter(legal == 1) %>%
    select(country, q24) %>%
    mutate(sources_help = case_when(
      q24 == 1  ~ 1,
      q24 == 0  ~ 0,
      q24 == 99 ~ NA_real_
    )) %>%
    group_by(country) %>%
    summarise(source_help = mean(sources_help, na.rm = T)) %>%
    ungroup() %>%
    mutate(source_help =  paste0(round(source_help*100,0), "%"))
  
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 4:  Problem status                                                                         ----
  ###
  ### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  "%!in%" <- compose("!", "%in%")
  
  status_section <- a2j_section %>%
    filter(country %!in% c("Honduras", "Panama", "Guatemala", "Belize", "El Salvador")) %>%
    filter(legal == 1) %>%
    select(country, q34_merge) %>%
    mutate(q34_merge       = if_else(q34_merge != 99, q34_merge, NA_real_)) %>%
    mutate(fully_resolved  = if_else(q34_merge == 4, 1, 0),
           problem_persist = if_else(q34_merge == 3, 1, 0)) %>%
    group_by(country) %>%
    summarise(fully_resolved  = mean(fully_resolved, na.rm = T),
              problem_persist = mean(problem_persist, na.rm = T)) %>%
    ungroup() %>%
    mutate(fully_resolved =  paste0(round(fully_resolved*100,0), "%"),
           problem_persist = paste0(round(problem_persist*100,0), "%"))
  
  # Create q34 merge
  
  status_section_CA <- a2j_section %>%
    filter(country %in% c("Honduras", "Panama", "Guatemala", "Belize", "El Salvador")) %>%
    filter(legal == 1) %>%
    select(country, q30, q34) %>%
    mutate(q30       = if_else(q30 == 99,  NA_real_, q30),
           q34       = if_else(q34 == 99, NA_real_, q34)) %>%
    mutate(fully_resolved  = if_else(q30 == 4 | q34 == 4, 1, 0, 0),
           problem_persist = if_else(q30 == 3 | q34 == 3, 1, 0, 0)) %>%
    group_by(country) %>%
    summarise(fully_resolved  = mean(fully_resolved, na.rm = T),
              problem_persist = mean(problem_persist, na.rm = T)) %>%
    ungroup() %>%
    mutate(fully_resolved =  paste0(round(fully_resolved*100,0), "%"),
           problem_persist = paste0(round(problem_persist*100,0), "%"))
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 5:  Process                                                                              ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  process_section <- a2j_section %>%
    mutate(process = if_else(q34_merge == 3 | q34_merge == 4, 1, 0)) %>%
    filter(process == 1) %>%
    filter(legal == 1) %>%
    select(country, q36a, q37b, q37c, q37d, q34_merge, q28) %>%
    mutate(fair = case_when(
      q36a == 1 ~ 1,
      q36a == 0 ~ 0,
      q36a == 99 ~ NA_real_
    ),
    difficult_cost       = if_else(q37d == 3 | q37d == 4, 1, 
                                   if_else(q37d == 1 | q37d == 2, 0, NA_real_)),
    financial_difficulty = if_else(q37c == 1 & difficult_cost == 1, 1, 0),
    time                 = if_else(q37b >= 0 & q28 == 1, q37b, NA_real_)) %>%
    group_by(country) %>%
    summarise(fair                 = mean(fair, na.rm = T),
              time                 = mean(time, na.rm = T),
              financial_difficulty = mean(financial_difficulty, na.rm = T)) %>%
    ungroup() %>%
    mutate(fair =  paste0(round(fair*100,0), "%"),
           financial_difficulty = paste0(round(financial_difficulty*100,0), "%"))
  
  process_section_CA <-  a2j_section %>%
    filter(legal == 1) %>%
    filter(country %in% c("Honduras", "Panama", "Guatemala", "Belize", "El Salvador")) %>%
    select(country, q30, q34, q36a, EXP22_q43, q37c, q37d, q34_merge, q28) %>%
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
    group_by(country) %>%
    summarise(fair                 = mean(fair, na.rm = T),
              time                 = mean(time, na.rm = T),
              financial_difficulty = mean(financial_difficulty, na.rm = T)) %>%
    ungroup() %>%
    mutate(fair =  paste0(round(fair*100,0), "%"),
           financial_difficulty = paste0(round(financial_difficulty*100,0), "%"),
           timeMoreSixMonths = paste0(round(time*100,0), "%"))
  
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###    Section 6:  Hardships.                                                                              ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  hardships_section <- a2j_section %>%
    filter(legal == 1) %>%
    select(country, q42a, q42b, q42c, q42d) %>%
    mutate(
      across(!country,
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
    group_by(country) %>%
    summarise(any_hardship = mean(hardships, na.rm = T)) %>%
    mutate(any_hardship = paste0(round(any_hardship*100,0), "%"))
  
  # Hardships divided
  
  hardships <- hardships_section %>%
    group_by(country) %>%
    summarise(health         = mean(q42a, na.rm = T),
              interpersonal  = mean(q42b, na.rm = T),
              economic       = mean(q42c, na.rm = T),
              drugs          = mean(q42d, na.rm = T)) %>%
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
                       file = paste0("Outputs/A2J_tables.xlsx"))
  
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##   Plots                                                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

a2j_plot1.fn <- function(a2j_section){
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###   First page                                                                                        ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  a2j_a <- a2j_section %>%
    select(starts_with("a2j_")) %>%  
    rename(
      "Accidental Illness & <br> Injury"   = "a2j_accidental",
      "Citizenship & ID"                   = "a2j_id",
      "Community & Natural <br> Resources" = "a2j_community",
      "Consumer"                           = "a2j_consumer",
      "Employment"                         = "a2j_employment",
      "Education"                          = "a2j_education",
      "Family"                             =  "a2j_family",
      "Housing"                            = "a2j_housing",
      "Land"                               = "a2j_land",
      "Law Enforcement"                    = "a2j_law",
      "Money & Debt"                       = "a2j_money",
      "Public Services"                    = "a2j_public"
    ) %>%
    summarise(
      across(everything(),
             mean, na.rm = T)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "problem", values_to = "value") %>%
    arrange(value)
  
  ### Bars organization by panels 
  
  first_panel <- a2j_a %>%
    top_n(n = 4) %>%
    aes_function(.) %>%
    arrange(-value)
  
  a2j_p1 <- horizontal_edgebars(data2plot    = first_panel,
                                y_value      = value,
                                x_var        = problem,
                                group_var    = group,
                                label_var    = label,
                                x_lab_pos    = x_pos,
                                y_lab_pos    = 0,
                                bar_color    = "#2a2a94",
                                margin_top   = 20)
  
  second_panel <- a2j_a[5:8,] %>%
    aes_function(.)
  
  a2j_p2 <- horizontal_edgebars(data2plot    = second_panel,
                                y_value      = value,
                                x_var        = problem,
                                group_var    = group,
                                label_var    = label,
                                x_lab_pos    = x_pos,
                                y_lab_pos    = 0,
                                bar_color    = "#2a2a94",
                                margin_top   = 20)
  
  third_panel <- a2j_a[1:4,] %>%
    aes_function(.)
  
  a2j_p3 <- horizontal_edgebars(data2plot    = third_panel,
                                y_value      = value,
                                x_var        = problem,
                                group_var    = group,
                                label_var    = label,
                                x_lab_pos    = x_pos,
                                y_lab_pos    = 0,
                                bar_color    = "#2a2a94",
                                margin_top   = 20)
  
  figures_problems<- list()
  figures_problems[["Panel A"]] <- a2j_p1
  figures_problems[["Panel B"]] <- a2j_p2
  figures_problems[["Panel C"]] <- a2j_p3
  
  figure_A2J_a <- figures_problems[["Panel A"]] + figures_problems[["Panel B"]] + figures_problems[["Panel C"]] +
    plot_layout(ncol = 3,
                nrow = 1,
                widths = unit(38, "mm"),
                heights = unit(63, "mm"))
  
  # Saving Patchwork
  
  ggsave(plot = figure_A2J_a, 
         filename = paste0("Outputs/imgChart19/figure_19.svg"), 
         width = 131.7974, 
         height = 65.02006,
         units  = "mm",
         dpi    = 72,
         device = "svg")
  
}

a2j_plot2.fn <- function(a2j_section){
  
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ###
  ###   Second page                                                                                        ----
  ###
  ### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  a2j_b <- a2j_section %>%
    filter(legal == 1) %>%
    rowwise() %>%
    mutate(helpSum = sum(q25_1,q25_2,q25_3,q25_4,q25_6,q25_7,q25_8,q25_9,q25_99, na.rm = T)) %>%
    ungroup() %>%
    mutate(filtro_dk = if_else(q25_99 == 1 & helpSum == 1, 1, 0, 0)) %>%
    #filter(q25_99 != 1) %>%
    filter(filtro_dk != 1) %>%
    summarise(
      "Friend or Family"                              = mean(q25_1, na.rm = T),
      "Lawyer or Professional <br> Advice Service"    = mean(q25_2, na.rm = T),
      "Government <br> Legal Aid Office"              = mean(q25_3, na.rm = T),
      "Court or Government <br> Body or Police"       = mean(q25_4, na.rm = T),
      "Health or Welfare <br> Professional"           = mean(q25_5, na.rm = T),
      "Trade Union or <br> Employer"                  = mean(q25_6, na.rm = T),
      "Religious or <br> Community Leader"            = mean(q25_7, na.rm = T),
      "Civil Society <br> Organization"               = mean(q25_8, na.rm = T),
      "Other Organization"                            = mean(q25_9, na.rm = T)) %>%
    pivot_longer(cols = everything(),
                 names_to = "help", values_to = "value") %>%
    arrange(-value)
  
  first_panel <- a2j_b[1:3,] %>%
    aes_function(panel = "v2")
  
  a2j_p1 <- horizontal_edgebars(data2plot    = first_panel,
                                y_value      = value,
                                x_var        = help,
                                group_var    = group,
                                label_var    = label,
                                x_lab_pos    = x_pos,
                                y_lab_pos    = 0, 
                                margin_top   = 10,
                                bar_color    = "#2a2a94")
  
  second_panel <- a2j_b[4:6,] %>%
    aes_function(panel = "v2")
  
  a2j_p2 <- horizontal_edgebars(data2plot    = second_panel,
                                y_value      = value,
                                x_var        = help,
                                group_var    = group,
                                label_var    = label,
                                x_lab_pos    = x_pos,
                                y_lab_pos    = 0,
                                margin_top   = 10,
                                bar_color    = "#2a2a94")
  
  third_panel <- a2j_b[7:9,] %>%
    aes_function(panel = "v2")
  
  a2j_p3 <- horizontal_edgebars(data2plot    = third_panel,
                                y_value      = value,
                                x_var        = help,
                                group_var    = group,
                                label_var    = label,
                                x_lab_pos    = x_pos,
                                y_lab_pos    = 0,
                                margin_top   = 10,
                                bar_color    = "#2a2a94")
  
  figures_problems<- list()
  figures_problems[["Panel A"]] <- a2j_p1
  figures_problems[["Panel B"]] <- a2j_p2
  figures_problems[["Panel C"]] <- a2j_p3
  
  figure_A2J_b <- figures_problems[["Panel A"]] + figures_problems[["Panel B"]] + figures_problems[["Panel C"]] +
    plot_layout(ncol = 3,
                nrow = 1,
                widths = unit(38, "mm"),
                heights = unit(63, "mm"))
  
  ggsave(plot = figure_A2J_b, 
         filename = paste0("Outputs/imgChart20/figure_20.svg"), 
         width = 131.7974, 
         height = 65.02006,
         units  = "mm",
         dpi    = 72,
         device = "svg")
}

### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###
###   Justice Gap                                                                                            ----
###
### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

jgap.fn <- function(data = master_data.df){
  
  # Estimating the Justice Gap
  data2plot <- data %>%
    filter(year == latestYear) %>%
    select(country, 
           starts_with("q20_"),
           age, gend, fin, COLOR, relig, ethni, edu, Urban,
           q21, q36a, q41b, q24, starts_with("q25_"), q37c, q37d, q37b, q34_merge) %>%
    mutate(
      unsatis_fair = case_when(
        q36a == 0 ~ 1,
        q36a == 1 ~ 0
      ),
      unsatis_info = case_when(
        q41b == 3 | q41b == 4 ~ 1,
        q41b == 1 | q41b == 2 ~ 0
      ),
      unsatis_profhelp = case_when(  # Remember that case_when() works with a hierarchy system
        q24 == 1 & q25_2 == 1 ~ 0,
        q24 == 1 & q25_3 == 1 ~ 0,
        q24 == 1 & q25_4 == 1 ~ 0,
        q24 == 1 & q25_8 == 1 ~ 0,
        (q24 == 1 | q24 == 2 ) & q25_99 != 1 ~ 1
      ),
      unsatis_cost = case_when(
        (q37c != 99) & (q37d == 3 | q37d == 4) ~ 1,
        (q37c != 99) & q37d != 3 & q37d != 4 & q37d != 99 ~ 1
      ),
      unsatis_time = case_when(
        q37b >= 12 ~ 1,
        q37b <  12 ~ 1
      ),
      unsatis_persistance = case_when(
        q34_merge == 3 ~ 1,
        q34_merge == 4 ~ 0
      )
    ) %>%
    rowwise() %>%
    mutate(
      comp4th = mean(c(unsatis_time, unsatis_cost, unsatis_fair), na.rm = T),
      a2j_idx = mean(c(unsatis_info, unsatis_profhelp, unsatis_persistance, comp4th), na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      selected_problem = if_else(q21 != "", 
                                 paste0("q20_", q21), 
                                 NA_character_),
      severity = NA_real_ # we need an empty variable for now
    )
  
  # Severity is created dynamically depending on which legal problem was selected
  for (i in 1:nrow(data2plot)) {
    if (!is.na(data2plot$selected_problem[i])){
      data2plot$severity[i] <- data2plot[i, data2plot$selected_problem[i]]
    }
  }
  
  # Wrangling the data
  data2plot <- data2plot %>%
    select(
      country, 
      age, gend, fin, COLOR, relig, ethni, edu, Urban,
      selected_problem, starts_with("unsatis_"), comp4th, a2j_idx, severity
    ) %>%
    mutate(
      across(
        c(comp4th, a2j_idx),
        ~if_else(severity < 4, NA_real_, .x)
      ),
      nbarriers = case_when(
        a2j_idx <= 0.25  ~ "0-1 Barriers",
        a2j_idx >  0.25 & a2j_idx <= 0.50  ~ "1-2 Barriers",
        a2j_idx >  0.50 & a2j_idx <= 0.75  ~ "2-3 Barriers",
        a2j_idx >  0.75 ~ "3-4 Barriers"
      ),
      within_jgap = case_when(
        a2j_idx >  0.65 ~ 1,
        a2j_idx <= 0.65 ~ 0
      )
    )
  
  return(data2plot)
}

jgap_bars.fn <- function(data, nchart = 28){
  
  # Preparing the data
  data2plot_1 <- data %>%
    group_by(country, within_jgap) %>%
    summarise(
      count = n()
    ) %>%
    rename(category = within_jgap) %>%
    mutate(
      category = case_when(
        category == 0 ~ "No",
        category == 1 ~ "Yes"
      )
    ) %>%
    filter(!is.na(category)) %>%
    mutate(ntotal = sum(count, na.rm = T))
  
  data2plot_2 <- data %>%
    group_by(country, nbarriers) %>%
    summarise(
      count = n()
    ) %>%
    rename(category = nbarriers) %>%
    filter(!is.na(category)) %>%
    mutate(ntotal = sum(count, na.rm = T))
  
  data2plot <- bind_rows(data2plot_1, data2plot_2) %>%
    mutate(
      group = case_when(
        category %in% c("No", "Yes") ~ "Within\nJustice Gap",
        TRUE ~ "Number of\nBarriers",
      ),
      value2plot = count/ntotal,
      category = factor(category,
                        c("Yes", "No",
                          "3-4 Barriers", "2-3 Barriers", "1-2 Barriers", "0-1 Barriers"))
    )
  
  # Drawing chart
  chart <- ggplot(data = data2plot) +
    geom_col(aes(x    = group,
                 y    = value2plot,
                 fill = category),
             width    = 0.75, 
             position = "stack") +
    geom_text(aes(x    = group,
                  y    = value2plot,
                  label = paste0(round(value2plot,2)*100,"%")),
              position = position_stack(vjust = .5),
              color    = "white",
              family   = "Lato Full",
              fontface = "bold", 
              size = 3.514598) +
    scale_fill_manual(values = c("Yes" = "#fa4d57",
                                 "No"  = "#003B88",
                                 "0-1 Barriers" = "#dfdeff",
                                 "1-2 Barriers" = "#9b9bd3",
                                 "2-3 Barriers" = "#5d61ad",
                                 "3-4 Barriers" = "#000066")) +
    scale_y_continuous(position = "right",
                       limits = c(0, 1.05),
                       breaks = seq(0, 1.05, 0.2),
                       labels = paste0(seq(0, 1.05, 0.2)*100, "%"),
                       expand = c(0,0)) +
    coord_flip() +
    WJP_theme() +
    theme(
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),
      axis.line.x        = element_blank(),
      legend.position    = "none",
      panel.grid.major.y = element_blank()
    )
  
  # Saving the chart
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = "A",
            w   = 189.7883,
            h   = 64)
    
}

jgap_logit.fn <- function(data, nchart = 28){
  
  data_subset.df <- jgap_data.df %>%
    mutate(
      skinColor  =
        case_when(
          COLOR == 1 | COLOR == 2 | COLOR == 3 | COLOR == 4 ~ "ZWhite",
          COLOR == 5 | COLOR == 6 | COLOR == 7 | COLOR == 8 | COLOR == 9 | COLOR == 10 | COLOR == 11 ~ "No White",
          T ~ NA_character_
        ),
      ageUnder30 =
        case_when(
          age < 30 & age >= 18 ~ "ZLess than 30 years",
          age >= 30            ~ "More than 30 years",
          T ~ NA_character_
        ),
      economicStatus = 
        case_when(
          fin == 1 | fin == 2 ~ "ZLow Economic Status",
          fin == 3 | fin == 4 | fin == 5 ~ "High Economic Status"
        ),
      areaType =
          case_when(
            Urban == 1 ~ "ZUrban",
            Urban == 2 ~ "Rural",
            T ~ NA_character_
          ),
      gender = 
        case_when(
          gend == 1 ~ "Male",
          gend == 2 ~ "ZFemale",
          T ~ NA_character_
        ),
      education =
        case_when(
          edu == 4 | edu == 5 | edu == 6| edu == 7 ~ "High Education Level",
          edu == 3 | edu == 2 | edu == 1 ~ "ZLow Education Level",
          T ~ NA_character_
        ),
      etnhicity = 
        if_else(
          ethni == "Macedonian", "ZMacedonian",
          if_else(
          ethni == "Albanian", "Albanian", NA_character_)
        )
    )
  
  condition <-  data_subset.df %>%
    select(skinColor, ageUnder30, economicStatus, areaType, gender, education, etnhicity) %>%
    mutate(counter = 1)
  
  skinColor       <- condition_categories(main_data = condition, group_var = skinColor, name_var = "skinColor")
  ageUnder30      <- condition_categories(main_data = condition, group_var = ageUnder30, name_var = "ageUnder30")
  economicStatus  <- condition_categories(main_data = condition, group_var = economicStatus, name_var = "economicStatus")
  areaType        <- condition_categories(main_data = condition, group_var = areaType, name_var = "areaType")
  gender          <- condition_categories(main_data = condition, group_var = gender, name_var = "gender")
  education       <- condition_categories(main_data = condition, group_var = education, name_var = "education")
  etnhicity       <- condition_categories(main_data = condition, group_var = etnhicity, name_var = "etnhicity")
  
  selectables <- bind_rows(skinColor, ageUnder30, economicStatus, areaType, gender, education, etnhicity) %>%
    group_by(variable) %>%
    summarise(min_group = min(N_obs, na.rm = T),
              total_group = sum(N_obs, na.rm = T)) %>%
    filter(min_group > 30) %>%
    filter(min_group != total_group) %>%
    pull(variable)
  
  formula <- selectables %>%
    t() %>%
    as.data.frame() %>%
    unite(., formula, sep = "+") %>%
    as.character()
  
  depVar <- "within_jgap"
  
  formula  <- as.formula(paste(depVar, "~", formula))
  
  logit    <- glm(formula,
                  data   = data_subset.df, 
                  family = "binomial")
  
  summaryLogit <- bind_rows(
    as.data.frame(coef(logit))
  )
  
  margEff      <- as.data.frame(
    margins_summary(logit, data = logit$data)
  )
  
  margEff$factor <-recode(margEff$factor,
                          "genderZFemale"                                = "Female",
                          "areaTypeZUrban"                               = "Urban",
                          "educationZLow Education Level"                = "No high school \ndiploma",
                          "ageUnder30ZLess than 30 years"                = "Younger than 30",
                          "etnhicityZMacedonian"                         = "Ethnic Macedonian \nbackground",
                          "skinColorZWhite"                              = "Light skin tone",
                          "economicStatusZLow Economic Status"           = "Low economic \nstatus")

  data2plot <- margEff %>%
    mutate(order_variable =
             case_when(
               factor == "Female"                              ~ 1,
               factor == "Urban"                               ~ 2,
               factor == "No high school \ndiploma"            ~ 3,
               factor == "Younger than 30"                     ~ 4,
               factor == "Ethnic Macedonian \nbackground"      ~ 5,
               factor == "Light skin tone"                     ~ 6,
               factor == "Low economic \nstatus"               ~ 7
             ),
           dependent_var  = depVar
    )
  
  logit_plot <- logit_demo_panel(mainData = data2plot, line_size = 1.5)
  
  saveIT.fn(chart  = logit_plot,
            n      = nchart,
            suffix = "B",
            w      = 175.027,
            h      = 80)
  
  }



