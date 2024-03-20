## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            North Macedonia Country Report - Section I Functions
##
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
##                    A. Santiago Pardo G.        (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     November 13th, 2023
##
## This version:      November 13th, 2023
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Authoritarian Behaviors (PAB)                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
figure_PAB.fn <- function(nchart = 1, data = master_data.df) {
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Independent" = c("CAR_q67_G1", "CAR_q67_G2", "CAR_q68_G1", "CAR_q61_G1"),
    "Judiciary"   = c("CAR_q66_G1", "CAR_q65_G1", "CAR_q64_G1"),
    "Media"       = c("CAR_q64_G2", "CAR_q60_G2", "CAR_q65_G2", "CAR_q60_G1")
  )
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(unlist(vars4plot, use.names = FALSE)) %>%
    mutate(
      across(everything(),
             ~ case_when(
               .x == 1  ~ "Strongly agree",
               .x == 2  ~ "Agree",
               .x == 3  ~ "Disagree",
               .x == 4  ~ "Strongly disagree",
               .x == 99 ~ "DK/NA"
             ))
    ) %>%
    pivot_longer(everything(),
                 names_to   = "variable",
                 values_to  = "statement") %>%
    group_by(variable, statement) %>%
    summarise(count = n())
  
  # Splitting DK/NA
  # data2plot <- data2plot %>%
  #   bind_rows(
  #     data2plot %>%
  #       filter(statement %in% c("Don't know (positive)")) %>%
  #       mutate(statement = "Don't know (negative)")
  #   ) %>%
  #   arrange(variable, statement)
  
  # Labeling and percentages
  data2plot <- data2plot %>%
    filter(!is.na(statement)) %>%
    group_by(variable) %>%
    mutate(
      n = sum(count),
      perc = count / n,
      value2plot  = perc * 100,
      labels = case_when(
        variable == "CAR_q60_G1" ~ "Censor information that comes \nfrom abroad",
        variable == "CAR_q61_G1" ~ "Censor opinions from opposition \ngroups",
        variable == "CAR_q60_G2" ~ "Resort to misinformation to shape \npublic opinion in their favor",
        variable == "CAR_q64_G2" ~ "Attack or attempt to discredit the \nmedia and civil society organizations\nthat criticize them",
        variable == "CAR_q67_G1" ~ "Attack or attempt to discredit \nopposition parties",
        variable == "CAR_q67_G2" ~ "Attack or attempt to discredit the \nelectoral system and other \nsupervisory organs", 
        variable == "CAR_q64_G1" ~ "Seek to limit the courts' competencies \nand freedom to interpret the law",
        variable == "CAR_q66_G1" ~ "Seek to influence the promotion and \nremoval of judges",
        variable == "CAR_q65_G2" ~ "Prosecute and convict journalists and \nleaders of civil society organizations     ",
        variable == "CAR_q68_G1" ~ "Prosecute and convict members of\nopposition parties                                   ",
        variable == "CAR_q65_G1" ~ "Refuse to comply with court rulings \nthat are not in their favor"
      ),
      # order_value = case_when(
      #   variable == "CAR_q60_G1" ~ 1,
      #   variable == "CAR_q61_G1" ~ 1,
      #   variable == "CAR_q60_G2" ~ 1,
      #   variable == "CAR_q64_G2" ~ 2,
      #   variable == "CAR_q67_G1" ~ 2,
      #   variable == "CAR_q67_G2" ~ 2,
      #   variable == "CAR_q64_G1" ~ 3,
      #   variable == "CAR_q66_G1" ~ 3,
      #   variable == "CAR_q65_G2" ~ 3,
      #   variable == "CAR_q68_G1" ~ 4,
      #   variable == "CAR_q65_G1" ~ 4
      # ),
      statement = factor(statement,
                         levels = c("Strongly agree", "Agree", "DK/NA", "Disagree", "Strongly disagree")),
      order = 
        case_when(
          statement %in% "Strongly agree" ~ 1,
          statement %in% "Agree" ~ 2,
          statement %in% "DK/NA" ~ 3,
          statement %in% "Disagree" ~ 4,
          statement %in% "Strongly disagree" ~ 5
        ),
      value_label = if_else(value2plot > 5,paste0(round(value2plot,0), "%"), "")
      )
  
  order_value <- data2plot %>%
    group_by(variable, statement) %>%
    summarise(order_value = sum(perc, na.rm = T)) %>%
    filter(statement %in% c("Agree", "Strongly agree")) %>%
    group_by(variable) %>%
    summarise(order_value = sum(order_value, na.rm = T)) %>%
    mutate(order_value = 1 - order_value)
  
  data2plot <- data2plot %>%
    left_join(y = order_value, by = "variable") %>%
    group_by(variable) %>%
    arrange(variable, -order) %>%
    mutate(stack_y = cumsum(value2plot) - (value2plot/2))
  
  # Defining color palette
  colors4plot <- c("Strongly agree"    = "#FA4D57", 
                   "Agree"             = "#FC9096", 
                   "DK/NA"             = "#E2E2E2", 
                   "Disagree"          = "#6783B7", 
                   "Strongly disagree" = "#003B88")
  
  # Saving data points
  data_PAB = data2plot %>% ungroup()
  
  # Plotting each panel of Figure 12
  imap(c("A" = "Independent",
         "B" = "Judiciary",
         "C" = "Media"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(variable %in% vars4plot[[varSet]])
         
         # Applying plotting function
         chart <- LAC_barsChart(data           = data2plot,
                                target_var     = "value2plot",
                                grouping_var   = "labels",
                                labels_var     = "value_label",
                                colors_var     = "statement",
                                colors         = colors4plot,
                                direction      = "horizontal",
                                stacked        = TRUE,
                                lab_pos        = "stack_y",
                                custom_order   = TRUE,
                                order_var      = "order_value")
         
         # Defining height
         if (length(vars4plot[[varSet]]) == 3 ) {
           h = 47.44707
         }
         
         if (length(vars4plot[[varSet]]) == 4 ) {
           h = 56.23357
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 189.7883,
                   h      = h)
       })
  
  return(data_PAB)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Authoritarian Behavior, by Support for the Current Administration and religion (PABGS) ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_PABGS.fn <- function(nchart = 2, data = master_data.df, group = "religion") {
  
  # Defining variables to use in the plot
  vars4plot <- list(
    "Independent" = c("CAR_q67_G1", "CAR_q67_G2", "CAR_q68_G1", "CAR_q61_G1"),
    "Judiciary"   = c("CAR_q66_G1", "CAR_q65_G1", "CAR_q64_G1"),
    "Media"       = c("CAR_q64_G2", "CAR_q60_G2", "CAR_q65_G2", "CAR_q60_G1")
  )
  
  #Defining data
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(relig, CAR_q59_G1, CAR_q59_G2, unlist(vars4plot, use.names = FALSE)) %>%
    mutate(
      religr = case_when(
        relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
        relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
      ),
      govSupp = case_when(
        !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
        CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
        CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
        CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
        is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_
      ),
      across(!c(relig, religr, govSupp),
             ~ if_else(.x == 1 | .x == 2, 1,
                       if_else(!is.na(.x) & .x != 99, 0, 
                               NA_real_)))
    ) 
    
  if(group == "religion") {
      
      data2plot <- data2plot %>%
        select(!c(CAR_q59_G1, CAR_q59_G2, relig, govSupp)) %>%
        filter(!is.na(religr)) %>%
        pivot_longer(!c(religr), names_to = "category", values_to = "value") %>%
        group_by(religr, category) %>%
        summarise(
          mean_value = mean(value, na.rm = TRUE),
          sd_value = sd(value, na.rm = TRUE),
          n_obs = n()
        ) %>%
        ungroup()
        
    } else {
      
      data2plot <- data2plot %>%
        select(!c(CAR_q59_G1, CAR_q59_G2, relig, religr)) %>%
        filter(!is.na(govSupp)) %>%
        pivot_longer(!c(govSupp), names_to = "category", values_to = "value") %>%
        group_by(govSupp, category) %>%
        summarise(
          mean_value = mean(value, na.rm = TRUE),
          sd_value = sd(value, na.rm = TRUE),
          n_obs = n()
        ) %>%
        ungroup()
    }
    
  data2plot <- data2plot %>%
    mutate(
      labels = case_when(
        category == "CAR_q60_G1" ~ "Censor information that comes \nfrom abroad",
        category == "CAR_q61_G1" ~ "Censor opinions from opposition \ngroups",
        category == "CAR_q60_G2" ~ "Resort to misinformation to shape \npublic opinion in their favor",
        category == "CAR_q64_G2" ~ "Attack or attempt to discredit the \nmedia and civil society organizations\nthat criticize them",
        category == "CAR_q67_G1" ~ "Attack or attempt to discredit \nopposition parties",
        category == "CAR_q67_G2" ~ "Attack or attempt to discredit the \nelectoral system and other \nsupervisory organs", 
        category == "CAR_q64_G1" ~ "Seek to limit the courts' competencies \nand freedom to interpret the law",
        category == "CAR_q66_G1" ~ "Seek to influence the promotion and \nremoval of judges",
        category == "CAR_q65_G2" ~ "Prosecute and convict journalists and \nleaders of civil society organizations     ",
        category == "CAR_q68_G1" ~ "Prosecute and convict members of\nopposition parties                                   ",
        category == "CAR_q65_G1" ~ "Refuse to comply with court rulings \nthat are not in their favor"
        
      ),
      mean_value = mean_value * 100,
      sd_value = sd_value*100,
      order_value = case_when(
        category == "CAR_q60_G1" ~ 4,
        category == "CAR_q61_G1" ~ 4,
        category == "CAR_q60_G2" ~ 1,
        category == "CAR_q64_G2" ~ 2,
        category == "CAR_q67_G1" ~ 1,
        category == "CAR_q67_G2" ~ 3, 
        category == "CAR_q64_G1" ~ 3,
        category == "CAR_q66_G1" ~ 1,
        category == "CAR_q65_G2" ~ 3,
        category == "CAR_q68_G1" ~ 2,
        category == "CAR_q65_G1" ~ 2
      )
    )
  
  if(group == "religion") {
    
    # Defining color palette
    colors4plot <- c("Orthodox Christian" = "#a90099", 
                     "Sunni Muslim"       = "#3273ff")
  } else{
    # Defining color palette
    colors4plot <- c("Non Gov. Supporter" = "#a90099", 
                     "Gov. Supporter"       = "#3273ff")
  }
  
  # Saving data points
  data_PABGS = data2plot %>% ungroup()
  
  # Plotting each panel of Figure
  imap(c("A" = "Independent", 
         "B" = "Judiciary", 
         "C" = "Media"),
       function(varSet, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% vars4plot[[varSet]])
         
         if(group == "religion") {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "mean_value",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "religr",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
         } else {
           
           # Applying plotting function
           chart <- NM_dotsChart(data         = data2plot,
                                 target_var   = "mean_value",
                                 sd_var       = "sd_value",
                                 n_obs        = "n_obs", 
                                 alpha        = 0.05,
                                 grouping_var = "govSupp",
                                 labels_var   = "labels",
                                 colors       = colors4plot,
                                 order_var    = "order_value")
           
         }
         
         # Defining height
         if (length(vars4plot[[varSet]]) == 3 ) {
           h = 47.44707
         }
         
         if (length(vars4plot[[varSet]]) == 4 ) {
           h = 56.23357
         }
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = paste0(panelName, "_", group),
                   w      = 189.7883,
                   h      = h)
       })
  
  return(data_PABGS)
}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Attitudes Towards Authoritarianism and Rule of Law (AROL)                                             ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_AROL.fn <- function(nchart = 3, data = master_data.df) {
  
  # Defining variables to use
  vars4plot <- c("q50", "q51", "q52", "CAR_q73", "CAR_q74")
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry & year == latestYear) %>%
    select(all_of(vars4plot)) %>%
    mutate(
      q52 = case_when(
        q52 == 1 ~ 4,
        q52 == 2 ~ 3,
        q52 == 3 ~ 2,
        q52 == 4 ~ 1,
        q52 == 5 ~ 5,
        q52 == 99 ~ 99
      ),
      across(everything(),
             ~case_when(
               .x == 3 | .x == 4 ~ "Positive",
               .x < 3   ~ "Negative",
               .x %in% c(5, 99)  ~ "Neutral/No answer"
             )),
      across(everything(),
             \(x) factor(x, levels = c("Positive", "Neutral/No answer", "Negative")))
      
    )
  
  # Defining colors for plot
  colors4plot <- c("#003B88","#B5B5B5", "#fa4d57")
  names(colors4plot) <- c("Positive", "Neutral/No answer", "Negative")
  
  
  # Plotting each figure panel
  names(vars4plot) <- c("A", "B", "C", "D", "E")
  imap(vars4plot,
       function(var, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           rename(category = all_of(var)) %>%
           group_by(category) %>%
           summarise(count = n()) %>%
           ungroup() %>%
           mutate(
             n          = sum(count),
             value2plot = (count/n) *100,
             ymax       = cumsum(value2plot),
             ymin       = ymax-value2plot,
             label      = to_percentage.fn(value2plot),
             labpos     = ymin  + ((ymax-ymin)/2)
           )
         
         chart <- ggplot(data2plot, 
                         aes(fill = category, 
                             ymax = ymax, 
                             ymin = ymin, 
                             xmax = 2, 
                             xmin = 1)) + 
           geom_rect() + 
           geom_text(aes(label = label,
                         y     = labpos,
                         x     = 1.5),
                     color     = "white",
                     size      = 0.866058*.pt,
                     family    = "Lato Full",
                     fontface  = "bold") +
           scale_x_continuous(limits = c(0,2)) +
           scale_y_continuous(limits = c(0,200)) +
           scale_fill_manual(values  = colors4plot) +
           coord_polar(theta = "y",
                       start = -pi/2) +
           WJP_theme() +
           labs(y = "",
                x = "") +
           theme(
             legend.position  = "none",
             panel.grid.major = element_blank(),
             axis.title.x     = element_blank(),
             # axis.title.y     = element_blank(),
             axis.text.x      = element_blank(),
             axis.text.y      = element_blank()
           )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 63.26276, 
                   h      = 63.26276)
         
         return(data2plot)
       })

}



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Fundamental Freedoms Over Time (FFOT)                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

figure_FFOT.fn <- function(nchart = 4, data = master_data.df) {
  
  # Defining data frame for plot
  data2plot <- data %>%
    filter(country == mainCountry) %>%
    select(year,
           q46c_G2, q46f_G2, q46g_G2, q46c_G1, q46e_G2,
           q46d_G2, q46f_G1, q46a_G2,
           q46d_G1, q46e_G1, q46h_G2) %>%
    mutate(
      across(!c(year),
             ~ case_when(
               .x == 1 | .x == 2 ~ 1,
               .x == 3 | .x == 4 ~ 0
             ))
    ) %>%
    group_by(year) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    pivot_longer(!year,
                 names_to  = "category",
                 values_to = "value") %>%
    mutate(value = value*100,
           label = paste0(format(round(value, 0),
                                 nsmall = 0),
                          "%")) %>%
    filter(year >= 2014)
  
  # Pulling minimum and maximum available year
  minyear <- 2013
  maxyear <- 2023
  
  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))
  
  # Saving data points
  data_FFOT = data2plot %>% ungroup()
  
  # Plotting each panel of Figure 5
  imap(c("A" = "q46c_G2", "B" = "q46f_G2", "C" = "q46g_G2", "D" = "q46c_G1", "E" = "q46e_G2",
         "F" = "q46d_G2", "G" = "q46f_G1", "H" = "q46a_G2",
         "I" = "q46d_G1", "J" = "q46e_G1", "K" = "q46h_G2"),
       function(var4plot, panelName) {
         
         # Filtering data2plot to leave the variable for each panel
         data2plot <- data2plot %>%
           filter(category %in% var4plot)
         
         # Defining colors4plot
         colors4plot <- mainCOLOR
         names(colors4plot) <- var4plot
         
         # Applying plotting function
         chart <- LAC_lineChart(data           = data2plot,
                                target_var     = "value",
                                grouping_var   = "year",
                                ngroups        = 1, 
                                labels_var     = "label",
                                colors_var     = "category",
                                colors         = colors4plot,
                                repel          = F,
                                custom.axis    = T,
                                x.breaks       = x.axis.values,
                                x.labels       = x.axis.labels,
                                sec.ticks      = sec.ticks
         )
         
         # Saving panels
         saveIT.fn(chart  = chart,
                   n      = nchart,
                   suffix = panelName,
                   w      = 90.67663,
                   h      = 45.68977)
         
       })
  
  return(data_FFOT)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Fundamental Freedoms, by Support for the Current Administration and religion (FFD)                                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++   

c("ethnigroup" = "Macedonian",
  "relgroup"   = "Christian")

figure_FFD.fn <- function(nchart = 5, data = master_data.df, group = "religion"){
  
 "%!in%" <- compose("!", "%in%")
  
  # Defining variables to include in plot
  vars4plot <- list("Expression"    = c("q46c_G2", "q46f_G2", "q46g_G2", "q46c_G1", "q46e_G2"),
                   "Participation" = c("q46d_G2", "q46f_G1", "q46a_G2"),
                   "Election"      = c("q46d_G1", "q46e_G1"),
                   "Religion"      = c("q46h_G2"))
  
  # Defining data frame for plot
  data2plot <- data %>%
    mutate(
      religr = case_when(
        relig %in% c("C57 - Orthodox Christian") ~ "Orthodox Christian",
        relig %in% c("G6 - Sunni Muslim")        ~ "Sunni Muslim"
    ),
    govSupp = case_when(
      !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
      CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
      CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
      CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
      is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_
    ))
  
  if(group == "religion"){
    data2plot <- data2plot %>%
      rename(gvar = religr)
    
  } else {
    data2plot <- data2plot %>%
      rename(gvar = govSupp)
  }
  
  data2plot <- data2plot %>%
    filter(year == latestYear & country == mainCountry) %>%
    select(gvar, all_of(unlist(vars4plot, use.names = F))) %>%
    mutate(
      across(!gvar,
             ~if_else(.x == 1 | .x == 2, 1, 
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_)))) %>%
    group_by(gvar) %>%
    summarise(across(everything(),
                     mean,
                     na.rm = T)) %>%
    filter(!is.na(gvar)) %>%
    pivot_longer(!gvar,
                 names_to   = "category",
                 values_to  = "value2plot") %>%
    mutate(
      order_value = case_when(
        category     == 'q46c_G2' ~ 1,
        category     == 'q46f_G2' ~ 2,
        category     == 'q46g_G2' ~ 3,
        category     == 'q46c_G1' ~ 4,
        category     == 'q46e_G2' ~ 5,
        category     == 'q46d_G2' ~ 6,
        category     == 'q46f_G1' ~ 7,
        category     == 'q46a_G2' ~ 8,
        category     == 'q46d_G1' ~ 9,
        category     == 'q46e_G1' ~ 10,
        category     == 'q46h_G2' ~ 11,
      ),
      label = case_when(
        category == 'q46c_G2'       ~ paste("**People** can <br> express opinions<br>against the government"),
        category == 'q46f_G2'       ~ paste("**Civil society** <br>organizations can <br> express opinions",
                                            "against<br>the government"),
        category == 'q46g_G2'       ~ paste("**Political parties**<br>can express opinions<br>",
                                            "against the<br>government"),
        category == 'q46c_G1'       ~ paste("**The media**<br>can express opinions<br>",
                                            "against the<br>government"),
        category == 'q46e_G2'       ~ paste("The media<br>can **expose cases<br>of corruption**"),
        category == 'q46d_G2'       ~ paste("People can<br>**attend community<br>meetings**"),
        category == 'q46f_G1'       ~ paste("People can<br>**join any political<br>organization**"),
        category == 'q46a_G2'       ~ paste("People can<br>**organize around an<br>issue or petition**"),
        category == 'q46d_G1'       ~ paste("Local government<br>officials **are elected<br>through a clean<br>process**"),
        category == 'q46e_G1'       ~ paste("People can<br>**vote freely** without<br>feeling harassed<br>or pressured"),
        category == 'q46h_G2'       ~ paste("Religious minorities<br>can **observe their<br>holy days**"),
      ),
      across(label,
             ~paste0("<span style='color:#524F4C;font-size:3.514598mm;font-weight:bold'>",
                     label,
                     "</span>")),
      label = if_else(gvar %!in% c("Orthodox Christian", "Non Gov. Supporter"),
                      NA_character_, 
                      label)
      ) %>%
    rename(year = gvar)
  
  # Defining color palette
  if(group == "religion") {
    
    data2plot <- data2plot %>%
      mutate(
        latestYear = "Orthodox Christian"
        )
    
    colors4plot <- c("Orthodox Christian" = "#a90099", 
                     "Sunni Muslim"       = "#3273ff")
    
  } else {
    
    data2plot <- data2plot %>%
      mutate(
        latestYear = "Non Gov. Supporter"
      )
    
    colors4plot <- c("Non Gov. Supporter" = "#a90099", 
                     "Gov. Supporter"       = "#3273ff")
    
  }
  
  # Saving data points
  data_FFD = data2plot %>% ungroup()
  
  # Plotting chart
  chart <- LAC_radarChart(data          = data2plot,
                          axis_var      = "category",         
                          target_var    = "value2plot",     
                          label_var     = "label", 
                          order_var     = "order_value",
                          colors        = colors4plot)
  
  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = paste0("_",group),
            w      = 189.7883,
            h      = 183.1106)
  
  return(data_FFD)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Accountability Over Time (PAOT)                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


figure_PAOT.fn <- function(nchart = 6, data = master_data.df) {

# Defining data frame for plot
data2plot <- data %>%
  select(country, year, q43_G2) %>%
  mutate(
    q43_G2 = if_else(q43_G2 == 3, 1,
                     if_else(!is.na(q43_G2) & q43_G2 != 99, 0,
                             NA_real_))
  ) %>%
    group_by(year, country) %>%
    summarise(value2plot = mean(q43_G2, na.rm = T)) %>%
    mutate(value2plot = value2plot*100,
           label = paste0(format(round(value2plot, 0),
                                 nsmall = 0),
                          "%"),
           label = if_else(country == mainCountry, label, NA_character_)) %>%
    filter(year >= 2014)

  # Pulling minimum and maximum available year
  minyear <- 2013
  maxyear <- 2023

  # Creating a vector for yearly axis
  x.axis.values <- seq(minyear, maxyear, by = 2)
  sec.ticks     <- seq(minyear, maxyear, by = 1)
  x.axis.labels <- paste0("'", str_sub(x.axis.values, start = -2))
  
  # Saving data points
  data_PAOT = data2plot %>% ungroup()

  # Applying plotting function
  chart <- LAC_lineChart(data           = data2plot,
                         target_var     = "value2plot",
                         grouping_var   = "year",
                         ngroups        = data2plot$country,
                         labels_var     = "label",
                         colors_var     = "country",
                         colors         = mainCOLOR,
                         repel          = F,
                         transparency   = F,
                         custom.axis    = T,
                         x.breaks       = x.axis.values,
                         x.labels       = x.axis.labels,
                         sec.ticks      = sec.ticks
  )

  # Saving panels
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = "A",
            w   = 189.7883,
            h   = 98.4671)
  
  return(data_PAOT)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##    Perceptions of Accountability by Cultural Group(PACG)                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
figure_PACG.fn <- function(nchart = 6, data = master_data.df) {
  
  # Defining variables to include in plot
  vars4plot <- c("q43_G2")
  
  # Defining data frame for plot
  data2plot <- data %>%
    select(CAR_q59_G1, CAR_q59_G2, all_of(unlist(vars4plot, use.names = F))) %>%
    mutate(
      govSupp = 
        case_when(
          !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
          CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
          CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
          CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
          is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_
          ),
      q43_G2 = if_else(q43_G2 == 3, 1,
                       if_else(!is.na(q43_G2) & q43_G2 != 99, 0,
                               NA_real_))
    ) %>%
    filter(!is.na(govSupp)) %>%
    group_by(govSupp) %>%
    summarise(value2plot = mean(q43_G2, na.rm = TRUE)) %>%
    mutate(
      value2plot  = value2plot * 100,
      highlighted = if_else(govSupp == "Gov. Supporter", "Highlighted", "Regular"),
      labels      = to_percentage.fn(value2plot),
      govSupp      = factor(govSupp, levels = c("Gov. Supporter", "Non Gov. Supporter"))
    )
  
  # Defining colors
  
  colors4plot <- c("Gov. Supporter"       = "#3273ff",
                   "Non Gov. Supporter"   = "#a90099")
  names(colors4plot) <- c("Highlighted", "Regular")
  
  # Saving data points
  data_PACG = data2plot %>% ungroup()
  
  # Applying plotting function
  chart <- LAC_barsChart(data           = data2plot,
                         target_var     = "value2plot",
                         grouping_var   = "govSupp",  
                         labels_var     = "labels",
                         colors_var     = "highlighted",
                         colors         = colors4plot,
                         direction      = "horizontal")
  
  # Saving the chart
  saveIT.fn(chart  = chart,
            n      = nchart,
            suffix = "B",
            w   = 189.7883,
            h   = 98.4671)
  
  return(data_PACG)
}
