diferencia_medias_significativa <- function(datos, var_grupo, vars_resultados, mainCountry) {
  # Crear un data frame vacío para almacenar los resultados
  resultados <- data.frame(variable = character(),
                           diferencia = numeric(),
                           p_valor = numeric(),
                           significance = character(),
                           stringsAsFactors = FALSE)
  
  # Iterar sobre las variables de resultados y calcular la diferencia de medias y p-valor para cada una
  for (var_resultado in vars_resultados) {
    
    # Filtrar los datos para el grupo 1 y el grupo 2
    grupo1 <- datos %>% filter({{ var_grupo }} == "Gov. Supporter")
    grupo2 <- datos %>% filter({{ var_grupo }} == "Non Gov. Supporter")
    
    # Calcular la media para cada grupo, ignorando los valores perdidos
    media_grupo1 <- mean(grupo1[[var_resultado]], na.rm = TRUE)
    media_grupo2 <- mean(grupo2[[var_resultado]], na.rm = TRUE)
    
    # Verificar que haya suficientes observaciones para realizar la prueba
    if (nrow(grupo1) < 3 || nrow(grupo2) < 3) {
      warning(paste0("No hay suficientes observaciones para la variable '", var_resultados, "'."))
      p_valor <- NA
      significativa <- "NA"
    } else {
      # Calcular la diferencia de medias y el p-valor usando una prueba t de Student
      resultado_prueba <- t.test(grupo1[[var_resultado]], grupo2[[var_resultado]], na.rm = TRUE)
      p_valor <- resultado_prueba$p.value
      significance <- ifelse(p_valor < 0.01, "YES", "NO")
    }
    
    # Agregar los resultados al data frame de resultados
    resultados <- rbind(resultados, data.frame(country = mainCountry,
                                               media_supporter = media_grupo1,
                                               media_non_supporter = media_grupo2,
                                               variable = var_resultado,
                                               diff = media_grupo1 - media_grupo2,
                                               p_valor = p_valor,
                                               significance = significance,
                                               stringsAsFactors = FALSE))
  }
  # Retornar el data frame de resultados
  return(resultados)
}
  
  authoritarianism <- c("q50_neg", "q51_neg", "q52_neg",
                        "q50_pos", "q51_pos", "q52_pos")
  bribery <- c("q4a", "q4b", "q4c", "q4d", "q4e")
  constraints    <- c("q45a_G1", "q45b_G1", "q45c_G1")
  ffreedoms      <- c("q46c_G2", "q46f_G2", "q46g_G2", "q46c_G1", "q46e_G2",
                      "q46d_G2", "q46f_G1", "q46a_G2",
                      "q46d_G1", "q46e_G1", "q46h_G2")
  accountability <- c("q43_G2")
  corruption     <- c("q2a", "q2d", "q2b", "q2c", "q2e", "q2f", "q2g")
  trust          <- c("q1a", "q1d", "q1b", "q1c", "q1e", "q1f", "q1g")
  security       <- c("q9")
  cjustice       <- c("q49a", "q49b_G2", "q49e_G2", "q49c_G2", "q49e_G1", "q49c_G1", "q49b_G1")
  effectiveness  <- c("q48f_G2", "q48h_G1", "q48g_G2")
  police         <- c("q48c_G2",
                      "q48a_G2", "q48b_G2", "q48b_G1",
                      "q48a_G1", "q48c_G1", "q48d_G2",
                      "q48d_G1")
  
  victimization <- c("victim", "prop_crimes", "life_crimes", "corr_crimes")
  VIP_vars <- c(ffreedoms, accountability,corruption, trust, security, cjustice,
                effectiveness, police, constraints, bribery)
  
  # Subsetting for variables of interest
  data_subset.df <- master_data.df %>%
    filter(country %in% mainCountry) %>%
    select(country, country_code, year,
           all_of(VIP_vars), 
           q50, q51, q52,
           age, fin, Urban, gend, edu, ethni, relig, paff3,
           CAR_q59_G1, CAR_q59_G2) %>%
    mutate(
      q52 = case_when(
        q52 == 1 ~ 4,
        q52 == 2 ~ 3,
        q52 == 3 ~ 2,
        q52 == 4 ~ 1,
        q52 == 5 ~ 5,
        q52 == 99 ~ 99
      ),
      across(c(q50, q51, q52),
             ~if_else(.x < 3, 1, 0),
             .names = "{.col}_neg"),
      across(c(q50, q51, q52),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(.x == 1 | .x == 2, 0, 
                              NA_real_)),
             .names = "{.col}_pos")) %>%
    mutate(
      across(all_of(constraints),
             ~ if_else(.x == 1 | .x == 2, 1,
                       if_else(.x == 3 | .x == 4, 0, 
                               NA_real_))),
      across(all_of(ffreedoms),
             ~ case_when(
               .x == 1 | .x == 2 ~ 1,
               .x == 3 | .x == 4 ~ 0
             )),
      across(all_of(accountability),
             ~if_else(.x == 3, 1, 
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_))),
      across(all_of(corruption),
             ~if_else(.x == 3 | .x == 4, 1,
                      if_else(!is.na(.x)  & .x != 99, 0, 
                              NA_real_))),
      across(all_of(bribery),
             ~if_else(.x == 99, NA_real_, as.double(.x))),
      across(all_of(trust),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_))),
      across(all_of(security),
             ~case_when(
               .x == 1 | .x == 2    ~ 1,
               .x == 3 | .x == 4    ~ 0
             )),
      across(all_of(cjustice),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, NA_real_))),
      across(all_of(effectiveness),
             ~if_else(.x == 1 | .x == 2, 1,
                      if_else(!is.na(.x) & .x != 99, 0, 
                              NA_real_))),
      across(all_of(police),
             ~case_when(
               .x == 1  ~ 1,
               .x == 2  ~ 1,
               .x == 3  ~ 0,
               .x == 4  ~ 0))
    ) %>%
    mutate(govSupp = case_when(
      !is.na(CAR_q59_G1) & !is.na(CAR_q59_G2) ~ NA_character_,
      CAR_q59_G1 == 1   | CAR_q59_G2 == 1     ~ "Gov. Supporter",
      CAR_q59_G1 == 2   | CAR_q59_G2 == 2     ~ "Non Gov. Supporter",
      CAR_q59_G1 == 99  | CAR_q59_G2 == 99    ~ NA_character_,
      is.na(CAR_q59_G1) & is.na(CAR_q59_G2)   ~ NA_character_)
    )
  
  data2table <- data_subset.df %>%
    filter(govSupp %in% c("Gov. Supporter", "Non Gov. Supporter")) 

  VIP_vars <- c(VIP_vars)
  
  diffmeans.df <- diferencia_medias_significativa(datos = data2table, 
                                                  var_grupo = govSupp, 
                                                  vars_resultados = VIP_vars,
                                                  mainCountry = mainCountry)  
  
  diffmeans.df <- diffmeans.df %>%
    mutate(label = case_when(
      variable == "q46c_G2" ~ "Fundamental Freedoms - People can express opinion against the government",
      variable == "q46f_G2" ~ "Fundamental Freedoms - Civil society organizations can express opinions against the government",
      variable == "q46g_G2" ~ "Fundamental Freedoms - Political parties can express opinions against the government",
      variable == "q46c_G1" ~ "Fundamental Freedoms - The media can express opinions against the government without fear of retaliation",
      variable == "q46e_G2" ~ "Fundamental Freedoms - The media can expose cases of corruption",
      variable == "q46d_G2" ~ "Fundamental Freedoms - People can attend community meetings",
      variable == "q46f_G1" ~ "Fundamental Freedoms - People can join any political organizations",
      variable == "q46a_G2" ~ "Fundamental Freedoms - People can organize around an issue or petition",
      variable == "q46d_G1" ~ "Fundamental Freedoms - Local government officials are elected through a clean process",
      variable == "q46e_G1" ~ "Fundamental Freedoms - People can vote freely without feeling harassed or pressured",
      variable == "q46h_G2" ~ "Fundamental Freedoms - Religious minorities can observe their holy days",
      variable == "q2a" ~ "Corruption - Members of the national assembly",
      variable == "q2d" ~ "Corruption - Police officers",
      variable == "q2b" ~ "Corruption - Local government officers",
      variable == "q2c" ~ "Corruption - National government officers",
      variable == "q2e" ~ "Corruption - Prosecutors",
      variable == "q2f" ~ "Corruption - Judges & magistrates",
      variable == "q2g" ~ "Corruption - Public Defense Attorneys",
      variable == "q1a" ~ "Trust - People living in their country",
      variable == "q1d" ~ "Trust - Police officers",
      variable == "q1b" ~ "Trust - Local government officers",
      variable == "q1c" ~ "Trust - National government officers",
      variable == "q1e" ~ "Trust - Prosecutors",
      variable == "q1f" ~ "Trust - Judges & magistrates",
      variable == "q1g" ~ "Trust - Public Defense Attorneys",
      variable == "q43_G2" ~ "Accountability - Government officials would be held accountable for breaking the law",
      variable == "q9" ~ "Security - Perception of security: feeling safe walking in their neighborhood at night",
      variable == "q49a" ~ "CJ - The criminal justice system is effective in bringing people who commit crimes to justice",
      variable == "q49b_G2" ~ "CJ - The criminal justice system allows all victims of crime to seek justice regardless of who they are",
      variable == "q49e_G2" ~ "CJ - The criminal justice system treats those accused of crime as 'innocent until proven guilty'",
      variable == "q49c_G2" ~ "CJ - The criminal justice system allows all those accused of crimes to get a fair trial regardless of who they are",
      variable == "q49e_G1" ~ "CJ - The criminal justice system gives punishments which fit the crime",
      variable == "q49c_G1" ~ "CJ - The criminal justice system makes sure everyone has access to the justice system if they need it",
      variable == "q49b_G1" ~ "CJ - The criminal justice system deals with cases promptly and efficiently",
      variable == "q48f_G2" ~ "Institutional Perfomance - Prosecutors prosecute crimes committed in an independent manner",
      variable == "q48h_G1" ~ "Institutional Perfomance - The public defenders do everything they can to defend poor people that are accused of committing a crime",
      variable == "q48g_G2" ~ "Institutional Perfomance - The judges decide cases in an independent manner and are not subject to any sort of pressure",
      variable == "q48c_G2" ~ "Police - The police be available to help you when you need it",
      variable == "q48a_G2" ~ "Police - The police resolve security problems in your community",
      variable == "q48b_G1" ~ "Police - Police investigators perform serious and law-abiding investigations to find the perpetrators of a crime",
      variable == "q48b_G2" ~ "Police - The police help you and your family to feel safe within and outside of your house",
      variable == "q48a_G1" ~ "Police - The police act according to the law",
      variable == "q48c_G1" ~ "Police - The basic rights of suspects are respected by the police",
      variable == "q48d_G2" ~ "Police - The police treat all people with kindness and respect",
      variable == "q48d_G1" ~ "Police - If members of the police violate the law, they are punished for these violations",
      variable == "q50_neg" ~ "Authoritarianism - Government efficiency is more important than citizen influence",
      variable == "q50_pos" ~ "Authoritarianism - It is important that citizens have a say in government matters, even at the expense of efficiency",
      variable == "q51_neg" ~ "Authoritarianism - The president should not be bound by the laws or courts",
      variable == "q51_pos" ~ "Authoritarianism - The president must always obey the law and the courts",
      variable == "q52_neg" ~ "Authoritarianism - It is not necessary to obey the laws of a government that you did not vote for",
      variable == "q52_pos" ~ "Authoritarianism - It is important to obey the government in power, no matter who you voted for",
      variable == "q45a_G1" ~ "Contrainst - Congress could hypothetically stop a Head of State'sillegal actions",
      variable == "q45b_G1" ~ "Contrainst - Courts could hypothetically stop a Head of State'sillegal actions",
      variable == "q45c_G1" ~ "Contrainst - Citizens could hypothetically stop a Head of State'sillegal actions",
      variable == "q4a" ~ "Bribery - Request a government permit",
      variable == "q4b" ~ "Bribery - Request public benefits",
      variable == "q4c" ~ "Bribery - Obtain a bith certificate",
      variable == "q4d" ~ "Bribery - Secure a place at a public school",
      variable == "q4e" ~ "Bribery - Use a public health service",
      variable == "victim" ~ "Security - Victims in the last 12 months",
      variable == "prop_crimes" ~ "Security - Property crimes",
      variable == "life_crimes" ~ "Security - Crime against life",
      variable == "corr_crimes" ~ "Security - Corruption, financial, and commercial crimes",
      variable == "q8d" ~ "Security - Victims that reported crimes",
      variable == "q8f" ~ "Security - Cases ended in prosecution",
      variable == "q17_1" ~ "Discrimination - Ancestry or national \norigin",
      variable == "q17_2" ~ "Discrimination - Gender",
      variable == "q17_3" ~ "Discrimination - Race",
      variable == "q17_4" ~ "Discrimination - Age",
      variable == "q17_5" ~ "Discrimination - Religion",
      variable == "q17_6" ~ "Discrimination - Height",
      variable == "q17_7" ~ "Discrimination - Weight",
      variable == "q17_8" ~ "Discrimination - Physical\nappearence",
      variable == "q17_9" ~ "Discrimination - Physical or mental \ndisability",
      variable == "q17_10" ~ "Discrimination - Sexual orientation",
      variable == "q17_11" ~ "Discrimination - Education or\nincome level",
      variable == "q17_12" ~ "Discrimination - Nationality or inmigration \nstatus",
      variable == "q17_13" ~ "Discrimination - Shade of \nskin color",
      variable == "q17_14" ~ "Discrimination - Tribe",
      variable == "q17_15" ~ "Discrimination - Clothing or\nhairstyle",
      variable == "q16a" ~ "Discrimination - You are treated with less courtesy or respect than other people",
      variable == "q16b" ~ "Discrimination - You receive poorer service than other people at restaurants or stores",
      variable == "q16c" ~ "Discrimination - People act as if they think you are not smart",
      variable == "q16d" ~ "Discrimination - People act as if they are afraid of you",
      variable == "q16e" ~ "Discrimination - You are threatened or harassed"
    ))

  write.xlsx(diffmeans.df, file = "Outputs/govSupp_differences.xlsx")
  
  relig <- data2table %>%
    mutate(religion = if_else(relig %in% ('C57 - Orthodox Christian'), 1,
                              if_else(relig %in% ('G6 - Sunni Muslim'), 0, NA_real_)))
  