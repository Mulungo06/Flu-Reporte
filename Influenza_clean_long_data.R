library(dplyr)
library(tidyr)
library(lubridate)

clean_flu_long_data <- function(df) {
  # Extract 'a' (years) and 'm' (months) from 'idade'
  df$idade <- as.character(df$idade)
  years <- as.numeric(sub("a.*", "", df$idade))
  months <- as.numeric(sub(".*m", "", df$idade))
  df$idade <- ifelse(!is.na(months), round(months / 12, 0), years)
  
  df <- df %>%
    mutate(across(where(is.character), trimws))
  
  df <- df %>%
    mutate(
      # Standardize 'resultados_sars'
      resultado_sars = case_when(
        is.na(resultado_sars) ~ NA_character_,
        grepl("^[0-9]+([,.][0-9]+)?$", resultado_sars) ~ "Positivo",
        resultado_sars %in% c('Negativo', 'Negtaivo', 'Negativo ', 'N/A', '') ~ 'Negativo',
        resultado_sars %in% c('Positivo', 'Positiva', 'Positivo ') ~ 'Positivo',
        TRUE ~ resultado_sars
      ),
      # Standardize 'h3'
      h3 = case_when(
        is.na(h3) ~ NA_character_,
        grepl("^[0-9]+([,.][0-9]+)?$", h3) ~ "Positivo",
        h3 %in% c('Negativo', 'Negtaivo', 'Negativo ', 'N/A', '') ~ 'Negativo',
        h3 %in% c('Positivo', 'Positiva', 'Positivo ') ~ 'Positivo',
        TRUE ~ h3
      ),
      # Standardize 'inf_a'
      inf_a = case_when(
        is.na(inf_a) ~ NA_character_,
        grepl("^[0-9]+([,.][0-9]+)?$", inf_a) ~ "Positivo",
        inf_a %in% c('Negativo', 'Negtaivo', 'Negativo ', 'N/A', '') ~ 'Negativo',
        inf_a %in% c('Positivo', 'Positiva', 'Positivo ') ~ 'Positivo',
        TRUE ~ inf_a
      ),
      # Standardize 'inf_b'
      inf_b = case_when(
        is.na(inf_b) ~ NA_character_,
        grepl("^[0-9]+([,.][0-9]+)?$", inf_b) ~ "Positivo",
        inf_b %in% c('Negativo', 'Negtaivo', 'Negativo ', 'N/A', '') ~ 'Negativo',
        inf_b %in% c('Positivo', 'Positiva', 'Positivo ') ~ 'Positivo',
        TRUE ~ inf_b
      ),
      # Standardize 'vic'
      vic = case_when(
        is.na(vic) ~ NA_character_,
        grepl("^[0-9]+([,.][0-9]+)?$", vic) ~ "Positivo",
        vic %in% c('Negativo', 'Negtaivo', 'Negativo ', 'N/A', '') ~ 'Negativo',
        vic %in% c('Positivo', 'Positiva', 'Positivo ') ~ 'Positivo',
        TRUE ~ vic
      ),
      # Standardize 'yam'
      yam = case_when(
        is.na(yam) ~ NA_character_,
        grepl("^[0-9]+([,.][0-9]+)?$", yam) ~ "Positivo",
        yam %in% c('Negativo', 'Negtaivo', 'Negativo ', 'N/A', '') ~ 'Negativo',
        yam %in% c('Positivo', 'Positiva', 'Positivo ') ~ 'Positivo',
        TRUE ~ yam
      ),
      # Standardize 'h1pdm'
      h1pdm = case_when(
        is.na(h1pdm) ~ NA_character_,
        grepl("^[0-9]+([,.][0-9]+)?$", h1pdm) ~ "Positivo",
        h1pdm %in% c('Negativo', 'Negtaivo', 'Negativo ', 'N/A', '') ~ 'Negativo',
        h1pdm %in% c('Positivo', 'Positiva', 'Positivo ') ~ 'Positivo',
        TRUE ~ h1pdm
      ),
      # Standardize 'resultado_rsv'
      resultado_rsv = case_when(
        is.na(resultado_rsv) ~ NA_character_,
        grepl("^[0-12]+([,.][0-12]+)?$", resultado_rsv) ~ "Positivo",
        resultado_rsv %in% c('Negativo', 'Negtaivo', 'Negativo ', 'N/A', '') ~ 'Negativo',
        resultado_rsv %in% c('Positivo', 'Positiva', 'Positivo ') ~ 'Positivo',
        TRUE ~ resultado_rsv
      )
    )
  
  
  # Verify missing values
  print(sapply(df, function(x) sum(is.na(x))))
  
  # Remove missing values from 'data_da_colheita' and 'idade' columns
  df_clean <- df %>% drop_na(data_da_colheita, idade)
  
  # Remove outliers in 'idade'
  boxplot_stats <- boxplot.stats(df_clean$idade)
  outliers <- boxplot_stats$out
  df_clean <- df_clean[!df_clean$idade %in% outliers, ]
  
  # Convert 'data_da_colheita' to Date format and extract week number
  df_clean$Week <- lubridate::week(df_clean$data_da_colheita)
  
  
  # Convert the data into long format
  df_long <- df_clean %>%
    pivot_longer(cols = c('inf_a', 'inf_b', 'resultado_sars', 'resultado_rsv'), 
                 names_to = 'virus_type', 
                 values_to = 'resultado_virus_long') %>% 
    pivot_longer(cols = c('h3', 'h1pdm', 'vic', 'yam'), 
                 names_to = 'influenza_subtype', 
                 values_to = 'resultado_flu_subtype')
  
  # Recoding the virus_type and influenza_subtype columns
  df_long <- df_long %>%
    mutate(
      virus_type = case_when(
        virus_type == 'inf_a' ~ 'Influenza A',
        virus_type == 'inf_b' ~ 'Influenza B',
        virus_type == 'resultado_sars' ~ 'SARS-Cov-2',
        virus_type == 'resultado_rsv' ~ 'RSV',
        TRUE ~ virus_type  # Retain any other values as they are
      ),
      influenza_subtype = case_when(
        influenza_subtype == 'h3' ~ 'H3N2',
        influenza_subtype == 'h1pdm' ~ 'H1N1 Pand2009',
        influenza_subtype == 'vic' ~ 'Victoria',
        influenza_subtype == 'yam' ~ 'Yamagata',
        TRUE ~ influenza_subtype  # Retain any other values as they are
      )
    )
  
  # Filter only positive results
  df_long_flu_p <- df_long %>%
    filter(resultado_virus_long == 'Positivo')
  
  return(df_long_flu_p)
}
