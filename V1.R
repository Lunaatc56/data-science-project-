library(tidyverse) #charger fonctions tidyverse
library(readxl) #importer fichiers Excel


# ============================================================
# 1. TABLE DE CORRESPONDANCE DES RÉGIONS
# ============================================================

region_map <- data.frame(
  
  region = c(
    
    "31 Østfold",
    "32 Akershus",
    "33 Buskerud",
    "30 Viken (2020-2023)",
    "01 Østfold (-2019)",
    "02 Akershus (-2019)",
    "06 Buskerud (-2019)",
    
    "34 Innlandet",
    "04 Hedmark (-2019)",
    "05 Oppland (-2019)",
    
    "39 Vestfold",
    "40 Telemark",
    "38 Vestfold og Telemark (2020-2023)",
    "07 Vestfold (-2019)",
    "08 Telemark (-2019)",
    
    "42 Agder",
    "09 Aust-Agder (-2019)",
    "10 Vest-Agder (-2019)",
    
    "11 Rogaland",
    
    "03 Oslo - Oslove",
    
    "46 Vestland",
    "12 Hordaland (-2019)",
    "14 Sogn og Fjordane (-2019)",
    
    "15 Møre og Romsdal",
    
    "50 Trøndelag - Trööndelage",
    "16 Sør-Trøndelag (-2017)",
    "17 Nord-Trøndelag (-2017)",
    
    "18 Nordland - Nordlánnda",
    
    "55 Troms - Romsa - Tromssa",
    "56 Finnmark - Finnmárku - Finmarkku",
    "54 Troms og Finnmark - Romsa ja Finnmárku (2020-2023)",
    "19 Troms - Romsa (-2019)",
    "20 Finnmark - Finnmárku (-2019)",
    
    "99g Not mainland Norway",
    "21-22 Svalbard and Jan Mayen",
    "23 The Continental Shelf",
    "25 Abroad",
    "26 Ocean areas"
    
  ),
  
  region_group = c(
    
    rep("Viken", 7),
    
    rep("Innlandet", 3),
    
    rep("Vestfold and Telemark", 5),
    
    rep("Agder", 3),
    
    "Rogaland",
    
    "Oslo",
    
    rep("Vestland", 3),
    
    "Møre og Romsdal",
    
    rep("Trøndelag", 3),
    
    "Nordland",
    
    rep("Troms og Finnmark", 5),
    
    rep("Not mainland Norway", 5)
    
  )
) #associer toutes les anciennes/nouvelles régions aux 12 groupes constants



# ============================================================
# 2. TABLE 11655 : EMPLOI + SALAIRE
# ============================================================

wage_sector <- read_excel(
  "D:/UNIVERSITE/UiO/Data Science/11655_20261016-124515.xlsx",
  sheet = "Lonsstakere",
  skip = 3,
  col_names = c(
    "region",
    "sector",
    "industry",
    "quarter",
    "employment",
    "median_wage"
  ),
  na = c(":", "..", ".", "-")
) #importer fichier et transformer symboles SSB en NA


wage_sector <- wage_sector |>
  
  fill(region, sector, industry) |> #remplir cellules vides du fichier SSB
  
  filter(sector == "Sum all sectors") |> #garder total secteurs institutionnels
  
  filter(substr(quarter, 5, 6) == "K4") |> #garder quatrième trimestre
  
  mutate(
    year = as.numeric(substr(quarter, 1, 4))
  ) |> #extraire année
  
  filter(year %in% 2016:2025) |> #garder période étudiée
  
  filter(industry != "00-99 All industries") |> #retirer total toutes industries
  
  filter(industry != "00 Unspecified") |> #retirer industrie non renseignée
  
  filter(employment > 0) |> #retirer observations sans emploi
  
  left_join(
    region_map,
    by = "region"
  ) |> #ajouter groupe régional harmonisé
  
  filter(!is.na(region_group)) |> #retirer régions non utilisées
  
  group_by(
    year,
    region_group,
    industry
  ) |> #regrouper régions appartenant au même groupe
  
  summarise(
    
    median_wage = sum(
      median_wage * employment,
      na.rm = TRUE
    ) /
      sum(
        employment[!is.na(median_wage)],
        na.rm = TRUE
      ), #calculer moyenne pondérée des médianes disponibles
    
    employment = sum(
      employment,
      na.rm = TRUE
    ) #additionner emploi régional
    
  ) |>
  
  ungroup() #supprimer regroupement



# ============================================================
# 3. TABLE 13888 : EMPLOI IMMIGRÉ
# ============================================================

imm_sector <- read_excel(
  "D:/UNIVERSITE/UiO/Data Science/13888_20261016-124232.xlsx",
  sheet = "Lonsstakere",
  skip = 3,
  col_names = c(
    "region",
    "immigration_category",
    "industry",
    "quarter",
    "immigrant_employment"
  ),
  na = c(":", "..", ".", "-")
) #importer données immigration et transformer symboles SSB en NA


imm_sector <- imm_sector |>
  
  fill(region, immigration_category, industry) |> #remplir cellules vides
  
  filter(
    immigration_category == "Immigrants"
  ) |> #garder seulement immigrés
  
  filter(
    substr(quarter, 5, 6) == "K4"
  ) |> #garder quatrième trimestre
  
  mutate(
    year = as.numeric(substr(quarter, 1, 4))
  ) |> #extraire année
  
  filter(
    year %in% 2016:2025
  ) |> #garder période étudiée
  
  filter(
    industry != "00-99 All industries"
  ) |> #retirer total industries
  
  filter(
    industry != "00 Unspecified"
  ) |> #retirer industrie inconnue
  
  left_join(
    region_map,
    by = "region"
  ) |> #ajouter groupe régional harmonisé
  
  filter(
    !is.na(region_group)
  ) |> #retirer régions non retenues
  
  group_by(
    year,
    region_group,
    industry
  ) |> #regrouper régions harmonisées
  
  summarise(
    immigrant_employment = sum(
      immigrant_employment,
      na.rm = TRUE
    )
  ) |> #additionner emploi immigré
  
  ungroup() #supprimer regroupement



# ============================================================
# 4. CONSTRUIRE PANEL_SECTOR
# ============================================================

panel_sector <- wage_sector |>
  
  left_join(
    imm_sector,
    by = c(
      "year",
      "region_group",
      "industry"
    )
  ) |> #joindre emploi immigré aux salaires et emploi
  
  mutate(
    immigrant_share = immigrant_employment / employment,
    log_wage = log(median_wage)
  ) |> #calculer part immigrée et logarithme salaire
  
  group_by(
    year,
    region_group
  ) |> #regrouper tous secteurs d'une région
  
  mutate(
    sector_employment_share = employment / sum(employment)
  ) |> #calculer poids de chaque secteur dans emploi régional
  
  ungroup() #supprimer regroupement



# ============================================================
# 5. TABLE 12555 : OCCUPATIONS
# ============================================================

occupation_region <- read_excel(
  "D:/UNIVERSITE/UiO/Data Science/12555_20261016-122314.xlsx",
  sheet = "LonnstakerBosted",
  skip = 3,
  col_names = c(
    "contents",
    "region",
    "occupation",
    as.character(2015:2025)
  ),
  na = c(":", "..", ".", "-")
) |> #importer fichier et convertir symboles SSB en NA
  
  fill(
    contents,
    region
  ) |> #remplir cellules vides
  
  mutate(
    across(
      `2015`:`2025`,
      as.numeric
    )
  ) |> #forcer toutes colonnes annuelles en numérique
  
  filter(
    contents == "Employed persons by place of work"
  ) |> #garder travailleurs selon lieu de travail
  
  pivot_longer(
    cols = `2015`:`2025`,
    names_to = "year",
    values_to = "occupation_employment"
  ) |> #passer format large vers format long
  
  mutate(
    year = as.numeric(year)
  ) |> #convertir année en numérique
  
  filter(
    year %in% 2016:2025
  ) |> #garder période étudiée
  
  left_join(
    region_map,
    by = "region"
  ) |> #harmoniser régions
  
  filter(
    !is.na(region_group)
  ) #retirer régions non retenues



# ============================================================
# 6. REGROUPER OCCUPATIONS EN CATÉGORIES
# ============================================================

skill_region <- occupation_region |>
  
  filter(
    substr(occupation, 1, 1) %in% as.character(1:9)
  ) |> #retirer total et occupations non identifiées
  
  mutate(
    
    skill_group = if_else(
      
      substr(occupation, 1, 1) %in% c("1", "2", "3"),
      
      "High skill",
      
      if_else(
        
        substr(occupation, 1, 1) == "9",
        
        "Elementary",
        
        "Intermediate"
        
      )
      
    )
    
  ) |> #1-3 high skill ; 4-8 intermediate ; 9 elementary
  
  group_by(
    year,
    region_group,
    skill_group
  ) |>
  
  summarise(
    occupation_employment = sum(
      occupation_employment,
      na.rm = TRUE
    )
  ) |> #additionner occupations d'une même catégorie
  
  group_by(
    year,
    region_group
  ) |>
  
  mutate(
    skill_share = occupation_employment /
      sum(occupation_employment)
  ) |> #calculer part de chaque catégorie professionnelle
  
  ungroup() #supprimer regroupement



# ============================================================
# 7. CONSTRUIRE PANEL_REGION
# ============================================================

panel_region <- panel_sector |>
  
  group_by(
    year,
    region_group
  ) |>
  
  mutate(
    mean_log_wage = sum(
      sector_employment_share * log_wage,
      na.rm = TRUE
    )
  ) |> #calculer moyenne pondérée des log-salaires
  
  summarise(
    
    total_employment = sum(
      employment,
      na.rm = TRUE
    ), #calculer emploi régional total
    
    immigrant_employment = sum(
      immigrant_employment,
      na.rm = TRUE
    ), #calculer emploi immigré régional total
    
    wage_variance = sum(
      sector_employment_share *
        (log_wage - mean_log_wage)^2,
      na.rm = TRUE
    ) #calculer variance pondérée entre secteurs
    
  ) |>
  
  mutate(
    
    immigrant_share = immigrant_employment /
      total_employment, #calculer part immigrée régionale
    
    wage_dispersion = sqrt(
      wage_variance
    ) #calculer indice de dispersion salariale
    
  ) |>
  
  ungroup() #supprimer regroupement



# ============================================================
# 8. VÉRIFICATIONS
# ============================================================

glimpse(panel_sector) #vérifier types et variables panel sectoriel

glimpse(panel_region) #vérifier types et variables panel régional

glimpse(skill_region) #vérifier données qualifications

View(panel_sector) #afficher panel sectoriel

View(panel_region) #afficher panel régional

View(skill_region) #afficher composition professionnelle