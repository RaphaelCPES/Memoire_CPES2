library(tidyverse)
library(labelled)
library(knitr)
library(kableExtra)
library(ggridges)
library(patchwork)
library(stargazer)
library(modelsummary)
library(forcats)
PARCOURSUP <- read.csv("/Users/raphael/Desktop/CPES2 (local)/Econometrie/S2/R/data/PARCOURSUP - 2025.csv", sep = ";")
APB <- read.csv("/Users/raphael/Desktop/CPES2 (local)/Econometrie/S2/R/data/APB - 2017.csv", sep = ";")

APB <- APB %>%
  filter(Session == 2017) %>%
  select(
    -c(Candidats.admis.dans.son.académie.d.origine,
       Candidats.admis.dans.son.établissement.d.origine..BTS.CPGE...dont.filles,
       Candidats.admis.dans.son.établissement.d.origine..BTS.CPGE.,
       X..Candidats.admis.dans.son.académie.d.origine,
       X..Candidats.admis.dans.son.établissement.d.origine..BTS.CPGE.,
       Admissions.sur.leur.vœu.1, Admissions.sur.leur.vœu.1..dont.filles,
       Demandes.en.vœu.1, Demandes.en.vœu.1..dont.filles,
       Candidats.admis..sans.mention.au.bac, Candidats.admis.avec.mention.A.B.au.bac,
       Candidats.admis.avec.mention.B.au.bac, Candidats.admis.avec.mention.T.B.au.bac,
       Rang.du.dernier.candidat.appelé, Effectif.total.des.candidats..dont.filles,
       Départements, Régions, Candidats.ayant.reçu.une.proposition..dont.filles)
  ) %>%
  rename(
    UAI = Code.UAI.de.l.établissement.d.accueil,
    Etablissement = Libellé.de.l.établissement.d.accueil,
    Departement = Code.département,
    Filieres = Filières.de.formations,
    Filieres_agregees = Filières.très.agrégées,
    Filieres_detaillees = Filières.de.formations.très.détaillées,
    Capacite = Capacité.de.l.établissement.par.formation,
    Candidats = Effectif.total.des.candidats,
    Candidats_admis = Effectif.total.des.candidats.admis,
    Candidats_admis_filles = Effectif.total.des.candidats.admis..dont.filles,
    Candidats_admis_boursiers = Effectif.des.admis.boursiers,
    Taux_boursiers = X..admis.boursiers,
    Taux_SM = X..Candidats.admis.sans.mention.au.bac,
    Taux_AB = X..Candidats.admis.avec.mention.A.B.au.bac,
    Taux_B = X..Candidats.admis.avec.mention.B.au.bac,
    Taux_TB = X..Candidats.admis.avec.mention.T.B.au.bac,
    Academie = Académies,
    Candidats_proposition_reçue = Candidats.ayant.reçu.une.proposition
  )

var_label(APB$UAI) <- "Code UAI de l'établissement"
var_label(APB$Academie) <- "Académie de l'établissement"
var_label(APB$Capacite) <- "Capacité d'accueil de la formation"
var_label(APB$Etablissement) <- "Nom de l'établissement"
var_label(APB$Departement) <- "Département de l'établissement"
var_label(APB$Filieres_agregees) <- "Filière d'étude générale"
var_label(APB$Filieres) <- "Filière d'étude"
var_label(APB$Filieres_detaillees) <- "Filière d'étude détaillée"
var_label(APB$Candidats_admis_boursiers) <- "Nombre de boursiers admis"
var_label(APB$Taux_boursiers) <- "Taux de boursiers parmi les admis"
var_label(APB$Taux_SM) <- "Taux d'admis sans mention"
var_label(APB$Taux_AB) <- "Taux d'admis avec mention assez bien"
var_label(APB$Taux_B) <- "Taux d'admis avec mention bien"
var_label(APB$Taux_TB) <- "Taux d'admis avec mention très bien"

PARCOURSUP <- PARCOURSUP %>%
  mutate(Taux_TB = X..d.admis.néo.bacheliers.avec.mention.Très.Bien.au.bac + 
           X..d.admis.néo.bacheliers.avec.mention.Très.Bien.avec.félicitations.au.bac) %>%
  rename(
    UAI = Code.UAI.de.l.établissement,
    Etablissement = Établissement,
    Departement = Code.départemental.de.l.établissement,
    Academie = Académie.de.l.établissement,
    Filieres_agregees = Filière.de.formation.très.agrégée,
    Filieres = Filière.de.formation.détaillée,
    Filieres_detaillees = Filière.de.formation.très.détaillée,
    Capacite = Capacité.de.l.établissement.par.formation,
    Candidats = Effectif.total.des.candidats.pour.une.formation,
    Candidats_admis = Effectif.total.des.candidats.ayant.accepté.la.proposition.de.l.établissement..admis.,
    Candidats_admis_filles = Dont.effectif.des.candidates.admises,
    Candidats_admis_boursiers = Dont.effectif.des.admis.boursiers.néo.bacheliers,
    Taux_boursiers = X..d.admis.néo.bacheliers.boursiers,
    Taux_SM = X..d.admis.néo.bacheliers.sans.mention.au.bac,
    Taux_AB = X..d.admis.néo.bacheliers.avec.mention.Assez.Bien.au.bac,
    Taux_B = X..d.admis.néo.bacheliers.avec.mention.Bien.au.bac,
    Candidats_proposition_reçue = Effectif.total.des.candidats.ayant.reçu.une.proposition.d.admission.de.la.part.de.l.établissement
  ) %>%
  select(Session, UAI, Etablissement, Departement, Academie, 
         Filieres_agregees, Filieres, Filieres_detaillees, 
         Capacite, Candidats, Candidats_admis, Candidats_admis_filles, 
         Candidats_admis_boursiers, Taux_boursiers, 
         Taux_SM, Taux_AB, Taux_B, Taux_TB, Candidats_proposition_reçue)

var_label(PARCOURSUP$UAI) <- "Code UAI de l'établissement"
var_label(PARCOURSUP$Academie) <- "Académie de l'établissement"
var_label(PARCOURSUP$Capacite) <- "Capacité d'accueil de la formation"
var_label(PARCOURSUP$Etablissement) <- "Nom de l'établissement"
var_label(PARCOURSUP$Departement) <- "Département de l'établissement"
var_label(PARCOURSUP$Filieres_agregees) <- "Filière d'étude générale"
var_label(PARCOURSUP$Filieres) <- "Filière d'étude"
var_label(PARCOURSUP$Filieres_detaillees) <- "Filière d'étude détaillée"
var_label(PARCOURSUP$Candidats_admis_filles) <- "Nombre d'admises"
var_label(PARCOURSUP$Candidats_admis_boursiers) <- "Nombre de boursiers néo-bacheliers admis"
var_label(PARCOURSUP$Taux_boursiers) <- "Taux de boursiers parmi les admis néo-bacheliers"
var_label(PARCOURSUP$Taux_SM) <- "Taux d'admis sans mention"
var_label(PARCOURSUP$Taux_AB) <- "Taux d'admis avec mention assez bien"
var_label(PARCOURSUP$Taux_B) <- "Taux d'admis avec mention bien"
var_label(PARCOURSUP$Taux_TB) <- "Taux d'admis avec mention très bien"


APB <- APB %>% 
  mutate(
    Candidats = as.numeric(Candidats),
    Candidats_proposition_reçue = as.numeric(Candidats_proposition_reçue),
    Taux_acces = Candidats_proposition_reçue / Candidats * 100
  )

APB <- APB %>% 
  mutate(Seuil_selectivite = cut(Taux_acces, 
                                 breaks = c(0, 10, 20, 30, 40, 100),  
                                 include.lowest = T,
                                 labels = c("0% - 10%", "10% - 20%", "20% - 30%", "30% - 40%","40% - 100%")))

APB <- APB %>% 
  mutate(Seuil_TB = cut(Taux_TB, 
                        breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),  
                        include.lowest = T,
                        labels = c("0% - 10%", "10% - 20%", "20% - 30%", "30% - 40%", "40% - 50%", "50% - 60%", "60% - 70%", "70% - 80%", "80% - 90%", "90% - 100%")))


var_label(APB$Candidats) <- "Nombre de candidats"
var_label(APB$Candidats_proposition_reçue) <- "Nombre de candidats ayant reçu une proposition"
var_label(APB$Candidats_admis) <- "Nombre d'admis"
var_label(APB$Candidats_admis_filles) <- "Nombre d'admises"
var_label(APB$Seuil_selectivite) <- "Seuil de taux d'accès"
var_label(APB$Taux_acces) <- "Taux d'accès"
var_label(APB$Seuil_TB) <- "Seuil de proportion de mention très bien"


PARCOURSUP <- PARCOURSUP %>% 
  mutate(
    Candidats = as.numeric(Candidats),
    Candidats_proposition_reçue = as.numeric(Candidats_proposition_reçue),
    Taux_acces = Candidats_proposition_reçue / Candidats * 100
  )

PARCOURSUP <- PARCOURSUP %>% 
  mutate(Seuil_selectivite = cut(Taux_acces, 
                                 breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100),  
                                 include.lowest = T,
                                 labels = c("0% - 20%", "20% - 30%", "30% - 40%", "40% - 50%","50% - 60%", "60% - 70%", "70% - 80%", "80% - 90%", "90% - 100%")))

PARCOURSUP <- PARCOURSUP %>% 
  mutate(Seuil_TB = cut(Taux_TB, 
                        breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),  
                        include.lowest = T,
                        labels = c("0% - 10%", "10% - 20%", "20% - 30%", "30% - 40%", "40% - 50%", "50% - 60%", "60% - 70%", "70% - 80%", "80% - 90%", "90% - 100%")))

var_label(PARCOURSUP$Candidats) <- "Nombre de candidats"
var_label(PARCOURSUP$Candidats_proposition_reçue) <- "Nombre de candidats ayant reçu une proposition"
var_label(PARCOURSUP$Candidats_admis) <- "Nombre d'admis"
var_label(PARCOURSUP$Candidats_admis_filles) <- "Nombre d'admises"
var_label(PARCOURSUP$Seuil_selectivite) <- "Seuil de taux d'accès"
var_label(PARCOURSUP$Taux_acces) <- "Taux d'accès"
var_label(PARCOURSUP$Seuil_TB) <- "Seuil de proportion de mention très bien"


APB <- APB %>%
  mutate(
    Filieres_agregees = str_replace_all(Filieres_agregees, "2_DUT", "BUT"),
    Filieres_agregees = str_replace_all(Filieres_agregees, "8_Ingénieur", "Ecole d'Ingénieur"),
    Filieres_agregees = str_replace_all(Filieres_agregees, "Autre", "Autre formation"),
    Filieres_agregees = str_replace_all(Filieres_agregees, "6_PACES", "PASS"),
    Filieres_agregees = str_replace_all(Filieres_agregees, "7_Management", "Ecole de Commerce")
  )

APB <- APB %>%
  mutate(
    UAI = str_squish(str_to_upper(UAI)),
    Filiere_clean = str_squish(str_remove_all(str_to_upper(Filieres_agregees), "^\\d+_")),
    Cle_Match = paste(UAI, Filiere_clean, sep = "_")
  )

PARCOURSUP <- PARCOURSUP %>%
  mutate(
    UAI = str_squish(str_to_upper(UAI)),
    Filiere_clean = str_squish(str_remove_all(str_to_upper(Filieres_agregees), "^\\d+_")),
    Cle_Match = paste(UAI, Filiere_clean, sep = "_")
  ) %>%
  filter(Cle_Match %in% APB$Cle_Match) %>%
  select(-Filiere_clean, -Cle_Match)

APB <- APB %>%
  select(-Filiere_clean, -Cle_Match)

var_label(APB$Filieres_agregees) <- "Filière d'étude générale"

#II


stats_variables_APB <- APB %>%
  summarise(
    Taux.acces_moyenne = mean(Taux_acces, na.rm = TRUE),
    Taux.acces_mediane = median(Taux_acces, na.rm = TRUE),
    Taux.acces_min = min(Taux_acces, na.rm = TRUE),
    Taux.acces_max = max(Taux_acces, na.rm = TRUE),
    Taux.acces_IQR = IQR(Taux_acces, na.rm = TRUE),
    Taux.acces_observations = n(),
    Boursiers_moyenne = mean(Taux_boursiers, na.rm = TRUE),
    Boursiers_mediane = median(Taux_boursiers, na.rm = TRUE),
    Boursiers_min = min(Taux_boursiers, na.rm = TRUE),
    Boursiers_max = max(Taux_boursiers, na.rm = TRUE),
    Boursiers_IQR = IQR(Taux_boursiers, na.rm = TRUE),
    Boursiers_observations = n(),
    SM_moyenne = mean(Taux_SM, na.rm = TRUE),
    SM_mediane = median(Taux_SM, na.rm = TRUE),
    SM_min = min(Taux_SM, na.rm = TRUE),
    SM_max = max(Taux_SM, na.rm = TRUE),
    SM_IQR = IQR(Taux_SM, na.rm = TRUE),
    SM_observations = n(),
    AB_moyenne = mean(Taux_AB, na.rm = TRUE),
    AB_mediane = median(Taux_AB, na.rm = TRUE),
    AB_min = min(Taux_AB, na.rm = TRUE),
    AB_max = max(Taux_AB, na.rm = TRUE),
    AB_IQR = IQR(Taux_AB, na.rm = TRUE),
    AB_observations = n(),
    B_moyenne = mean(Taux_B, na.rm = TRUE),
    B_mediane = median(Taux_B, na.rm = TRUE),
    B_min = min(Taux_B, na.rm = TRUE),
    B_max = max(Taux_B, na.rm = TRUE),
    B_IQR = IQR(Taux_B, na.rm = TRUE),
    B_observations = n(),
    TB_moyenne = mean(Taux_TB, na.rm = TRUE),
    TB_mediane = median(Taux_TB, na.rm = TRUE),
    TB_min = min(Taux_TB, na.rm = TRUE),
    TB_max = max(Taux_TB, na.rm = TRUE),
    TB_IQR = IQR(Taux_TB, na.rm = TRUE),
    TB_observations = n(),
  ) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = c("Variable", "Statistique"), 
    names_sep = "_",    
    values_to = "Valeur"                
  ) %>%
  pivot_wider(
    names_from = Statistique,
    values_from = Valeur
  )

stats_seuils_APB <- APB %>%
  filter(!is.na(Seuil_selectivite)) %>%
  group_by(Seuil_selectivite) %>%
  summarise(
    moyenne = mean(Taux_acces, na.rm = TRUE),
    mediane = median(Taux_acces, na.rm = TRUE),
    min = min(Taux_acces, na.rm = TRUE),
    max = max(Taux_acces, na.rm = TRUE),
    IQR = IQR(Taux_acces, na.rm = TRUE),
    observations = n()
  ) %>%
  ungroup() %>%
  mutate(Variable = paste("Seuil", Seuil_selectivite)) %>%
  select(Variable, moyenne, mediane, min, max, IQR, observations)

stats_descriptives_APB <- bind_rows(stats_variables_APB, stats_seuils_APB)
kable(stats_descriptives_APB, 
      caption = "Tableau de données descriptives - APB (2017)",
      digits = 2)


stats_variables_PARCOURSUP <- PARCOURSUP %>%
  summarise(
    Taux.acces_moyenne = mean(Taux_acces, na.rm = TRUE),
    Taux.acces_mediane = median(Taux_acces, na.rm = TRUE),
    Taux.acces_min = min(Taux_acces, na.rm = TRUE),
    Taux.acces_max = max(Taux_acces, na.rm = TRUE),
    Taux.acces_IQR = IQR(Taux_acces, na.rm = TRUE),
    Taux.acces_observations = n(),
    Boursiers_moyenne = mean(Taux_boursiers, na.rm = TRUE),
    Boursiers_mediane = median(Taux_boursiers, na.rm = TRUE),
    Boursiers_min = min(Taux_boursiers, na.rm = TRUE),
    Boursiers_max = max(Taux_boursiers, na.rm = TRUE),
    Boursiers_IQR = IQR(Taux_boursiers, na.rm = TRUE),
    Boursiers_observations = n(),
    SM_moyenne = mean(Taux_SM, na.rm = TRUE),
    SM_mediane = median(Taux_SM, na.rm = TRUE),
    SM_min = min(Taux_SM, na.rm = TRUE),
    SM_max = max(Taux_SM, na.rm = TRUE),
    SM_IQR = IQR(Taux_SM, na.rm = TRUE),
    SM_observations = n(),
    AB_moyenne = mean(Taux_AB, na.rm = TRUE),
    AB_mediane = median(Taux_AB, na.rm = TRUE),
    AB_min = min(Taux_AB, na.rm = TRUE),
    AB_max = max(Taux_AB, na.rm = TRUE),
    AB_IQR = IQR(Taux_AB, na.rm = TRUE),
    AB_observations = n(),
    B_moyenne = mean(Taux_B, na.rm = TRUE),
    B_mediane = median(Taux_B, na.rm = TRUE),
    B_min = min(Taux_B, na.rm = TRUE),
    B_max = max(Taux_B, na.rm = TRUE),
    B_IQR = IQR(Taux_B, na.rm = TRUE),
    B_observations = n(),
    TB_moyenne = mean(Taux_TB, na.rm = TRUE),
    TB_mediane = median(Taux_TB, na.rm = TRUE),
    TB_min = min(Taux_TB, na.rm = TRUE),
    TB_max = max(Taux_TB, na.rm = TRUE),
    TB_IQR = IQR(Taux_TB, na.rm = TRUE),
    TB_observations = n(),
  ) %>% 
  pivot_longer(
    cols = everything(), 
    names_to = c("Variable", "Statistique"), 
    names_sep = "_",    
    values_to = "Valeur"                
  ) %>%
  pivot_wider(
    names_from = Statistique,
    values_from = Valeur
  )

stats_seuils_PARCOURSUP <- PARCOURSUP %>%
  filter(!is.na(Seuil_selectivite)) %>%
  group_by(Seuil_selectivite) %>%
  summarise(
    moyenne = mean(Taux_acces, na.rm = TRUE),
    mediane = median(Taux_acces, na.rm = TRUE),
    min = min(Taux_acces, na.rm = TRUE),
    max = max(Taux_acces, na.rm = TRUE),
    IQR = IQR(Taux_acces, na.rm = TRUE),
    observations = n()
  ) %>%
  ungroup() %>%
  mutate(Variable = paste("Seuil", Seuil_selectivite)) %>%
  select(Variable, moyenne, mediane, min, max, IQR, observations)

stats_descriptives_PARCOURSUP <- bind_rows(stats_variables_PARCOURSUP, stats_seuils_PARCOURSUP)

kable(stats_descriptives_PARCOURSUP, 
      caption = "Tableau de données descriptives - Parcoursup (2025)",
      digits = 2)


APB_clean <- APB %>%
  mutate(Source = "APB") %>%
  mutate(Filieres_agregees = str_remove(Filieres_agregees, "^\\d+_")) %>%
  mutate(across(everything(), as.character))

PARCOURSUP_clean <- PARCOURSUP %>%
  mutate(Source = "PARCOURSUP") %>%
  mutate(Filieres_agregees = str_remove(Filieres_agregees, "^\\d+_")) %>%
  mutate(across(everything(), as.character))

comparaison_data <- bind_rows(APB_clean, PARCOURSUP_clean) %>%
  mutate(across(c(Taux_boursiers, Taux_acces, 
                  Taux_SM, Taux_AB, Taux_B, Taux_TB), as.numeric))

filieres_communes <- intersect(
  unique(APB_clean$Filieres_agregees), 
  unique(PARCOURSUP_clean$Filieres_agregees)
)

comparaison_data_filieres <- comparaison_data %>%
  filter(Filieres_agregees %in% filieres_communes) %>%
  filter(!is.na(Filieres_agregees))

couleurs <- c("APB" = "skyblue3", "PARCOURSUP" = "orange3")
theme_bien <- theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(face = "italic", size = 9, hjust = 0, color = "grey20"),
    strip.text = element_text(face = "bold")
  ) +
  theme(panel.grid  = element_blank())


ggplot(comparaison_data, aes(x = Taux_acces, fill = Source)) +
  geom_histogram(color = "white", bins = 30, alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Source) +
  scale_fill_manual(values = couleurs) +
  theme_bien +
  labs(
    title = "Comparaison du taux d'accès",
    x = "Taux d'accès (%)",
    y = "Nombre de formations",
    caption = "Lecture : Sous APB, près de 2 000 formations présentent un taux d'accès compris entre 20 et 25%."
  )


ggplot(comparaison_data, aes(x = Taux_boursiers, fill = Source)) +
  geom_histogram(color = "white", bins = 30, alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~Source) +
  scale_fill_manual(values = couleurs) +
  theme_bien +
  labs(
    title = "Comparaison du taux de boursiers",
    x = "Taux de boursiers (%)",
    y = "Nombre de formations",
    caption = "Lecture : Sous Parcoursup, plus de 1 000 formations affichent un taux de boursiers compris entre 20% et 25%."
  )


p1 <- ggplot(comparaison_data_filieres, 
             aes(x = Taux_boursiers, y = Filieres_agregees, fill = Source)) +
  geom_density_ridges(alpha = 0.6, scale = 0.8, color = "white", show.legend = FALSE) +
  facet_wrap(~Source) +
  scale_fill_manual(values = couleurs) +
  coord_cartesian(xlim = c(0, 100)) +
  theme_ridges() +
  theme_bien +
  theme(legend.position = "none") +
  labs(
    title = "Taux de boursiers par filière", 
    x = "Taux de boursiers (%)",
    y = "Filière"
  )

p2 <- ggplot(comparaison_data_filieres, 
             aes(x = Taux_acces, y = Filieres_agregees, fill = Source)) +
  geom_density_ridges(alpha = 0.6, scale = 0.8, color = "white", show.legend = FALSE) +
  facet_wrap(~Source) +
  scale_fill_manual(values = couleurs) +
  coord_cartesian(xlim = c(0, 100)) +
  theme_ridges() +
  theme_bien + 
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Sélectivité par filière", 
    x = "Taux d'accès (%)"
  )

p1 + p2 +
  plot_annotation(
    caption = "Lecture : Les BTS accueillent globalement davantage de boursiers, tandis que les CPGE et les écoles d’ingénieurs
se concentrent sur des taux de boursiers plus faibles"
  ) &
  theme(
    plot.caption = element_text(
      hjust = 0,
      face = "italic",
      size = 8.5
    )
  )


ggplot(comparaison_data %>% 
         filter(!is.na(Seuil_selectivite)) %>%
         mutate(Seuil_selectivite = forcats::fct_reorder(Seuil_selectivite, readr::parse_number(as.character(Seuil_selectivite)))), 
       aes(x = Seuil_selectivite, y = Taux_boursiers, fill = Source)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  scale_fill_manual(values = couleurs) +
  theme_bien +
  labs(
    title = "Taux de boursiers par seuil de sélectivité",
    x = "Seuil de sélectivité",
    y = "Taux de boursiers (%)",
    caption = "Lecture : Sous Parcoursup, pour les formations dont le taux d’accès est compris entre 0 % et 20 %,
le taux médian de boursiers se situe autour de 25 %."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  coord_cartesian(ylim = c(0, 60))



p1 <- ggplot(comparaison_data %>% 
               filter(!is.na(Seuil_TB)) %>%
               mutate(Seuil_TB = forcats::fct_reorder(Seuil_TB, readr::parse_number(as.character(Seuil_TB)))), 
             aes(x = Seuil_TB, y = Taux_acces)) +
  geom_boxplot(aes(fill = Source), alpha = 0.6, outlier.size = 0.5, color = "grey30") +
  stat_summary(fun = median, geom = "line", aes(group = Source, color = Source), linewidth = 1.2) +
  scale_fill_manual(values = couleurs) +
  scale_color_manual(values = couleurs) +
  theme_bien +
  labs(
    title = "Relation entre la part de mentions TB et le taux d'accès",
    x = "Seuil de mention TB",
    y = "Taux d'accès (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(comparaison_data %>% 
               filter(!is.na(Seuil_TB)) %>%
               mutate(Seuil_TB = forcats::fct_reorder(Seuil_TB, readr::parse_number(as.character(Seuil_TB)))), 
             aes(x = Seuil_TB, y = Taux_boursiers)) +
  geom_boxplot(aes(fill = Source), alpha = 0.6, outlier.size = 0.5, color = "grey30") +
  stat_summary(fun = median, geom = "line", aes(group = Source, color = Source), linewidth = 1.2) +
  scale_fill_manual(values = couleurs) +
  scale_color_manual(values = couleurs) +
  theme_bien +
  labs(
    title = "Relation entre la part de mentions TB et le taux de boursiers",
    x = "Seuil de mention TB",
    y = "Taux de boursiers (%)",
    caption = "Sous Parcoursup et APB, lorsque la part de mentions Très bien est comprise entre 90 % et 100 %,
le taux médian de boursiers est inférieur à 10 %."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1 / p2

#III

APB <- APB %>%
  mutate(Seuil_selectivite = fct_relevel(factor(Seuil_selectivite), "40% - 100%"),
         Seuil_TB = fct_relevel(factor(Seuil_TB), "0% - 10%"))

PARCOURSUP <- PARCOURSUP %>%
  mutate(Seuil_selectivite = fct_relevel(factor(Seuil_selectivite), "90% - 100%"),
         Seuil_TB = fct_relevel(factor(Seuil_TB), "0% - 10%"))

modele_APB_1 <- lm(Taux_boursiers ~ Seuil_selectivite, APB)
modele_PARCOURSUP_1 <- lm(Taux_boursiers ~ Seuil_selectivite, PARCOURSUP)

ordre_selectivite <- c(
  "Seuil_selectivite0% - 10%", 
  "Seuil_selectivite0% - 20%",   
  "Seuil_selectivite10% - 20%", 
  "Seuil_selectivite20% - 30%", 
  "Seuil_selectivite30% - 40%", 
  "Seuil_selectivite40% - 50%", 
  "Seuil_selectivite50% - 60%", 
  "Seuil_selectivite60% - 70%", 
  "Seuil_selectivite70% - 80%", 
  "Seuil_selectivite80% - 90%"
)

stargazer(modele_APB_1, modele_PARCOURSUP_1, 
          type = "text",
          column.labels = c("APB (2017)", "Parcoursup (2025)"),
          dep.var.labels = "Taux de boursiers (%)",
          order = ordre_selectivite,
          title = "Impact de la sélectivité sur la part de boursiers")


modele_APB_2 <- lm(Taux_boursiers ~ Seuil_selectivite + Seuil_TB, APB)
modele_PARCOURSUP_2 <- lm(Taux_boursiers ~ Seuil_selectivite + Seuil_TB, PARCOURSUP)

ordre_TB <- c(
  "Seuil_TB90% - 100%",
  "Seuil_TB80% - 90%",
  "Seuil_TB70% - 80%",
  "Seuil_TB60% - 70%",
  "Seuil_TB50% - 60%",
  "Seuil_TB40% - 50%",
  "Seuil_TB30% - 40%",
  "Seuil_TB20% - 30%",
  "Seuil_TB10% - 20%"
)

stargazer(modele_APB_1, modele_APB_2, modele_PARCOURSUP_1, modele_PARCOURSUP_2,  
          type = "text",
          column.labels = c("APB.1", "APB.2", "PARCOURSUP.1", "PARCOURSUP.2"),
          dep.var.labels = "Taux de boursiers (%)",
          order = c(ordre_selectivite, ordre_TB), 
          title = "Impact de la sélectivité sur la part de boursiers (contrôlé par les mentions TB)")


APB_FA <- APB_clean %>% 
  filter(Filieres_agregees %in% filieres_communes) %>%
  mutate(Filieres_agregees = fct_relevel(factor(Filieres_agregees), "Licence"),
         Seuil_selectivite = fct_relevel(factor(Seuil_selectivite), "40% - 100%"),
         Seuil_TB = fct_relevel(factor(Seuil_TB), "0% - 10%"))

PARCOURSUP_FA <- PARCOURSUP_clean %>% 
  filter(Filieres_agregees %in% filieres_communes) %>%
  mutate(Filieres_agregees = fct_relevel(factor(Filieres_agregees), "Licence"),
         Seuil_selectivite = fct_relevel(factor(Seuil_selectivite), "90% - 100%"),
         Seuil_TB = fct_relevel(factor(Seuil_TB), "0% - 10%"))

modele_APB_3 <- lm(Taux_boursiers ~ Seuil_selectivite + Filieres_agregees, APB_FA)
modele_PARCOURSUP_3 <- lm(Taux_boursiers ~ Seuil_selectivite + Filieres_agregees, PARCOURSUP_FA)

stargazer(modele_APB_1, modele_APB_3, modele_PARCOURSUP_1, modele_PARCOURSUP_3,  
          type = "text",
          column.labels = c("APB.1", "APB.3", "PARCOURSUP.1", "PARCOURSUP.3"),
          dep.var.labels = "Taux de boursiers (%)",
          order = c(ordre_selectivite, "^Filieres_agregees"),
          title = "Impact de la sélectivité sur la part de boursiers contrôlé par la filière agrégée")


modele_APB_4 <- lm(Taux_boursiers ~ Seuil_selectivite + Seuil_TB + Filieres_agregees, APB_FA)
modele_PARCOURSUP_4 <- lm(Taux_boursiers ~ Seuil_selectivite + Seuil_TB + Filieres_agregees, PARCOURSUP_FA)

stargazer(modele_APB_1, modele_APB_4, modele_PARCOURSUP_1, modele_PARCOURSUP_4,  
          type = "text",
          dep.var.labels = "Taux de boursiers (%)",
          column.labels = c("APB.1", "APB.4", "PARCOURSUP.1", "PARCOURSUP.4"),
          order = c(ordre_selectivite, ordre_TB, "^Filieres_agregees"),
          title = "Impact de la sélectivité sur la part de boursiers contrôlé par le taux de mentions très bien et par les filières agrégées")


modele_APB_5 <- lm(Taux_boursiers ~ Seuil_selectivite * Seuil_TB, data = APB)

stargazer(modele_APB_1, modele_APB_2, modele_APB_5, 
          type = "text",
          out = "Heterogeneite_APB_TB.html",
          column.labels = c("Base", "Contrôle TB", "Interaction TB"),
          dep.var.labels = "Taux de boursiers (%)",
          order = c(ordre_selectivite, ordre_TB),
          omit = ":",
          add.lines = list(c("Interactions", "Non", "Non", "Oui")),
          title = "Hétérogénéité APB : Sélectivité et Mentions TB")


modele_PARCOURSUP_5 <- lm(Taux_boursiers ~ Seuil_selectivite * Seuil_TB, data = PARCOURSUP)

stargazer(modele_PARCOURSUP_1, modele_PARCOURSUP_2, modele_PARCOURSUP_5, 
          type = "text",
          out = "Heterogeneite_PS_TB.html",
          column.labels = c("Base", "Contrôle TB", "Interaction TB"),
          dep.var.labels = "Taux de boursiers (%)",
          order = c(ordre_selectivite, ordre_TB),
          omit = ":",
          add.lines = list(c("Interactions", "Non", "Non", "Oui")),
          title = "Hétérogénéité Parcoursup : Sélectivité et Mentions TB")


modele_APB_6 <- lm(Taux_boursiers ~ Seuil_selectivite * Filieres_agregees, data = APB_FA)

stargazer(modele_APB_1, modele_APB_3, modele_APB_6, 
          type = "text",
          out = "Heterogeneite_APB_Filiere_agregees.html",
          column.labels = c("Base", "Contrôle FA", "Interaction FA"),
          dep.var.labels = "Taux de boursiers (%)",
          order = c(ordre_selectivite, "^Filieres_agregees"),
          omit = ":",
          add.lines = list(c("Interactions", "Non", "Non", "Oui")),
          title = "Hétérogénéité APB : Sélectivité et Filières agrégées")


modele_PARCOURSUP_6 <- lm(Taux_boursiers ~ Seuil_selectivite * Filieres_agregees, data = PARCOURSUP_FA)

stargazer(modele_PARCOURSUP_1, modele_PARCOURSUP_3, modele_PARCOURSUP_6, 
          type = "text",
          out = "Heterogeneite_PARCOURSUP_Filiere_agregees.html",
          column.labels = c("Base", "Contrôle FA", "Interaction FA"),
          dep.var.labels = "Taux de boursiers (%)",
          order = c(ordre_selectivite, "^Filieres_agregees"),
          omit = ":",
          add.lines = list(c("Interactions", "Non", "Non", "Oui")),
          title = "Hétérogénéité PARCOURSUP : Sélectivité et Filières agrégées")