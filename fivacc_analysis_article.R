###################
# FIVACC analysis 
###################

# Install packages
install.packages("readxl")
install.packages("writexl")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("questionr")
install.packages("dplyr")
install.packages("dunn.test")
install.packages("unikn")


# load
library(readxl)
library(writexl)
library("tidyverse")
library(lubridate)
library(questionr)
library("dplyr")
library("dunn.test")
library("unikn")

# Set a working directory
setwd("/Users/laurene/Desktop/ITM/Vaccine group project/FIVACC/base_de_donnees")

# Import DS
ds <- read_excel("fivacc_mon_export_29092021.xlsx", guess_max = 3000)


# Data preparation
###########################

# Cleaning date
ds$start_date <- as.Date(ds$start)

# Exclude data from Pilot study
ds_1 <- ds %>%
  filter(start_date >= "2021-06-09")

# Exclude the record collected in Matonge
# I got the ID on the map of Kobocollect
ds_2 <- ds_1 %>%
  filter(`_id`!= 100895324)
#table(ds_1$zones_sante, exclude= NULL)

# Keep data when consent is "yes"
table(ds_2$A_3_Consentement)
ds_3 <- ds_2 %>%
  filter(A_3_Consentement=="oui")

# Merge the 2 columns of age
ds_3[is.na(ds_3$`B_1_Quel_est_votre_ge`),]$`B_1_Quel_est_votre_ge` <- ds_3[is.na(ds_3$`B_1_Quel_est_votre_ge`),]$`B_1_Quel_est_votre_ge_001`

# Create numeric age variable
ds_3$age <- as.numeric(ds_3$`B_1_Quel_est_votre_ge`)

# Merge column of autre aire de sant
#ds$aire_sante <- str_replace(ds$aire_sante, "^[Mm]ut.*", "mutombo")


# Coding scores
# Belief score: score based on 4 questions
## Recoding of the first question
ds_3[ds_3$`C_9_Pensez_vous_qu_portant_de_vacciner_` == "non", "vacc_important"] <- 1
ds_3[ds_3$`C_9_Pensez_vous_qu_portant_de_vacciner_` == "ne_sait_pas", "vacc_important"] <- 2
ds_3[ds_3$`C_9_Pensez_vous_qu_portant_de_vacciner_` == "peut_tre", "vacc_important"] <- 2
ds_3[ds_3$`C_9_Pensez_vous_qu_portant_de_vacciner_` == "oui", "vacc_important"] <- 3
ds_3[ds_3$`C_9_Pensez_vous_qu_portant_de_vacciner_` == "refus_de_r_pondre ", "vacc_important"] <- NA

## Recoding of the second question
ds_3[ds_3$`C_10_Pensez_vous_q_s_vaccins_sont_s_rs_` == "non", "vacc_sur"] <- 1
ds_3[ds_3$`C_10_Pensez_vous_q_s_vaccins_sont_s_rs_` == "ne_sait_pas", "vacc_sur"] <- 2
ds_3[ds_3$`C_10_Pensez_vous_q_s_vaccins_sont_s_rs_` == "peut_tre", "vacc_sur"] <- 2
ds_3[ds_3$`C_10_Pensez_vous_q_s_vaccins_sont_s_rs_` == "oui", "vacc_sur"] <- 3
ds_3[ds_3$`C_10_Pensez_vous_q_s_vaccins_sont_s_rs_` == "refus_de_r_pondre ", "vacc_sur"] <- NA

## Recoding of the third question
ds_3[ds_3$`C_11_Pensez_vous_qu_cins_sont_efficaces_` == "non", "vacc_efficace"] <- 1
ds_3[ds_3$`C_11_Pensez_vous_qu_cins_sont_efficaces_` == "ne_sait_pas", "vacc_efficace"] <- 2
ds_3[ds_3$`C_11_Pensez_vous_qu_cins_sont_efficaces_` == "peut_tre", "vacc_efficace"] <- 2
ds_3[ds_3$`C_11_Pensez_vous_qu_cins_sont_efficaces_` == "oui", "vacc_efficace"] <- 3
ds_3[ds_3$`C_11_Pensez_vous_qu_cins_sont_efficaces_` == "refus_de_r_pondre ", "vacc_efficace"] <- NA

## Recoding of the fourth question
ds_3[ds_3$`C_12_Est_ce_que_c_t_pratiquement_plus_` == "non", "disease_disappeared"] <- 1
ds_3[ds_3$`C_12_Est_ce_que_c_t_pratiquement_plus_` == "ne_sait_pas", "disease_disappeared"] <- 2
ds_3[ds_3$`C_12_Est_ce_que_c_t_pratiquement_plus_` == "peut_tre", "disease_disappeared"] <- 2
ds_3[ds_3$`C_12_Est_ce_que_c_t_pratiquement_plus_` == "oui", "disease_disappeared"] <- 3
ds_3[ds_3$`C_12_Est_ce_que_c_t_pratiquement_plus_` == "refus_de_r_pondre ", "disease_disappeared"] <- NA

## Belief score = mean score on the 4 questions
ds_3$score_gen_belief <- (ds_3$vacc_important + ds_3$vacc_sur + ds_3$vacc_efficace + ds_3$disease_disappeared)/4


# Knowledge score: score based on 4 questions
## Recoding of the first question
ds_3[ds_3$`C_13_Pour_les_malad_es_reste_n_cessaire_` == "non", "vacc_still_necessaire"] <- 1
ds_3[ds_3$`C_13_Pour_les_malad_es_reste_n_cessaire_` == "ne_sait_pas", "vacc_still_necessaire"] <- 2
# "ne_sait_pas" si oui (nécessaire) ou "ne_sait_pas" si non (pas nécessaire) donc neutral answer like "peut-être"
ds_3[ds_3$`C_13_Pour_les_malad_es_reste_n_cessaire_` == "peut_tre", "vacc_still_necessaire"] <- 2
ds_3[ds_3$`C_13_Pour_les_malad_es_reste_n_cessaire_` == "oui", "vacc_still_necessaire"] <- 3
ds_3[ds_3$`C_13_Pour_les_malad_es_reste_n_cessaire_` == "refus_de_r_pondre ", "vacc_still_necessaire"] <- NA

## Recoding of the second question
ds_3[ds_3$`C_14_Pensez_vous_q_ladies_infectieuses_` == "non", "vacc_gd_nombre"] <- 1
ds_3[ds_3$`C_14_Pensez_vous_q_ladies_infectieuses_` == "ne_sait_pas", "vacc_gd_nombre"] <- 2
ds_3[ds_3$`C_14_Pensez_vous_q_ladies_infectieuses_` == "peut_tre", "vacc_gd_nombre"] <- 2
ds_3[ds_3$`C_14_Pensez_vous_q_ladies_infectieuses_` == "oui", "vacc_gd_nombre"] <- 3
ds_3[ds_3$`C_14_Pensez_vous_q_ladies_infectieuses_` == "refus_de_r_pondre ", "vacc_gd_nombre"] <- NA

## Recoding of the third question
ds_3[ds_3$`C_15_La_plupart_des_araissent_rapidement` == "non", "effet_secondaire"] <- 1
ds_3[ds_3$`C_15_La_plupart_des_araissent_rapidement` == "ne_sait_pas", "effet_secondaire"] <- 2
ds_3[ds_3$`C_15_La_plupart_des_araissent_rapidement` == "peut_tre", "effet_secondaire"] <- 2
ds_3[ds_3$`C_15_La_plupart_des_araissent_rapidement` == "oui", "effet_secondaire"] <- 3
ds_3[ds_3$`C_15_La_plupart_des_araissent_rapidement` == "refus_de_r_pondre ", "effet_secondaire"] <- NA

## Recoding of the fourth question
ds_3[ds_3$`C_16_L_allaitement_que_la_vaccination_` == "non", "allaitement"] <- 3
ds_3[ds_3$`C_16_L_allaitement_que_la_vaccination_` == "ne_sait_pas", "allaitement"] <- 2
ds_3[ds_3$`C_16_L_allaitement_que_la_vaccination_` == "peut_tre", "allaitement"] <- 2
ds_3[ds_3$`C_16_L_allaitement_que_la_vaccination_` == "oui", "allaitement"] <- 1
ds_3[ds_3$`C_16_L_allaitement_que_la_vaccination_` == "refus_de_r_pondre ", "allaitement"] <- NA

## Knowledge score = mean score on the 4 questions
ds_3$score_knowledge <- (ds_3$vacc_still_necessaire + ds_3$vacc_gd_nombre + ds_3$effet_secondaire + ds_3$allaitement)/4


# Attitude score: score based on 3 questions
## Recoding of the first question
ds_3[ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_` == "option_2" &!is.na(ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_`), "vacc_necessaire"] <- 1
ds_3[ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_` == "ne_sait_pas" &!is.na(ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_`), "vacc_necessaire"] <- 2
# "ne_sait_pas" si oui (nécessaire) ou "ne_sait_pas" si non (pas nécessaire) donc neutral answer like "peut-être"
ds_3[ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_` == "peut_tre" &!is.na(ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_`), "vacc_necessaire"] <- 2
ds_3[ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_` == "option_1" &!is.na(ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_`), "vacc_necessaire"] <- 3
ds_3[ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_` == "refus_de_r_pondre" &!is.na(ds_3$`C_22_Pensez_vous_qu_tion_est_n_cessaire_`), "vacc_necessaire"] <- NA
# option 1 dans Kobo est "oui" et option 2 est "non"

## Recoding of the second question
ds_3[ds_3$`C_23_Pensez_vous_qu_eptable_socialement_` == "non" &!is.na(ds_3$`C_23_Pensez_vous_qu_eptable_socialement_`), "vacc_acceptable"] <- 1
ds_3[ds_3$`C_23_Pensez_vous_qu_eptable_socialement_` == "ne_sait_pas" &!is.na(ds_3$`C_23_Pensez_vous_qu_eptable_socialement_`), "vacc_acceptable"] <- 2
ds_3[ds_3$`C_23_Pensez_vous_qu_eptable_socialement_` == "peut_tre" &!is.na(ds_3$`C_23_Pensez_vous_qu_eptable_socialement_`), "vacc_acceptable"] <- 2
ds_3[ds_3$`C_23_Pensez_vous_qu_eptable_socialement_` == "oui" &!is.na(ds_3$`C_23_Pensez_vous_qu_eptable_socialement_`), "vacc_acceptable"] <- 3
ds_3[ds_3$`C_23_Pensez_vous_qu_eptable_socialement_` == "refus_de_r_pondre" &!is.na(ds_3$`C_23_Pensez_vous_qu_eptable_socialement_`), "vacc_acceptable"] <- NA

## Recoding of the third question
ds_3[ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_` == "non" &!is.na(ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_`), "vacc_souhaitable"] <- 1
ds_3[ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_` == "ne_sait_pas" &!is.na(ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_`), "vacc_souhaitable"] <- 2
ds_3[ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_` == "peut_tre" &!is.na(ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_`), "vacc_souhaitable"] <- 2
ds_3[ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_` == "oui" &!is.na(ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_`), "vacc_souhaitable"] <- 3
ds_3[ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_` == "refus_de_r_pondre" &!is.na(ds_3$`C_24_Pensez_vous_qu_ion_est_souhaitable_`), "vacc_souhaitable"] <- NA

## Attitude score = mean score on the 3 questions
ds_3$score_attitude <- (ds_3$vacc_necessaire + ds_3$vacc_acceptable + ds_3$vacc_souhaitable)/3
x <- ds_3 %>%
  select(score_attitude, vacc_necessaire, vacc_acceptable, vacc_souhaitable)


# Trust and confidence score: score based on 9 questions
## Recoding of the first question
ds_3[ds_3$`C_17_Les_vaccins_r_es_enfants_sont_s_rs` == "non", "vacc_sur_5ans"] <- 1
ds_3[ds_3$`C_17_Les_vaccins_r_es_enfants_sont_s_rs` == "ne_sait_pas", "vacc_sur_5ans"] <- 2
# "ne_sait_pas" si oui (nécessaire) ou "ne_sait_pas" si non (pas nécessaire) donc neutral answer like "peut-être"
ds_3[ds_3$`C_17_Les_vaccins_r_es_enfants_sont_s_rs` == "peut_tre", "vacc_sur_5ans"] <- 2
ds_3[ds_3$`C_17_Les_vaccins_r_es_enfants_sont_s_rs` == "oui", "vacc_sur_5ans"] <- 3
ds_3[ds_3$`C_17_Les_vaccins_r_es_enfants_sont_s_rs` == "refus_de_r_pondre ", "vacc_sur_5ans"] <- NA

## Recoding of the second question
ds_3[ds_3$`C_18_Il_est_importa_les_recommandations` == "non", "follow_recommandation"] <- 1
ds_3[ds_3$`C_18_Il_est_importa_les_recommandations` == "ne_sait_pas", "follow_recommandation"] <- 2
ds_3[ds_3$`C_18_Il_est_importa_les_recommandations` == "peut_tre", "follow_recommandation"] <- 2
ds_3[ds_3$`C_18_Il_est_importa_les_recommandations` == "oui", "follow_recommandation"] <- 3
ds_3[ds_3$`C_18_Il_est_importa_les_recommandations` == "refus_de_r_pondre ", "follow_recommandation"] <- NA

## Recoding of the third question
ds_3[ds_3$`E_4_Avez_vous_avez_de_nouveaux_vaccins_` == "non", "conf_scientific"] <- 1
ds_3[ds_3$`E_4_Avez_vous_avez_de_nouveaux_vaccins_` == "ne_sait_pas", "conf_scientific"] <- 2
ds_3[ds_3$`E_4_Avez_vous_avez_de_nouveaux_vaccins_` == "un_peu", "conf_scientific"] <- 3
ds_3[ds_3$`E_4_Avez_vous_avez_de_nouveaux_vaccins_` == "oui", "conf_scientific"] <- 4
ds_3[ds_3$`E_4_Avez_vous_avez_de_nouveaux_vaccins_` == "refus_de_r_pondre ", "conf_scientific"] <- NA

## Recoding of the fourth question
ds_3[ds_3$`E_6_Avez_vous_avez_s_vaccins_autoris_s_` == "non", "conf_ministere"] <- 1
ds_3[ds_3$`E_6_Avez_vous_avez_s_vaccins_autoris_s_` == "ne_sait_pas", "conf_ministere"] <- 2
ds_3[ds_3$`E_6_Avez_vous_avez_s_vaccins_autoris_s_` == "un_peu", "conf_ministere"] <- 3
ds_3[ds_3$`E_6_Avez_vous_avez_s_vaccins_autoris_s_` == "oui", "conf_ministere"] <- 4
ds_3[ds_3$`E_6_Avez_vous_avez_s_vaccins_autoris_s_` == "refus_de_r_pondre ", "conf_ministere"] <- NA

## Recoding of the fifth question
ds_3[ds_3$`E_7_En_la_personne_cin_j_ai_confiance_` == "non" & !is.na(ds_3$`E_7_En_la_personne_cin_j_ai_confiance_`), "conf_vaccinateur"] <- 1
ds_3[ds_3$`E_7_En_la_personne_cin_j_ai_confiance_` == "ne_sait_pas" & !is.na(ds_3$`E_7_En_la_personne_cin_j_ai_confiance_`), "conf_vaccinateur"] <- 2
ds_3[ds_3$`E_7_En_la_personne_cin_j_ai_confiance_` == "un_peu" & !is.na(ds_3$`E_7_En_la_personne_cin_j_ai_confiance_`), "conf_vaccinateur"] <- 3
ds_3[ds_3$`E_7_En_la_personne_cin_j_ai_confiance_` == "oui" & !is.na(ds_3$`E_7_En_la_personne_cin_j_ai_confiance_`), "conf_vaccinateur"] <- 4
ds_3[ds_3$`E_7_En_la_personne_cin_j_ai_confiance_` == "refus_de_r_pondre" & !is.na(ds_3$`E_7_En_la_personne_cin_j_ai_confiance_`), "conf_vaccinateur"] <- NA

## Recoding of the sixth question
ds_3[ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_` == "non" & !is.na(ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_`), "conf_centre_vacc"] <- 1
ds_3[ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_` == "ne_sait_pas" & !is.na(ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_`), "conf_centre_vacc"] <- 2
ds_3[ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_` == "un_peu" & !is.na(ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_`), "conf_centre_vacc"] <- 3
ds_3[ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_` == "oui" & !is.na(ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_`), "conf_centre_vacc"] <- 4
ds_3[ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_` == "refus_de_r_pondre" & !is.na(ds_3$`E_8_Dans_le_centre_ion_j_ai_confiance_`), "conf_centre_vacc"] <- NA

## Recoding of the seven question
ds_3[ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_` == "non" & !is.na(ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_`), "conf_systm_sante"] <- 1
ds_3[ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_` == "ne_sait_pas" & !is.na(ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_`), "conf_systm_sante"] <- 2
ds_3[ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_` == "un_peu" & !is.na(ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_`), "conf_systm_sante"] <- 3
ds_3[ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_` == "oui" & !is.na(ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_`), "conf_systm_sante"] <- 4
ds_3[ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_` == "refus_de_r_pondre" & !is.na(ds_3$`E_9_Dans_le_syst_me_des_j_ai_confiance_`), "conf_systm_sante"] <- NA

## Recoding of the eighth question
ds_3[ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_` == "non" & !is.na(ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_`), "conf_personel_fiable"] <- 1
ds_3[ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_` == "ne_sait_pas" & !is.na(ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_`), "conf_personel_fiable"] <- 2
ds_3[ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_` == "un_peu" & !is.na(ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_`), "conf_personel_fiable"] <- 3
ds_3[ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_` == "oui" & !is.na(ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_`), "conf_personel_fiable"] <- 4
ds_3[ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_` == "refus_de_r_pondre" & !is.na(ds_3$`E_10_Pensez_vous_qu_sur_la_vaccination_`), "conf_personel_fiable"] <- NA

## Recoding of the nine question
ds_3[ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins` == "non" & !is.na(ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins`), "conf_personel_protege"] <- 1
ds_3[ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins` == "ne_sait_pas" & !is.na(ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins`), "conf_personel_protege"] <- 2
ds_3[ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins` == "peut_tre" & !is.na(ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins`), "conf_personel_protege"] <- 2
ds_3[ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins` == "oui" & !is.na(ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins`), "conf_personel_protege"] <- 3
ds_3[ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins` == "refus_de_r_pondre" & !is.na(ds_3$`E_11_Pensez_vous_qu_commande_des_vaccins`), "conf_personel_protege"] <- NA

## Trust and confidence score = mean score on the 9 questions
ds_3$score_confiance <- (ds_3$vacc_sur_5ans + ds_3$follow_recommandation + ds_3$conf_scientific
                         + ds_3$conf_ministere + ds_3$conf_vaccinateur + ds_3$conf_centre_vacc 
                         + ds_3$conf_systm_sante + ds_3$conf_personel_fiable + ds_3$conf_personel_protege )/9



# Risk perception side effect score: score based on 1 question
## Recoding of the question
ds_3[ds_3$`C_20_Si_je_me_fais_fets_secondaires_est` == "tr_s__lev" &!is.na(ds_3$`C_20_Si_je_me_fais_fets_secondaires_est`), "score_risk_side_effect"] <- 1
ds_3[ds_3$`C_20_Si_je_me_fais_fets_secondaires_est` == "ne_sait_pas" &!is.na(ds_3$`C_20_Si_je_me_fais_fets_secondaires_est`), "score_risk_side_effect"] <- 2
ds_3[ds_3$`C_20_Si_je_me_fais_fets_secondaires_est` == "tr_s_faible" &!is.na(ds_3$`C_20_Si_je_me_fais_fets_secondaires_est`), "score_risk_side_effect"] <- 3
ds_3[ds_3$`C_20_Si_je_me_fais_fets_secondaires_est` == "refus_de_r_pondre" &!is.na(ds_3$`C_20_Si_je_me_fais_fets_secondaires_est`), "score_risk_side_effect"] <- NA


# Feeling informed score: score based on 1 question
## Recoding of the question
ds_3[ds_3$`D_31_De_mani_re_g_n_vous_faire_vacciner_` == "non" &!is.na(ds_3$`D_31_De_mani_re_g_n_vous_faire_vacciner_`), "score_feel_info"] <- 1
ds_3[ds_3$`D_31_De_mani_re_g_n_vous_faire_vacciner_` == "ne_sait_pas" &!is.na(ds_3$`D_31_De_mani_re_g_n_vous_faire_vacciner_`), "score_feel_info"] <- 2
ds_3[ds_3$`D_31_De_mani_re_g_n_vous_faire_vacciner_` == "oui" &!is.na(ds_3$`D_31_De_mani_re_g_n_vous_faire_vacciner_`), "score_feel_info"] <- 3
ds_3[ds_3$`D_31_De_mani_re_g_n_vous_faire_vacciner_` == "refus_de_r_pondre" &!is.na(ds_3$`D_31_De_mani_re_g_n_vous_faire_vacciner_`), "score_feel_info"] <- NA


# Risk perception disease score: score based on 1 question
## Recoding of the question
ds_3[ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est` == "tr_s_faible" &!is.na(ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est`), "score_risk_maladie"] <- 1
ds_3[ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est` == "le_risque_est_le_m_me " &!is.na(ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est`), "score_risk_maladie"] <- 2
ds_3[ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est` == "ne_sait_pas" &!is.na(ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est`), "score_risk_maladie"] <- 3
# NSP is better than "le risque est le même" because it has no false perception on the risk to get the disease
ds_3[ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est` == "tr_s__lev" &!is.na(ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est`), "score_risk_maladie"] <- 4
ds_3[ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est` == "refus_de_r_pondre "& !is.na(ds_3$`C_19_Si_je_ne_me_fa_adie_infectieuse_est`), "score_risk_maladie"] <- NA


# Supply chain score: score based on 3 questions
## Recoding of the first question
ds_3[ds_3$`C_3_Avez_vous_connu_gnants_dans_un_PS_CS` == "non" &!is.na(ds_3$`C_3_Avez_vous_connu_gnants_dans_un_PS_CS`), "chaine_froid"] <- 3
ds_3[ds_3$`C_3_Avez_vous_connu_gnants_dans_un_PS_CS` == "ne_sait_pas" &!is.na(ds_3$`C_3_Avez_vous_connu_gnants_dans_un_PS_CS`), "chaine_froid"] <- 2
ds_3[ds_3$`C_3_Avez_vous_connu_gnants_dans_un_PS_CS` == "oui" &!is.na(ds_3$`C_3_Avez_vous_connu_gnants_dans_un_PS_CS`), "chaine_froid"] <- 1

## Recoding of the second question
ds_3[ds_3$`C_4_Y_a_t_il_suffis_it_s_de_vaccination_` == "non" &!is.na(ds_3$`C_4_Y_a_t_il_suffis_it_s_de_vaccination_`), "material_stock"] <- 1
ds_3[ds_3$`C_4_Y_a_t_il_suffis_it_s_de_vaccination_` == "ne_sait_pas" &!is.na(ds_3$`C_4_Y_a_t_il_suffis_it_s_de_vaccination_`), "material_stock"] <- 2
ds_3[ds_3$`C_4_Y_a_t_il_suffis_it_s_de_vaccination_` == "oui" &!is.na(ds_3$`C_4_Y_a_t_il_suffis_it_s_de_vaccination_`), "material_stock"] <- 3

## Recoding of the third question
ds_3[ds_3$`C_5_En_g_n_ral_dep_de_vaccins_en_stock_` == "non" &!is.na(ds_3$`C_5_En_g_n_ral_dep_de_vaccins_en_stock_`), "vacc_en_stock"] <- 1
ds_3[ds_3$`C_5_En_g_n_ral_dep_de_vaccins_en_stock_` == "ne_sait_pas" &!is.na(ds_3$`C_5_En_g_n_ral_dep_de_vaccins_en_stock_`), "vacc_en_stock"] <- 2
ds_3[ds_3$`C_5_En_g_n_ral_dep_de_vaccins_en_stock_` == "oui" &!is.na(ds_3$`C_5_En_g_n_ral_dep_de_vaccins_en_stock_`), "vacc_en_stock"] <- 3

## Supply chain score = mean score on the 3 questions
ds_3$score_stock <- (ds_3$chaine_froid + ds_3$material_stock + ds_3$vacc_en_stock)/3

# Create urban/rural variable
ds_3 <- ds_3 %>%
  mutate(urban_zs = case_when(
    str_detect(zones_sante, "limete") == TRUE ~ "urban",
    str_detect(zones_sante, "boma") == TRUE ~ "urban",
    str_detect(zones_sante, "matadi") == TRUE ~ "urban",
    str_detect(zones_sante, "kenge") == TRUE ~ "urban",
    str_detect(zones_sante, "kikwit_n") == TRUE ~ "urban",
    str_detect(zones_sante, "nsele") == TRUE ~ "rural",
    str_detect(zones_sante, "mbanza_n") == TRUE ~ "rural",
    str_detect(zones_sante, "boko") == TRUE ~ "rural",
    str_detect(zones_sante, "masi_man") == TRUE ~ "rural",
    str_detect(zones_sante, "lusanga") == TRUE ~ "rural")
  )

table(ds_3$urban_zs, useNA = "always")
table(ds_3$urban_zs, ds_3$zones_sante, useNA = "always")


# Study population analysis
####################################################
# Table 1
table(ds_3$zones_sante, exclude= NULL)
table(ds_3$"B_2_Vous_tes", exclude= NULL)

ds_3$age <- as.numeric(ds_3$B_1_Quel_est_votre_ge)
ds_3$age[ds_3$age==99]  <- NA

table(ds_3$age, exclude= NULL)
quantile(ds_3$age, probs=0.50, na.rm = TRUE)
quantile(ds_3$age, probs=0.25, na.rm = TRUE)
quantile(ds_3$age, probs=0.75, na.rm = TRUE)

table(ds_3$"A_4_Cat_gorie_du_participant", exclude= NULL)
ds_3 %>% 
  group_by(A_4_Cat_gorie_du_participant) %>%  
  summarise(n = n(),
            median = median(age, na.rm= TRUE), 
            quant_25 = quantile(age, probs=0.25, na.rm= TRUE),
            quant_75 = quantile(age, probs=0.75, na.rm= TRUE))

table(ds_3$"B_4_Quel_est_votre_niveau_d_ducation_" , exclude= NULL)


# Analysis of KAP concepts
####################################################

# KAP concepts by urbanization status: urban vs rural areas 
############################################################

###########################
# score_gen_belief
###########################
wilcox.test(score_gen_belief ~ urban_zs, data = ds_3) 

# Histogram to normalise (on y axis) on the number of participants per category (urban/rural areas)
ggplot(ds_3,aes(x=score_gen_belief,fill=urban_zs))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score belief",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("rural areas", "urban areas" )) +
  theme_unikn()

###########################
# score_knowledge
###########################
wilcox.test(score_knowledge ~ urban_zs, data = ds_3) 

ggplot(ds_3,aes(x=score_knowledge,fill=urban_zs))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score knowledge",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("rural areas", "urban areas" )) +
  theme_unikn()

###########################
# score_attitude
###########################
wilcox.test(score_attitude ~ urban_zs, data = ds_3)

ggplot(ds_3,aes(x=score_attitude,fill=urban_zs))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score attitude",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("rural areas", "urban areas" )) +
  theme_unikn()

########################################
# score risk perception side effects
########################################
wilcox.test(score_risk_side_effect ~ urban_zs, data = ds_3)

ggplot(ds_3,aes(x=score_risk_side_effect,fill=urban_zs))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score perception of side effects",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("rural areas", "urban areas" )) +
  theme_unikn()

#######################
# score_feel_informed
#######################
wilcox.test(score_feel_info ~ urban_zs, data = ds_3)

ggplot(ds_3,aes(x=score_feel_info,fill=urban_zs))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score feeling informed",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("rural areas", "urban areas" )) +
  theme_unikn()

########################################
# score risk perception on VPDs
########################################
wilcox.test(score_risk_maladie ~ urban_zs, data = ds_3)

ggplot(ds_3,aes(x=score_risk_maladie,fill=urban_zs))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score perception on vaccine preventable disease",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("rural areas", "urban areas" )) +
  theme_unikn()


###########################
# score_confiance (Trust)
###########################
ds_4 <- ds_3 %>%
  filter(`A_4_Cat_gorie_du_participant` == "parent_d_enfant_de_moins_de_5ans")

wilcox.test(score_confiance ~ urban_zs, data = ds_4) 

tab <- table(ds_4$score_confiance, ds_4$urban_zs, useNA = "always")
cprop(tab)

table(ds_4$conf_scientific, useNA = "always")
table(ds_4$conf_ministere, useNA = "always")
table(ds_4$conf_vaccinateur, useNA = "always")
table(ds_4$conf_centre_vacc, useNA = "always")
table(ds_4$conf_systm_sante, useNA = "always")

ds_3$score_confiance <- (ds_3$vacc_sur_5ans + ds_3$follow_recommandation + ds_3$conf_scientific
                         + ds_3$conf_ministere + ds_3$conf_vaccinateur + ds_3$conf_centre_vacc 
                         + ds_3$conf_systm_sante + ds_3$conf_personel_fiable + ds_3$conf_personel_protege )/9



# KAP concepts by group of participants
########################################

###########################
# score_gen_belief
###########################
kruskal.test(score_gen_belief ~ `A_4_Cat_gorie_du_participant`, data = ds_3)

ggplot(ds_3,aes(x=score_gen_belief,fill=`A_4_Cat_gorie_du_participant`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score belief",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("parents", "elderly", "health care workers"), palette = "Greens") +
  theme_unikn()

###########################
# score_knowledge
###########################
kruskal.test(score_knowledge ~ `A_4_Cat_gorie_du_participant`, data = ds_3)

ggplot(ds_3,aes(x=score_knowledge,fill=`A_4_Cat_gorie_du_participant`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score knowledge",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("parents", "elderly", "health care workers"), palette = "Greens") +
  theme_unikn()

###########################
# score_attitude
###########################
kruskal.test(score_attitude ~ `A_4_Cat_gorie_du_participant`, data = ds_3)

ggplot(ds_3,aes(x=score_attitude,fill=`A_4_Cat_gorie_du_participant`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score attitude",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("parents", "elderly", "health care workers"), palette = "Greens") +
  theme_unikn()

########################################
# score risk perception side effects
########################################
kruskal.test(score_risk_side_effect ~ `A_4_Cat_gorie_du_participant`, data = ds_3)

ggplot(ds_3,aes(x=score_risk_side_effect,fill=`A_4_Cat_gorie_du_participant`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score perception of side effects",
       y = "Percentage (%)")+
  scale_fill_brewer(name = "", labels = c("parents", "elderly", "health care workers"), palette = "Greens") +
  theme_unikn()

dunn.test(ds_3$score_risk_side_effect, ds_3$`A_4_Cat_gorie_du_participant`, method="bh")

#######################
# score_feel_informed
#######################
kruskal.test(score_feel_info ~ `A_4_Cat_gorie_du_participant`, data = ds_3)

ggplot(ds_3,aes(x=score_feel_info,fill=`A_4_Cat_gorie_du_participant`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score perception of feeling informed",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("parents", "elderly", "health care workers"), palette = "Greens") +
  theme_unikn()

dunn.test(ds_3$score_feel_info, ds_3$`A_4_Cat_gorie_du_participant`, method="bh")

########################################
# score risk perception of the disease
########################################
kruskal.test(score_risk_maladie ~ `A_4_Cat_gorie_du_participant`, data = ds_3)

ggplot(ds_3,aes(x=score_risk_maladie,fill=`A_4_Cat_gorie_du_participant`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score perception on vaccine preventable disease",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("parents", "elderly", "health care workers"), palette = "Greens") +
  theme_unikn()


# Vaccine acceptance analysis
###################################

###################################################################################
# Computing score of vaccine acceptance routine vaccinations: BCG, measles, polio
###################################################################################
## Vaccine acceptance score BCG
ds_3[ds_3$`D_32_Le_BCG_est_des_en_accord_avec_cela_` == "non", "acceptance_bcg"] <- 1
ds_3[ds_3$`D_32_Le_BCG_est_des_en_accord_avec_cela_` == "ne_sait_pas", "acceptance_bcg"] <- 2
ds_3[ds_3$`D_32_Le_BCG_est_des_en_accord_avec_cela_` == "peut_tre", "acceptance_bcg"] <- 2
ds_3[ds_3$`D_32_Le_BCG_est_des_en_accord_avec_cela_` == "oui", "acceptance_bcg"] <- 3
ds_3[ds_3$`D_32_Le_BCG_est_des_en_accord_avec_cela_` == "refus_de_r_pondre ", "acceptance_bcg"] <- NA

ds_3 %>% 
  group_by(acceptance_bcg) %>%  
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## Vaccine acceptance score measles
# For measles, score based on 2 questions : questions D_34 (agree on vaccination age) and D_35 (if outbreak, agree to vaccinate) 
# are not limited to parents so use them both to compute the score
ds_3[ds_3$`D_34_La_vaccination_t_ge_de_vaccination` == "non__cela_est_trop_t_t", "acceptance_measles_1"] <- 1
ds_3[ds_3$`D_34_La_vaccination_t_ge_de_vaccination` == "non__cela_est_trop_tard", "acceptance_measles_1"] <- 1
ds_3[ds_3$`D_34_La_vaccination_t_ge_de_vaccination` == "ne_sait_pas", "acceptance_measles_1"] <- 2
ds_3[ds_3$`D_34_La_vaccination_t_ge_de_vaccination` == "peut_tre", "acceptance_measles_1"] <- 2
ds_3[ds_3$`D_34_La_vaccination_t_ge_de_vaccination` == "oui", "acceptance_measles_1"] <- 3
ds_3[ds_3$`D_34_La_vaccination_t_ge_de_vaccination` == "refus_de_r_pondre ", "acceptance_measles_1"] <- NA
# I considered the two options 'non" as equal (=1) because in both options you disagree to vaccinate for measles

ds_3[ds_3$`D_35_Si_une_pid_mi_cciner_votre_enfant_` == "non", "acceptance_measles_2"] <- 1
ds_3[ds_3$`D_35_Si_une_pid_mi_cciner_votre_enfant_` == "ne_sait_pas", "acceptance_measles_2"] <- 2
ds_3[ds_3$`D_35_Si_une_pid_mi_cciner_votre_enfant_` == "peut_tre", "acceptance_measles_2"] <- 2
ds_3[ds_3$`D_35_Si_une_pid_mi_cciner_votre_enfant_` == "oui", "acceptance_measles_2"] <- 3
ds_3[ds_3$`D_35_Si_une_pid_mi_cciner_votre_enfant_` == "refus_de_r_pondre ", "acceptance_measles_2"] <- NA

ds_3$acceptance_measles <- (ds_3$acceptance_measles_1 + ds_3$acceptance_measles_2)/2

ds_3 %>% 
  group_by(acceptance_measles) %>%  
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## Vaccine acceptance score polio (if outbreak, agree to vaccinate)
ds_3[ds_3$`D_37_Si_une_pid_mi_vous_d_tre_vaccin_` == "non", "acceptance_polio"] <- 1
ds_3[ds_3$`D_37_Si_une_pid_mi_vous_d_tre_vaccin_` == "ne_sait_pas", "acceptance_polio"] <- 2
ds_3[ds_3$`D_37_Si_une_pid_mi_vous_d_tre_vaccin_` == "peut_tre", "acceptance_polio"] <- 2
ds_3[ds_3$`D_37_Si_une_pid_mi_vous_d_tre_vaccin_` == "oui", "acceptance_polio"] <- 3
ds_3[ds_3$`D_37_Si_une_pid_mi_vous_d_tre_vaccin_` == "refus_de_r_pondre ", "acceptance_polio"] <- NA

ds_3 %>% 
  group_by(acceptance_polio) %>%  
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## Vaccine acceptance score routine vaccinations: BCG, measles, polio
ds_3$score_acceptance_routine <- (ds_3$acceptance_bcg + ds_3$acceptance_measles +  ds_3$acceptance_polio)/3

ds_3 %>% 
  group_by(score_acceptance_routine) %>%  
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(ds_3,aes(x=score_acceptance_routine))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge")+
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for routine vaccinations",
       y = "Percentage (%)") +
  theme_unikn()


########################################################################################
# Computing score of vaccine acceptance outbreak vaccinations: cholera, Ebola, COVID-19
#######################################################################################
## Vaccine acceptance score cholera
ds_3[ds_3$`D_40_Accepteriez_vo_vous_tait_propos_` == "non", "acceptance_cholera"] <- 1
ds_3[ds_3$`D_40_Accepteriez_vo_vous_tait_propos_` == "ne_sait_pas", "acceptance_cholera"] <- 2
ds_3[ds_3$`D_40_Accepteriez_vo_vous_tait_propos_` == "peut_tre", "acceptance_cholera"] <- 2
ds_3[ds_3$`D_40_Accepteriez_vo_vous_tait_propos_` == "oui", "acceptance_cholera"] <- 3
ds_3[ds_3$`D_40_Accepteriez_vo_vous_tait_propos_` == "refus_de_r_pondre ", "acceptance_cholera"] <- NA

ds_3 %>% 
  group_by(acceptance_cholera) %>%  
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## Vaccine acceptance score Ebola
ds_3[ds_3$`D_42_Accepteriez_vo_vous_tait_propos_` == "non", "acceptance_ebola"] <- 1
ds_3[ds_3$`D_42_Accepteriez_vo_vous_tait_propos_` == "ne_sait_pas", "acceptance_ebola"] <- 2
ds_3[ds_3$`D_42_Accepteriez_vo_vous_tait_propos_` == "peut_tre", "acceptance_ebola"] <- 2
ds_3[ds_3$`D_42_Accepteriez_vo_vous_tait_propos_` == "oui", "acceptance_ebola"] <- 3
ds_3[ds_3$`D_42_Accepteriez_vo_vous_tait_propos_` == "refus_de_r_pondre ", "acceptance_ebola"] <- NA

ds_3 %>% 
  group_by(acceptance_ebola) %>%  
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## Vaccine acceptance score COVID-19
ds_3[ds_3$`D_43_Si_un_vaccin_s_vous_d_tre_vaccin_` == "non", "acceptance_covid"] <- 1
ds_3[ds_3$`D_43_Si_un_vaccin_s_vous_d_tre_vaccin_` == "ne_sait_pas", "acceptance_covid"] <- 2
ds_3[ds_3$`D_43_Si_un_vaccin_s_vous_d_tre_vaccin_` == "peut_tre", "acceptance_covid"] <- 2
ds_3[ds_3$`D_43_Si_un_vaccin_s_vous_d_tre_vaccin_` == "oui", "acceptance_covid"] <- 3
ds_3[ds_3$`D_43_Si_un_vaccin_s_vous_d_tre_vaccin_` == "refus_de_r_pondre ", "acceptance_covid"] <- NA

ds_3 %>% 
  group_by(acceptance_covid) %>%  
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

## Vaccine acceptance score outbreak
ds_3$score_acceptance_outbreak <- (ds_3$acceptance_cholera + ds_3$acceptance_ebola +  ds_3$acceptance_covid)/3

ds_3 %>% 
  group_by(score_acceptance_outbreak) %>%  
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

ggplot(ds_3,aes(x=score_acceptance_outbreak))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge")+
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for outbreak vaccinations",
       y = "Percentage (%)") +
  theme_unikn()


# Comparison of vaccine acceptance score outbreak vaccinations: urban vs rural areas
######################################################################################
wilcox.test(score_acceptance_outbreak ~ urban_zs, data = ds_3) 

ggplot(ds_3,aes(x=score_acceptance_outbreak,fill=urban_zs))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for outbreak vaccinations",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("rural areas", "urban areas")) +
  theme_unikn()


# Comparison of vaccine acceptance score outbreak vaccinations by group of participants
########################################################################################
kruskal.test(score_acceptance_outbreak ~ `A_4_Cat_gorie_du_participant`, data = ds_3)

ggplot(ds_3,aes(x=score_acceptance_outbreak,fill=`A_4_Cat_gorie_du_participant`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for outbreak vaccinations",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("parents", "elderly", "health care workers"), palette = "Greens") +
  theme_unikn()

dunn.test(ds_3$score_acceptance_outbreak, ds_3$`A_4_Cat_gorie_du_participant`, method="bh")


# Comparison of vaccine acceptance score outbreak vaccinations: belief categories
#######################################################################################
# Compute category of participants according to their score of belief
ds_3$score_gen_belief[is.na(ds_3$score_gen_belief)==TRUE] <- "NA"
ds_3[ds_3$score_gen_belief <2 , "belief_cat"] <- "disbelief"
ds_3[ds_3$score_gen_belief ==2 , "belief_cat"] <- "neutral_belief"
ds_3[ds_3$score_gen_belief >2 , "belief_cat"] <- "positive_belief"

kruskal.test(score_acceptance_outbreak ~ belief_cat, data = ds_3)

# Relevel group factor
ds_3$belief_cat <- factor(ds_3$belief_cat,                
                             levels = c("positive_belief", "neutral_belief", "disbelief"))

ggplot(ds_3,aes(x=score_acceptance_outbreak,fill=`belief_cat`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge")+
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for outbreak vaccinations",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = function(x) gsub("_", " ", x, fixed = TRUE), palette = "Reds") +
  theme_unikn() +
  theme(legend.position="bottom")

dunn.test(ds_3$score_acceptance_outbreak, ds_3$`belief_cat`, method="bh")

# Comparison of vaccine acceptance score outbreak vaccinations: knowledge categories
######################################################################################
# Compute category of participants according to their score of knowledge
ds_3$score_knowledge[is.na(ds_3$score_knowledge)==TRUE] <- "NA"
ds_3[ds_3$score_knowledge <2 , "knowledge_cat"] <- "incorrect_knowledge"
ds_3[ds_3$score_knowledge ==2 , "knowledge_cat"] <- "insufficent_knowledge"
ds_3[ds_3$score_knowledge >2 , "knowledge_cat"] <- "correct_knowledge"

kruskal.test(score_acceptance_outbreak ~ knowledge_cat, data = ds_3)

# Relevel group factor
ds_3$knowledge_cat <- factor(ds_3$knowledge_cat,                
                          levels = c("correct_knowledge", "insufficent_knowledge", "incorrect_knowledge"))

ggplot(ds_3,aes(x=score_acceptance_outbreak,fill=`knowledge_cat`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for outbreak vaccinations",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = function(x) gsub("_", " ", x, fixed = TRUE), palette = "Reds") +
  theme_unikn() +
  theme(legend.position="bottom")

dunn.test(ds_3$score_acceptance_outbreak, ds_3$`knowledge_cat`, method="bh")

# Comparison of vaccine acceptance score outbreak vaccinations: attitude categories
####################################################################################
# Compute category of participants according to their score of attitude
ds_3$score_attitude[is.na(ds_3$score_attitude)==TRUE] <- "NA"
ds_3[ds_3$score_attitude <2 , "attitude_cat"] <- "negative_attitude"
ds_3[ds_3$score_attitude ==2 , "attitude_cat"] <- "neutral_attitude"
ds_3[ds_3$score_attitude >2 , "attitude_cat"] <- "positive_attitude"

kruskal.test(score_acceptance_outbreak ~ attitude_cat, data = ds_3)

# Relevel group factor
ds_3$attitude_cat <- factor(ds_3$attitude_cat,                
                             levels = c("positive_attitude", "neutral_attitude", "negative_attitude"))

ggplot(ds_3,aes(x=score_acceptance_outbreak,fill=`attitude_cat`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge")+
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for outbreak vaccinations",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = function(x) gsub("_", " ", x, fixed = TRUE), palette = "Reds") +
  theme_unikn() +
  theme(legend.position="bottom")

dunn.test(ds_3$score_acceptance_outbreak, ds_3$`attitude_cat`, method="bh")


# Comparison of vaccine acceptance score outbreak vaccinations: risk perception side effects categories
########################################################################################################
# Compute category of participants according to their score of risk perception of side effects
ds_3$score_risk_side_effect[is.na(ds_3$score_risk_side_effect)==TRUE] <- "NA"
ds_3[ds_3$score_risk_side_effect <2 , "risk_perception_side_effect_cat"] <- "high_risk_side_effects"
ds_3[ds_3$score_risk_side_effect ==2 , "risk_perception_side_effect_cat"] <- "neutral_risk_side_effects"
ds_3[ds_3$score_risk_side_effect >2 , "risk_perception_side_effect_cat"] <- "low_risk_side_effects"

kruskal.test(score_acceptance_outbreak ~ risk_perception_side_effect_cat, data = ds_3)

# Relevel group factor
ds_3$risk_perception_side_effect_cat <- factor(ds_3$risk_perception_side_effect_cat,                
                            levels = c("low_risk_side_effects", "neutral_risk_side_effects", "high_risk_side_effects"))

ggplot(ds_3,aes(x=score_acceptance_outbreak,fill=`risk_perception_side_effect_cat`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge")+
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for outbreak vaccinations",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("perceived low risk for side effects", "neutral perception of risk for side effects",
                                         "perceived high risk for side effects"), 
                    palette = "Reds") +
  theme_unikn() +
  theme(legend.position="bottom")

dunn.test(ds_3$score_acceptance_outbreak, ds_3$`risk_perception_side_effect_cat`, method="bh")


# Comparison of the scores intention to vaccinate outbreak : feeling informed categories
#########################################################################################
# Compute category of participants according to their score of feeling informed
ds_3$score_feel_info[is.na(ds_3$score_feel_info)==TRUE] <- "NA"
ds_3[ds_3$score_feel_info <2 , "feel_info_cat"] <- "not_feel_info"
ds_3[ds_3$score_feel_info ==2 , "feel_info_cat"] <- "dont_know_feel_info"
ds_3[ds_3$score_feel_info >2 , "feel_info_cat"] <- "feel_info"

kruskal.test(score_acceptance_outbreak ~ feel_info_cat, data = ds_3)

# Relevel group factor
ds_3$feel_info_cat <- factor(ds_3$feel_info_cat,                
                              levels = c("feel_info", "dont_know_feel_info", "not_feel_info"))

ggplot(ds_3,aes(x=score_acceptance_outbreak,fill=`feel_info_cat`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for outbreak vaccinations",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("sufficently informed", "don't know if insufficently informed", 
                                            "insufficently informed"), palette = "Reds") +
  theme_unikn() +
  theme(legend.position="bottom")

dunn.test(ds_3$score_acceptance_outbreak, ds_3$`feel_info_cat`, method="bh")


# Comparison of the scores intention to vaccinate outbreak : risk perception disease categories
################################################################################################
# Compute category of participants according to their score of risk perception disease
ds_3$score_risk_maladie[is.na(ds_3$score_risk_maladie)==TRUE] <- "NA"
ds_3[ds_3$score_risk_maladie <3 , "risk_perception_disease_cat"] <- "low_risk_vaccine_preventable_disease"
ds_3[ds_3$score_risk_maladie ==3 , "risk_perception_disease_cat"] <- "neutral_risk_vaccine_preventable_disease"
ds_3[ds_3$score_risk_maladie >3 , "risk_perception_disease_cat"] <- "high_risk_vaccine_preventable_disease"

kruskal.test(score_acceptance_outbreak ~ risk_perception_disease_cat, data = ds_3)

# Relevel group factor
ds_3$risk_perception_disease_cat <- factor(ds_3$risk_perception_disease_cat,
                             levels = c("high_risk_vaccine_preventable_disease", "neutral_risk_vaccine_preventable_disease",
                                        "low_risk_vaccine_preventable_disease"))

ggplot(ds_3,aes(x=score_acceptance_outbreak,fill=`risk_perception_disease_cat`)) +
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge") +
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for outbreak vaccinations",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("perceived high risk of disease", "neutral perception of risk for disease", 
                                          "perceived low risk of disease"), palette = "Reds") +
  theme_unikn() +
  theme(legend.position="bottom")

dunn.test(ds_3$score_acceptance_outbreak, ds_3$`risk_perception_disease_cat`, method="bh")


# Vaccine acceptance when people know recent cases
###################################################
# Cholera
ds_cholera <- ds_3 %>%
  filter(`D_28_Avez_vous_conn_r_cents_de_Chol_ra` == "oui" |`D_28_Avez_vous_conn_r_cents_de_Chol_ra` == "non")


# Relevel group factor
ds_cholera$`D_28_Avez_vous_conn_r_cents_de_Chol_ra` <- factor(ds_cholera$`D_28_Avez_vous_conn_r_cents_de_Chol_ra`,
                                           levels = c("oui", "non"))

ggplot(ds_cholera,aes(x=acceptance_cholera,fill=`D_28_Avez_vous_conn_r_cents_de_Chol_ra`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge")+
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for Cholera",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("knowledge of recent cases", "no knowledge of recent cases"), palette = "Reds") +
  theme_unikn()

wilcox.test(acceptance_cholera ~ `D_28_Avez_vous_conn_r_cents_de_Chol_ra`, data = ds_cholera)


# Ebola
ds_ebola <- ds_3 %>%
  filter(`D_29_Avez_vous_conn_cents_d_Ebola_29` == "oui" |`D_29_Avez_vous_conn_cents_d_Ebola_29` == "non")

# Relevel group factor
ds_ebola$`D_29_Avez_vous_conn_cents_d_Ebola_29` <- factor(ds_ebola$`D_29_Avez_vous_conn_cents_d_Ebola_29`,
                                                              levels = c("oui", "non"))

ggplot(ds_ebola,aes(x=acceptance_ebola,fill=`D_29_Avez_vous_conn_cents_d_Ebola_29`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge")+
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for Ebola",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("knowledge of recent cases", "no knowledge of recent cases"), palette = "Reds") +
  theme_unikn()

wilcox.test(acceptance_ebola ~ `D_29_Avez_vous_conn_cents_d_Ebola_29`, data = ds_ebola)


# COVID-19
ds_covid <- ds_3 %>%
  filter(`D_30_Avez_vous_conn_r_cents_de_COVID_19` == "oui" |`D_30_Avez_vous_conn_r_cents_de_COVID_19` == "non")

# Relevel group factor
ds_covid$`D_30_Avez_vous_conn_r_cents_de_COVID_19` <- factor(ds_covid$`D_30_Avez_vous_conn_r_cents_de_COVID_19`,
                                                          levels = c("oui", "non"))

ggplot(ds_covid,aes(x=acceptance_covid,fill=`D_30_Avez_vous_conn_r_cents_de_COVID_19`))+
  geom_histogram(aes(y=50*..density..),binwidth=0.5, position="dodge")+
  labs(fill = "",
       title = "",
       x = "value score vaccine acceptance for COVID-19",
       y = "Percentage (%)") +
  scale_fill_brewer(name = "", labels = c("knowledge of recent cases", "no knowledge of recent cases"), palette = "Reds") +
  theme_unikn()

wilcox.test(acceptance_covid ~ `D_30_Avez_vous_conn_r_cents_de_COVID_19`, data = ds_covid)


# Supply chain analysis
########################
ds_5 <- ds_3 %>%
  filter(`A_4_Cat_gorie_du_participant` == "professionnel_de_sant")
table(ds_5$vacc_en_stock, useNA = "always")
table(ds_5$chaine_froid, useNA = "always")
table(ds_5$material_stock, useNA = "always")

ds_6 <- ds_5 %>%
  filter(chaine_froid == 1)
table(ds_6$material_stock, ds_6$vacc_en_stock, useNA = "always")

ds_7 <- ds_5 %>%
  filter(material_stock == 1)
table(ds_7$vacc_en_stock, ds_7$chaine_froid, useNA = "always")

ds_8 <- ds_5 %>%
  filter(vacc_en_stock == 1)
table(ds_8$material_stock, ds_8$chaine_froid, useNA = "always")
