# Script: Code for figures in Sep MSC, Veenman K, Vinkers CH, STRESS-EU Consortium. 2024. The STRESS-EU database: a European resource of human acute stress studies for the worldwide research community. Neuroscience Applied. https://doi.org/10.1016/j.nsa.2024.104063
# Author: Milou Sep
# Contact: m.s.c.sep(at)amsterdamumc.nl

# Input: central stored anonymous data (note, see www.stressdatabase.eu for the data access procedure)
# participants.csv contains all cortisol values and time points of the studies in the STRESS-EU database
# studies.csv contains all meta-information on the included studies in the STRESS-EU database
# Output: figures

# Script adapted with permission from https://doi.org/10.1016/j.psyneuen.2022.105735

# Environment preparation -------------------------------------------------
rm(list = ls())
library(readxl)
library(tidyverse)

# https://bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html
# # install.packages("gmodels")
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)

# Load data ---------------------------------------------------------------
participants <- read_excel("data/raw/participants.xlsx")
studies <- read_excel("data/raw/studies.xlsx", sheet = "meta_studies")
q_group <- read.csv("data/raw/questionnaires_classification.csv", sep=";") # questionnaire classification needed for Fig3; ask VB in two weeks

# rename to same names as script VB
meta <-studies %>% rename(study_id = `_id`,
                          acute_stress_grouped = Stress_test)
ipd <- rename(participants, study_id = studyId)
ipd$participant_id = rep(1:nrow(ipd)) # Add new ID number

meta <- meta %>%
  mutate(acute_stress_grouped = case_when(
    acute_stress_grouped %in% "P_SECPT" ~ "social_and_physical",
    study_id %in% "EV_exp38" ~ "social_and_physical",
    T ~ acute_stress_grouped)
  )

# recode spelling
ipd <- ipd %>%
  mutate(diagnosis = ifelse(
    diagnosis %in% c("Healthy", "healthy"), "healthy", diagnosis
  ))
glimpse(meta)
glimpse(ipd)

# General info -------------------------------------------------------------------------
paste("The database contains", nrow(ipd), "participants")

# participants by sex
ipd %>% 
  group_by(gender) %>% 
  count()

# experiments
paste("The database contains", length(unique(ipd$study_id)), "experiments")

# studies
n_published <- meta %>% 
  filter(!doi %in% c("not_applicable")) %>% 
  summarize(published_n = length(unique(doi)))

n_unpublished <- meta %>% 
  filter(doi == "not_applicable") %>% 
  summarize(unpublished_n = length(unique(study_id)))

n_published + n_unpublished

# control vs stress
ipd %>% 
  group_by(stress_control_condition) %>% 
  summarize(n_prop=length(study_id) / nrow(.))

# diagnosis
ipd %>% 
  mutate(diag = case_when(
    diagnosis %in% c("siblings", "control", "healthy") ~ "healthy", 
    diagnosis %in% c("not_available") ~ "not_available",
    T ~ "not_healthy"
  )) %>%
  group_by(diag) %>% 
  summarize(n_prop = length(study_id)/ nrow(.))

# contraceptives
ipd %>% 
  filter(gender == "female") %>%
  mutate(contraceptive = ifelse(contraceptive == "not_available", "not_available", "available")) %>%
  group_by(contraceptive) %>% 
  summarize(n_prop = length(study_id) / nrow(.))

ipd %>% 
  filter(gender == "female") %>%
  mutate(menstrual_phase = ifelse(menstrual_phase == "not_available", "not_available", "available")) %>%
  group_by(menstrual_phase) %>% 
  summarize(n_prop=length(study_id) / nrow(.))

## acute stress n exp and n participants
ipd %>% 
  left_join(meta %>% select(study_id, acute_stress_grouped)) %>% 
  group_by(acute_stress_grouped) %>% 
  summarize(n_exp = length(unique(study_id)), 
            n_part = length(unique(participant_id)))

# number of cortisol samples per study
ipd %>% 
  group_by(study_id) %>% 
  reframe(
    n_cort = str_count((cortisol_timepoint),",") +1
  ) %>% 
  ungroup() %>%
  summarise(
    mean=mean(n_cort),
    min=min(n_cort),
    max=max(n_cort)
  )

glimpse(meta)
glimpse(ipd)

# Figure 1A ---------------------------------------------------------------
part_label <-meta %>% 
  group_by(principal_investigator) %>% 
  summarize(n_tot = sum(sample_size_available),
            n_exp = paste(sum(length(unique(study_id)))))

g_participants <- 
  meta %>% 
  ggplot(aes(x=reorder(principal_investigator, -sample_size_available),y= sample_size_available)) +
  geom_bar(
    stat="identity", 
    fill = "white", color = "black") + 
  theme_bw()  +
  labs(x = "PI", y = expression("N"["participants"])) + 
  geom_text(data = part_label, aes(x = principal_investigator, y = n_tot + 40
                                   , label = paste(n_tot)), 
            parse = TRUE, color = "black"
            , size = 2.5) + 
  geom_text(data = part_label, aes(x = principal_investigator, y = n_tot + 25, label = n_exp), 
            parse = TRUE, color = "black"
            , size = 2.5) + 
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.text.x = element_text(size = 10)) +
  # remove x-labels
  theme(axis.text.x=element_blank(),
    axis.ticks.x=element_blank())

# Figure 1B ---------------------------------------------------------------
g_age <- ipd %>% 
  ggplot(aes(age_years)) + 
  geom_histogram(fill = "#7F7F7F") + 
  facet_grid(~gender) + 
  theme_bw() + 
  labs(x = "Age (years)", y = expression("N"["participants"])) + 
  theme(legend.position = "none", 
        text = element_text(size = 14)
  )

# Figure 1C ---------------------------------------------------------------
g_diagnosis <- ipd %>% 
  mutate(
    diagnosis = factor(diagnosis,
                       levels = c("healthy", "with diagnosis", "Info n.a.")),
    gender = str_to_sentence(gender)) %>% 
  ggplot(aes(diagnosis, fill = diagnosis)) +  
  geom_histogram(stat = "count", colour = "black") + 
  facet_grid(~gender) + 
  theme_bw() + 
  scale_fill_manual(values = c("white", "grey", "black")) + 
  labs(x = "", y = expression("N"["participants"])) + 
  theme(legend.position = "none", 
        text = element_text(size = 14),
        axis.text.x = element_text(angle=45, hjust=1)
  )

# Figure 1D ---------------------------------------------------------------
g_contraceptives <- ipd %>% 
  filter(gender == "female") %>%
  mutate(contraceptive = case_when(
    contraceptive %in% "no" ~ "Without contracept.",
    contraceptive %in% "yes" ~ "With contracept.",
    T ~ "Info n.a."), 
  gender = str_to_sentence(gender),
  contraceptive = factor(contraceptive, levels = c("With contracept.", "Without contracept.", "Info n.a."))) %>%
  ggplot(aes(contraceptive)) + 
  geom_histogram(stat = "count", colour = "black", fill = "white") + 
  facet_grid(~gender) +
  theme_bw() + 
  labs(x = "", y = expression("N"["participants"])) + 
  theme(legend.position = "none", 
        text = element_text(size = 14),
        axis.text.x = element_text(angle=45, hjust=1)
  )

# Figure 1E ---------------------------------------------------------------
g_stress <- meta %>% filter(acute_stress_grouped != 'Control') %>%
  mutate(acute_stress_grouped = str_to_sentence(str_replace_all(acute_stress_grouped, "_", " "))) %>%
  ggplot(aes(acute_stress_grouped)) +
  geom_histogram(stat = "count", colour = "black", fill = "white") + 
  theme_bw() + 
  labs(x = "", y = expression("N"["exp"])) + 
  theme(legend.position = "none", 
        text = element_text(size = 14),
        axis.text.x = element_text(angle=45, hjust=1)
  )

# Merge Figure 1 ----------------------------------------------------------
ggpubr::ggarrange(
  g_participants,
  ggpubr::ggarrange(
    g_age, 
    g_diagnosis,
    ggpubr::ggarrange(
      g_contraceptives, g_stress, ncol = 2, labels = c("d", "e")
      ),
    nrow = 3, labels = c("b", "c", "")),
  ncol = 2, labels = c("a", ""), widths = c(1.3, 1)
)
ggsave("results/figure1_stressEUpaper_demo.eps", width =10, height = 12)
ggsave("results/figure1_stressEUpaper_demo.jpg", width =12, height = 10)

# Figure 2 ----------------------------------------------------------------
# Examples of analyses for cortisol measurements. a) Heat map of cortisol timepoints across experiments. Grey = measurement present; white = measurement absent.
time_bin <- ipd %>%
  # get dois
  left_join( meta %>% select(ends_with("_id"), "doi"), by='study_id') %>%
  select(doi, cortisol_timepoint
  ) %>% 
  mutate(doi = str_replace_all(doi, "not_available", "unpublished")) %>%
  separate(cortisol_timepoint, into=as.character(paste0("T",rep(1:20))), sep=",") %>% #splits a single column into multiple columns
  gather(.,key="time_bin", value = "cortisol_timepoint", -doi,
         grep('T',names(.), value=T), na.rm = T) %>% 
  unique() %>% 
  mutate(cortisol_timepoint=as.numeric(cortisol_timepoint)) %>%
  mutate(time_bin = case_when(
    cortisol_timepoint < -30 ~ "-100 - -30", 
    cortisol_timepoint <= 0 ~ "-30 - 0", 
    cortisol_timepoint <= 10 ~ "1 - 10", 
    cortisol_timepoint <= 20 ~ "11 - 20", 
    cortisol_timepoint <= 30 ~ "21 - 30", 
    cortisol_timepoint <= 40 ~ "31 - 40", 
    cortisol_timepoint <= 50 ~ "41 - 50", 
    cortisol_timepoint <= 60 ~ "51 - 60", 
    cortisol_timepoint <= 70 ~ "61 - 70", 
    cortisol_timepoint <= 80 ~ "71 - 80", 
    cortisol_timepoint <= 90 ~ "81 - 90", 
    cortisol_timepoint <= 100 ~ "91 - 100", 
    cortisol_timepoint <= 120 ~ "101 - 120", 
    cortisol_timepoint <= 140 ~ "121 - 140", 
    T ~ "> 140"
  ))

# Time heat map - create matrix
time_mat <- time_bin %>% 
  # wide format bins
  select(-cortisol_timepoint) %>%
  unique() %>%
  group_by(doi, time_bin) %>% 
  count() %>%
  mutate(
    n = ifelse(n > 1, 1, n), 
    doi = factor(doi)) %>%
  pivot_wider(names_from = time_bin, values_from = n, values_fill = 0) %>%
  # to matrix
  relocate("-100 - -30", "-30 - 0", "1 - 10", "11 - 20", 
           "21 - 30", "31 - 40", "41 - 50", "51 - 60", 
           "61 - 70", "71 - 80", "81 - 90", "91 - 100", 
           "101 - 120", "121 - 140") %>%
  arrange(doi) %>%
  column_to_rownames("doi") %>%
  as.matrix() 

# specify colour legend for heat map
g_time <- Heatmap(time_mat, 
                  cluster_columns = FALSE, col = c("#FFFFFF", "#9B9B9B"), 
                  show_row_dend = FALSE, 
                  row_order = rank(rownames(time_mat)),
                  column_names_centered = TRUE, row_names_gp = gpar(fontsize = 7),
                  row_title_rot = 0, row_title_gp = gpar(size = 5),
                  border = TRUE,
                  # # legend
                  show_heatmap_legend = FALSE, column_title = "Time (binned)", column_title_side = "bottom"
) 
g_time
ggsave("results/figure2_stressEUpaper_cortisol.eps", plot=  grid.grabExpr(draw(g_time)), width =6, height = 10)
ggsave("results/figure2_stressEUpaper_cortisol.jpg", plot=  grid.grabExpr(draw(g_time)),width =6, height = 10)

# Figure 3 ----------------------------------------------------------------
# SELECT DATA FOR OUTCOME GRAPH AND PIVOT 'OUTCOMES' TO LONG FORMAT (i.e. one outcome per row)
meta_long_outcomes <- meta %>%
  # select relevant vars
  select(n=sample_size_available, ends_with("_id"), ends_with("tests"), ends_with("type"), 
         cognitive_task, alphaamylase_measured,subjectivestress_measured, cortisol_measured, doi) %>%
  # long format
  pivot_longer(cols = c(ends_with("tests"), ends_with("type"), ends_with("task"), ends_with("measured")),
               names_to = "outcome", values_to = "test")

# CHECK AND CORRECT FOR UNPUBLISHED
meta_long_outcomes %>% filter(doi == 'not_applicable') # non
meta_long_outcomes %>% filter(doi == 'unpublished') # non
meta_long_outcomes %>% filter(is.na(doi)) # non
meta_long_outcomes %>% filter(doi == 'not_available') # 24

meta_long_outcomes_corrected <- meta_long_outcomes %>%
 mutate(doi = str_replace_all(doi, "not_available", "unpublished"))
#check
meta_long_outcomes_corrected %>% filter(doi == 'not_available') # 0
meta_long_outcomes_corrected %>% filter(doi == 'unpublished') # 24
 
# RECODE TEST VARIABLES (if needed before pivot)
 meta_long_outcomes_recoded <- meta_long_outcomes_corrected %>%
  mutate(
    test= case_when(
      ## GENETICS
      str_detect(test, "candidate_genes|SNP") ~ "candidate_genes", # recode for specific database entry, with examples of candidate genes
      test %in% c("oragene_tubes_saliva") ~ "candidate_gene",
      str_detect(test, "GWAS|gwas|illumina|Axiom|Illumina") ~ "genome_wide", 
      str_detect(test, "epigenetics|450K_array|EWAS") ~ "epigenetics",
      # for stress measures
      str_detect(test, "no|not_applicable|not_availavle") ~ "not_available",
      str_detect(test, "yes") ~ str_remove(outcome, "_measured"),
      TRUE ~ test)
) 

# PIVOT 'TESTS' TO LONG FORMAT (i.e. one test per row)
meta_long_tests <- meta_long_outcomes_recoded %>%
   separate(test, into=as.character(paste0("test",rep(1:23))), ",") %>%
   pivot_longer(cols = starts_with("test"),
                names_to = "test_number", values_to = "test", values_drop_na = T) %>%
  # change text formatting
  mutate(test=ifelse(test=='',"not_available",test),
         test=trimws(test),
         test=str_replace(test, "etc.", "not_available"))

# REMOVE MISSINGS
meta_long_tests_cc <- meta_long_tests %>% filter(!test == "not_available")
glimpse(meta_long_tests_cc)

# CREATE TEST GROUPING
meta_long_tests_cc %>%
  mutate(
    test_grouped = case_when(
      ## IMAGING
      str_detect(test, "fMRI") ~ "fMRI", 
      str_detect(test, "MRI") ~ "MRI",
      str_detect(test, "EEG") ~ "EEG",
      ## OMICS
     outcome =="genetics_tests" ~ "genomics", 
      outcome == "epigenetics_tests" ~ "epigenomics", 
      outcome == "transcriptomics_tests" ~ "transcriptomics", 
      outcome == "metabolomics_tests" ~ "metabolomics", 
      ## Physiology
      # STRESS MARKERS (nb subjective stress at questionnaires section)
      str_detect(test, "heart_rate|HRV|HR|cardiac vagal tone|ECG") ~ "Heart rate",
      str_detect(test, "cortisol") ~ "Cortisol",
      str_detect(test, "alphaamylase") ~ "Alpha amylase",
      str_detect(test, "BP|baroreceptor_sensitivity|blood|Pulse") ~ "Blood pressure",
     str_detect(test, "breathing|respiration|lung") ~ "Breathing",
     str_detect(test, "dermal|skin_") ~ "Skin conductance/temperature",
     str_detect(test, "eyeblink") ~ "Eyeblink",
     outcome == "immunesystem_tests" ~ "immune system",
      ## COGNITION BEHAVIOR
      str_detect(test, "memory|Memory_Contextualization_Task|learning|recall|WM|picture_rating_task|15 woorden taak|Amsterdamse_Korte_Termijn_Geheugen_Taak|Fear_Generalization_Task|Source Monitoring Test") ~ "learning_and/or_memory",
      str_detect(test, glob2rx("emotion|fear_conditioning|freezing_behavior|approach_avoidance|Approach*avoidance|Sternberg_type_emotional_inhibition_task|Freeze-Fight*|Approach-Avoidance_Task|IAPS_n_back|iPANAT|AAT|Emotional_N-back_task|Emotional_stroop")) ~ "emotion_related",
      str_detect(test, glob2rx("neuropsychological|attention|CPT|Stop-signal_task|SMT|N-back_task|ANT|stroop*|Stroop*|mirror_drawing|digit_span_task|gaze_cue_task|mimicry_task|O-Span|face_processing|backward_masking_task|2back|intertemporal_choice_task|inhibitory_control*|stop_signal_task|cognitive_flexibility|word_fluency|trail_making_test|cognitive_task")) ~ "neuro_psych", 
      str_detect(test, "reward|ultimatum_game|dictator_game|decision_making|Columbia_Card_Task") ~ "reward_or_decision_making", 
      str_detect(test, "social|shooting_task|RMET|trust_game|Interpersonal_Choice_Task") ~ "social", 
      str_detect(test, "attention") ~ "attention",
      str_detect(test, "IQ|AH4|WISC|intelligence") ~ "IQ_related",
      ## questionnaires
      str_detect(test, glob2rx(paste(unique(q_group[q_group$grouping == "health_status",]$questionnaires), collapse='|'))) ~ "health_status", 
      str_detect(test, glob2rx(paste(unique(q_group[q_group$grouping == "severity_disease",]$questionnaires), collapse='|'))) ~ "severity_disease", 
      str_detect(test, glob2rx(paste(unique(q_group[q_group$grouping == "childhood_adversity",]$questionnaires), collapse='|'))) ~ "childhood_adversity", 
      str_detect(test, "childhood_trauma") ~ "childhood_adversity",
      str_detect(test, glob2rx(paste(unique(q_group[q_group$grouping == "life_events",]$questionnaires), collapse='|'))) ~ "life_events", 
      str_detect(test, glob2rx(paste(unique(q_group[q_group$grouping == "mood_and_personality",]$questionnaires), collapse='|'))) ~ "mood_and_personality", 
      str_detect(test, glob2rx(paste(unique(q_group[q_group$grouping == "diagnosis",]$questionnaires), collapse='|'))) ~ "diagnosis", 
      str_detect(test, glob2rx(paste(unique(q_group[q_group$grouping == "other",]$questionnaires), collapse='|'))) ~ "other", 
      str_detect(test, glob2rx(paste(unique(q_group[q_group$grouping == "perceived_stress",]$questionnaires), collapse='|'))) ~ "Subjective stress", 
      str_detect(test, "subjectivestress") ~ "Subjective stress",
      TRUE ~ "other"
      )
  ) -> meta_long_tests_grouped
 meta_long_tests_grouped$outcome %>% unique()
 meta_long_tests_grouped$test_grouped %>% unique()

# RECODE outcome labeling
meta_long_tests_grouped_recoded <- meta_long_tests_grouped %>%
  mutate(
    outcome=case_when(
      # Stress markers
      outcome %in% c("alphaamylase_measured", "cortisol_measured", "immunesystem_tests", "subjectivestress_measured") ~ "Stress markers",
      test_grouped %in% c("Subjective stress") ~ "Stress markers",
      # Omics
      test_grouped %in% c("genomics", "epigenomics","transcriptomics", "metabolomics") ~ "Omics",
      # general info
      outcome == "questionnaires_tests" ~ "General info",
      test_grouped == "mood_and_personality" ~ "General info",
      # cognition
      outcome %in% c("cognitive_task") ~ "Cognition Behavior",
      #physiology
      test_grouped %in% c("Skin conductance/temperature") ~ "physiology",
      T ~ str_remove(outcome,"_tests|_measured|_task|_scan_type"))
  )

# RECODE NAMING
meta_long_tests_grouped_recoded %>% 
  mutate( 
    outcome = outcome %>%
           str_replace_all("_", " ") %>%
           str_to_sentence(),
         test_grouped = test_grouped %>%
           str_replace_all("_", " ") %>%
           str_to_sentence()) %>%
  mutate(test_grouped = case_when(
    test_grouped == c("Mri") ~ "MRI",
    test_grouped == c("Eeg") ~ "EEG",
    test_grouped == "Fmri" ~ "fMRI",
    test_grouped == "Iq related" ~ "IQ related",
    T ~ test_grouped
  )) ->meta_long_tests_grouped_recoded2
meta_long_tests_grouped_recoded2$test_grouped %>% unique()

meta_long_tests_grouped_recoded2 %>%
  #reorder tests
  mutate(test_grouped = factor(test_grouped, levels = c(
    # stress markers
    "Cortisol", "Alpha amylase", "Subjective stress", "Heart rate", "Blood pressure", 
    # Physiology (other)
    "Immune system", "Breathing", "Eyeblink", "Skin conductance/temperature",
    # questionnaires
    "Childhood adversity", "Perceived stress", "Life events", "Health status",
    "Diagnosis", "Severity disease", "Mood and personality", 
    # genetics
    "Genomics", "Epigenomics", "Transcriptomics", "Metabolomics",
    # cog behavior
    "Learning and/or memory", "Reward or decision making", "Neuro psych",
    "IQ related", "Emotion related", "Attention", "Social", 
    # brain activity
    "MRI", "fMRI", "EEG",
    "Other"
  ))) ->meta_long_tests_grouped_recoded3 
# meta_long_tests_grouped_recoded3$outcome %>% unique()
# meta_long_tests_grouped_recoded3$test_grouped %>% unique()

# outcomes 
meta_long_tests_grouped_recoded3 %>% 
  select(-test_number, -test) %>%
    unique() %>%
  mutate(outcome = factor(outcome, levels = c("General info", "Omics",
                                              "Stress markers", "Physiology", "Brain activity",
                                              "Cognition behavior" ))) %>%
  ggplot(aes(fct_rev(test_grouped), n, fill = doi)) + 
  geom_histogram(stat = "identity", colour = "black", fill = "white") + 
  coord_flip() +
  facet_grid(outcome~., scales = "free", space = "free") +
  theme_bw() + 
  labs(x = "", y = "Number of participants") + 
  theme(legend.position = "none", 
        text = element_text(size = 16),
        axis.text.x = element_text(angle=45, hjust=1)
  )
ggsave(paste0( "results/figure3_outcomes.jpeg"), width =7, height = 11)