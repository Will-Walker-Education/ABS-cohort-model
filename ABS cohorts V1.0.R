#ABS cohort model V1.0
#Will Walker 
#12/01/2024
#QA by Raff Sasso
#xx/01/2024

#This script computes the ABS cohort and workforce model, implementing the methodology set out in the accompanying slides "ABS cohort model specification and walk through.pptx"

#section 2 reads in our YPMAD data table, which contains a row for each learner that began KS5 at age 16 in 201819. 
  #this section also reads in  GIAS extract that lists school characteristics.  
  #some of the institution IDs in YPMAD are UPINs which are not directly compatible with the URN and LAESTABs in GIAS, section 2 also reads in a mapping.

#section 3 examines the YPMAD data and extract data used tp form assumptions based on learners chosen pathways and subject routes in the existing K25 system, 
  #subject to their prior attainment in English and Maths. these results were manually copied from their output CSVs into the master assumptions xlsx, 
  #from which they have been manually adjusted to populate the assumptions read in in section 4

#section 4.1 reads in the models assumptions from the master assumptions workbook "ABS_pathway_scenarios.xlsx"
  #sector 4.2 translates the raw assumption matrixes into a sets of thresholds against which each learners dice rolls are evaluated in the micro simulation

#section 5 prepares the YPMAD cohort data for use in the cohort model in section 6 
  #this section uses UPIN and LAESTABS to match every learners to provider to the correct (as of 2016-18) entry in GIAS. 

#section 6 runs the ABS cohort model, this works by rolling dice for each random choice made by learners in the cohort, 
  #then attaching on the correct distribution of choices given each learners prior attainment in English and Maths  

#section 7 performs basic exploratory analysis of the ABS cohort models output - these outputs are present in the working slide pack "ABS cohorts, providers and workforce analytical map.pptx"

#section 8 manually (for now) attaches  GLH to the simulated cohorts learners ABS pathways and subject choices, 
  #as well as for each learners pathway in the legacy system (what they actually did), then aggregates to compare the relative workforce need. 

#section 9.1 sensitivity tests changes in the base cohorts attainment this runs the cohort model with 
  #GCSE pass rates of 80 and 90%. Then takes a sample of the outputs cohorts (the full data set would too large to render a ggplot in a tolerable amount of time)
  #to create a plot of the attainment distribution and learners on each route
  #section 9.2 returns with model though assumes attainment improvements are restricted only to those who do not currently pass,  
  #whereas 9.1 assumes the improvements occur across the distribution this section creates an alternative (but equivalent) cohort model function which has the attainment section altered

#section 10 examines the distribution of provider types attended by learners on each simulated ABS track. 
#This outputs forms the assumption for future runs of the model about the types of provider learners would whs to attend. 

# 0. Packages ----------------------------------------------------------------

library(odbc)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(data.table)
library(stargazer)
library(openxlsx)

# 1. SQL connection  --------------------------------------------------------------

PPU_con <- dbConnect(odbc(), Driver = "SQL Server", 
                     Server = "T1PRMDRSQL\\SQLPROD,55842", 
                     Database = "MDR_Modelling_PPU",
                     Trusted_Connection = "yes")

# 2.1 read in data ------------------------------------------------------------

#read in YPMAD data
#run time ~ 75 seconds
start <- Sys.time()
Raw_cohort_data_16_17_18 <-dbGetQuery(con = PPU_con,
                                      
                                      "Select * 
                      from empivot_2001_16_17_18_pupil"
                                      
)

end <- Sys.time()
end - start

cohort_data_16_17_18 <- Raw_cohort_data_16_17_18 %>% 
  filter(eng_gcse_15 %in% c(as.character(1:9), "H.Below G")) %>% 
  filter(maths_gcse_15 %in% c(as.character(1:9), "H.Below G"))

#1443 learner do not have GCSE attainment listed as 1:9 or "H.Below G"
paste0((Raw_cohort_data_16_17_18 %>%  nrow()) - cohort_data_16_17_18 %>%  nrow(), " rows dropped")

#read in GIAS
GIAS_all <- read.csv("GIAS_all.csv")

#modify dates and LAESTAB ID variables
GIAS_extract <- GIAS_all %>% 
  mutate(LAESTAB = paste0(LA..code.,EstablishmentNumber) %>%  as.numeric(),
         LAESTAB =  case_when(is.na(LAESTAB) ~ 0,T ~ LAESTAB),
         CloseDate = as.Date(CloseDate, format = "%d/%m/%Y"),
         OpenDate = as.Date(OpenDate, format = "%d/%m/%Y")) %>% 
  select(URN,UKPRN,LAESTAB,LA..code.,DistrictAdministrative..code.,LA..name.,EstablishmentName, GOR..name.,Easting,Northing,CloseDate,OpenDate) 

#read in UKPIN mapping to allow for connection betwen GIAS data and YPMAD institution variables. 
UKPRN_UPIN <- read.csv("YPMAD - 20220725 - UKPRN UPIN.csv")

# 3.1 generate assumption data --------------------------------------------------------

#here we estimate progression from L2 attainment to Eng/maths A level
#this forms the basis of our baseline assumptions
#the csvs (commented out) are copied in to the master assumptions workbook  "ABS_pathway_scenarios.xlsx".
#from where they are modified and translated into assumption about learners behavior under ABS
#The QAer can cross check the figures in the tables made in console against the figures in the master assumptions workbook

#First we look at progression from English and maths at GCSE to A level

# English progression from GCSE attainment to KS5 in 16/17 YPMAD cohort
English_cohort_pivot <- Raw_cohort_data_16_17_18  %>%
  group_by(eng_gcse_15, eng_part_lit_16) %>%
  summarise(count = sum(cnt)) %>%
  group_by(eng_gcse_15) %>%
  mutate(prop = (count/sum(count)) %>%  percent()) %>%
  select(-count) %>%
  pivot_wider(names_from = "eng_part_lit_16", values_from = "prop") %>%
  mutate_all(funs(ifelse(is.na(.),"0%",.)))
# 
# write.csv(English_cohort_pivot,"ENG_continuation_2.csv")

# Maths progression from GCSE attainment to KS5 in 16/17 YPMAD cohort
Maths_cohort_pivot <- Raw_cohort_data_16_17_18 %>%
  group_by(maths_gcse_15, maths_part_16) %>%
  summarise(count = sum(cnt)) %>%
  group_by(maths_gcse_15) %>%
  mutate(prop = (count/sum(count)) %>%  percent()) %>%
  select(-count) %>%
  pivot_wider(names_from = "maths_part_16", values_from = "prop") %>%
  mutate_all(funs(ifelse(is.na(.),"0%",.)))

# write.csv(Maths_cohort_pivot,"MATH_continuation_2.csv")

#here we look at routes according to prior attainment (overall and maths and eng)
#this forms the basis of our baseline assumptions

# YPMAD KS5 grouped qualification route by overall attainment category
KS5_routes <- cohort_data_16_17_18  %>%
  group_by(overallattainment_15, grouped_qual_16) %>%
  summarise(count = n()) %>%
  group_by(overallattainment_15) %>%
  mutate(prop = (count/sum(count)) %>%  percent()) %>%
  select(-count) %>%
  pivot_wider(names_from = "grouped_qual_16", values_from = "prop") %>%
  mutate_all(funs(ifelse(is.na(.),"0%",.)))

# write.csv(KS5_routes,"KS5_routes.csv")

# YPMAD KS5 grouped qualification route by English attainment at GCSE
KS5_routes_eng <- cohort_data_16_17_18  %>%
  group_by(eng_gcse_15, grouped_qual_16) %>%
  summarise(count = n()) %>%
  group_by(eng_gcse_15) %>%
  mutate(prop = (count/sum(count)) %>%  percent()) %>%
  select(-count) %>%
  pivot_wider(names_from = "grouped_qual_16", values_from = "prop") %>%
  mutate_all(funs(ifelse(is.na(.),"0%",.)))

# write.csv(KS5_routes_eng,"KS5_routes_eng.csv")

# YPMAD KS5 grouped qualification route by Maths attainment at GCSE
KS5_routes_maths <- cohort_data_16_17_18  %>%
  group_by(maths_gcse_15, grouped_qual_16) %>%
  summarise(count = n()) %>%
  group_by(maths_gcse_15) %>%
  mutate(prop = (count/sum(count)) %>%  percent()) %>%
  select(-count) %>%
  pivot_wider(names_from = "grouped_qual_16", values_from = "prop") %>%
  mutate_all(funs(ifelse(is.na(.),"0%",.)))

# write.csv(KS5_routes_maths,"KS5_routes_maths.csv")

# YPMAD KS5 grouped qualification route by Maths and English attainment at GCSE
KS5_routes_maths_eng <- cohort_data_16_17_18  %>%
  group_by(maths_gcse_15, grouped_qual_16,eng_gcse_15) %>%
  summarise(count = n()) %>%
  mutate(eng_maths_concat = paste0("Eng_",eng_gcse_15,"_Maths_",maths_gcse_15)) %>%
  group_by(eng_maths_concat) %>%
  mutate(prop = (count/sum(count)) %>%  percent()) %>%
  select(-count) %>%
  pivot_wider(names_from = "grouped_qual_16", values_from = "prop") %>%
  mutate_all(funs(ifelse(is.na(.),"0%",.)))

# write.csv(KS5_routes_maths_eng,"KS5_routes_maths_eng.csv")

# YPMAD KS5 grouped qualification route by combined Maths and English score at GCSE
KS5_routes_maths_plus_eng <- cohort_data_16_17_18  %>%
  mutate_at(vars(maths_gcse_15,eng_gcse_15),funs(case_when(. == "H.Below G" ~ 0,T ~ as.numeric(.)))) %>%
  mutate(math_plus_eng = maths_gcse_15 + eng_gcse_15) %>%
  group_by(math_plus_eng, grouped_qual_16) %>%
  summarise(count = n()) %>%
  group_by(math_plus_eng) %>%
  mutate(prop = (count/sum(count)) %>%  percent()) %>%
  select(-count) %>%
  pivot_wider(names_from = "grouped_qual_16", values_from = "prop") %>%
  mutate_all(funs(ifelse(is.na(.),"0%",.)))

# write.csv(KS5_routes_maths_plus_eng,"KS5_routes_maths_plus_eng.csv")

# 4.1 read in assumptions -------------------------------------------------

#assumption about chosen ABS maths subject routes given maths GCSE attainment
maths_behaviour_data <- read.xlsx(xlsxFile = "ABS_pathway_scenarios_2.xlsx",
                                  sheet = "3.2 Maths_behaviour_2")

#assumption about chosen ABS English subject routes given english GCSE attainment
eng_behaviour_data <- read.xlsx(xlsxFile = "ABS_pathway_scenarios_2.xlsx",
                                sheet = "3.1 Eng_behaviour_2")

#assumption about chosen ABS pathway given maths prior attainment at GCSE
ABS_pathway_maths_data <- read.xlsx(xlsxFile = "ABS_pathway_scenarios_2.xlsx",
                                    sheet = "3.3 ABS_pathway_chosen_maths",
                                    startRow = 5) %>% 
  rename("X3_majors_3_minors" = `3_majors_3_minors`,
         "X3_majors_2_minors" = `3_majors_2_minors`)

#assumption about chosen ABS pathway given English prior attainment at GCSE
ABS_pathway_eng_data <- read.xlsx(xlsxFile = "ABS_pathway_scenarios_2.xlsx",
                                  sheet = "3.4 ABS_pathway_chosen_english",
                                  startRow = 5) %>% 
  rename("X3_majors_3_minors" = `3_majors_3_minors`,
         "X3_majors_2_minors" = `3_majors_2_minors`)

#assumption about preferred institution type given chose ABS pathway 
#this assumption is generated based off of a preliminary run of the model, 
#these distribution off of which the assumption was generated is generated in section 10.
ABS_institution_draw <- read.xlsx(xlsxFile = "ABS_pathway_scenarios_2.xlsx",
                                  sheet = "5.1 Institution_type_assumption")

#GLH are not current read in from the matder assumptions workbook
# ABS_pathway_GLH <- read.xlsx(xlsxFile = "ABS_pathway_scenarios_2.xlsx",
#                              sheet = "4.1 ABS_path_GLH_assumptions")
# 
# Baseline_pathway_GLH <- read.xlsx(xlsxFile = "ABS_pathway_scenarios_2.xlsx",
#                                   sheet = "5.2 Baseline_pathway_GLH_asssup")
# 
# ABS_subject_GLH <- read.xlsx(xlsxFile = "ABS_pathway_scenarios_2.xlsx",
#                              sheet = "4.2 subject_path_GLH_assumption")
# 
# Baseline_subject_GLH <- read.xlsx(xlsxFile = "ABS_pathway_scenarios_2.xlsx",
#                                   sheet = "5.1 Baseline_GLH_assumptions")

# 4.2 transforming learner behavior assumptions -------------------------------------------

#manipulate learner behavior assumptions so that the can compute the outcome of dice rolls which determine their ABS pathways

maths_behaviour <- maths_behaviour_data  %>% 
  mutate(Maths_major_threshold = Major,
         Maths_minor_threshold = Major + Minor,
         Maths_L2.5_threshold = Major + Minor + L2.5,
         Maths_resit_threshold = Major + Minor + L2.5 + GCSE_retake,
         Maths_for_life_threshold = Major + Minor + L2.5 + GCSE_retake + Subject_for_life) %>% 
  select(-c(Major:Subject_for_life))

eng_behaviour <- eng_behaviour_data %>% 
  mutate(Eng_major_threshold = Major,
         Eng_minor_threshold = Major + Minor,
         Eng_L2.5_threshold = Major + Minor + L2.5,
         Eng_resit_threshold = Major + Minor + L2.5 + GCSE_retake,
         Eng_for_life_threshold = Major + Minor + L2.5 + GCSE_retake + Subject_for_life) %>% 
  select(-c(Major:Subject_for_life))

#ABS pathway will take both eng and maths into consideration

#ABS pathway assumptions given maths attainment
ABS_pathway_maths <- ABS_pathway_maths_data  %>% 
  mutate(Maths_3_majors_3_minors_threshold = X3_majors_3_minors,
         Maths_3_majors_2_minors_threshold = X3_majors_3_minors +X3_majors_2_minors,
         Maths_Vocational_double_major_threshold = X3_majors_3_minors +X3_majors_2_minors + Vocational_double_major,
         Maths_Apprenticeship_threshold = X3_majors_3_minors +X3_majors_2_minors + Vocational_double_major + Apprenticeship,
         Maths_Level_2_ABS_threshold = X3_majors_3_minors +X3_majors_2_minors + Vocational_double_major + Apprenticeship + Level_2_ABS,
         Maths_Below_L2_threshold = X3_majors_3_minors +X3_majors_2_minors + Vocational_double_major + Apprenticeship + Level_2_ABS + Below_L2) %>% 
  select(-c(X3_majors_3_minors:Below_L2)) %>% 
  rename("Pathway_Scenario" = Scenario)

#ABS pathway assumptions given english attainment
ABS_pathway_eng <- ABS_pathway_eng_data %>% 
  mutate(Eng_3_majors_3_minors_threshold = X3_majors_3_minors,
         Eng_3_majors_2_minors_threshold = X3_majors_3_minors +X3_majors_2_minors,
         Eng_Vocational_double_major_threshold = X3_majors_3_minors +X3_majors_2_minors + Vocational_double_major,
         Eng_Apprenticeship_threshold = X3_majors_3_minors +X3_majors_2_minors + Vocational_double_major + Apprenticeship,
         Eng_Level_2_ABS_threshold = X3_majors_3_minors +X3_majors_2_minors + Vocational_double_major + Apprenticeship + Level_2_ABS,
         Eng_Below_L2_threshold = X3_majors_3_minors +X3_majors_2_minors + Vocational_double_major + Apprenticeship + Level_2_ABS + Below_L2) %>% 
  select(-c(X3_majors_3_minors:Below_L2)) %>% 
  rename("Pathway_Scenario" = Scenario)

#type of institution studied at
ABS_institution_distribution <- ABS_institution_draw %>% 
  mutate(School_threshold = School,
         Tech.collage_threshold = School + Tech.collage,
         Alternative.provision_threshold = School + Tech.collage + Alternative.provision,
         None_threshold = School + Tech.collage + Alternative.provision + None,
         Other_threshold = School + Tech.collage + Alternative.provision + None + Other) %>% 
  select(-c(School:Other))

# 5.1 simulated cohort baseline data -------------------------------------------------------------

#map institution codes to GIAS via UPINS - not all institutions are matched this way, some are mached below on LAESTABs
simulation_pupils_and_grades_URN_matches <- cohort_data_16_17_18 %>%
  select(l23c_base_year,grouped_qual_16,eng_gcse_15,maths_gcse_15,eng_part_lit_16,maths_part_16,main_inst_16,inst_16,l23s_gender,idaci_quartile,ethnic,l23s_senstatusage15) %>% 
  left_join(UKPRN_UPIN %>%  mutate(MASTERUPIN = as.numeric(MASTERUPIN)), by = c("main_inst_16" ="MASTERUPIN")) %>% 
  left_join(GIAS_extract %>%  filter(!is.na(UKPRN)), by = c("MASTERUKPRN" = "UKPRN")) %>% 
  select(-LAESTAB) %>%  
  rename("UKPRN" = MASTERUKPRN)

#map remaining institutions directly to GIAS on LAESTABS
simulation_pupils_and_grades_LAESTAB_matches <- simulation_pupils_and_grades_URN_matches %>% 
  filter(is.na(EstablishmentName)) %>% 
  select(l23c_base_year,grouped_qual_16,eng_gcse_15,maths_gcse_15,eng_part_lit_16,maths_part_16,main_inst_16,inst_16,l23s_gender,idaci_quartile,ethnic,l23s_senstatusage15) %>% 
  mutate(row_id = row_number()) %>% 
  left_join(GIAS_extract, by = c("main_inst_16" = "LAESTAB"))%>% 
  group_by(row_id) %>% 
  mutate(count = n(),
         row_number = row_number()) %>% 
  ungroup() %>% 
  filter(is.na(OpenDate ) | OpenDate < as.Date("2018-09-01")) %>% 
  group_by(row_id) %>% 
  filter(!(count > 1 & row_number < max(row_number))) %>% 
  ungroup() %>% 
  select(-c(count,row_number,row_id))

#merge learners matched on LAESTABs and UPINs
simulation_pupils_and_grades <- simulation_pupils_and_grades_URN_matches %>% 
  filter(!is.na(EstablishmentName)) %>% 
  rbind(simulation_pupils_and_grades_LAESTAB_matches)

#check number of learners who could not be matched - this is satisfyingly low. 
paste0(nrow(cohort_data_16_17_18) - nrow(simulation_pupils_and_grades), " rows dropped due to lack of provider match")
paste0(nrow(Raw_cohort_data_16_17_18) - nrow(simulation_pupils_and_grades), " rows dropped from the raw dataset")
paste0(((nrow(simulation_pupils_and_grades)/nrow(Raw_cohort_data_16_17_18)) %>%  round(digits = 4))*100,"% of learners are included in the cohort model outputs")

# 6.1 ABS cohort simulation model ---------------------------------------------

run_cohort_simulation <- function(maths_attainment_adjustment = 0,
                                  english_attainment_adjustment = 0,
                                  demographic_muiltiplyer = 1.15) {

#roll dice for placement on attainment distribution, and Eng and Maths subject routes, ABS pathway and institution type demanded. 
simulation_maths_UMS_add_on <- runif(min = 0,max = 10, n = nrow(simulation_pupils_and_grades))
simulation_Eng_UMS_add_on  <- runif(min = 0,max = 10, n = nrow(simulation_pupils_and_grades))
simulation_maths_choice <- runif(min = 0,max = 1, n = nrow(simulation_pupils_and_grades))
simulation_Eng_choice <- runif(min = 0,max = 1, n = nrow(simulation_pupils_and_grades))
ABS_pathway_choice <- runif(min = 0,max = 1, n = nrow(simulation_pupils_and_grades))
Institution_choice <- runif(min = 0,max = 1, n = nrow(simulation_pupils_and_grades))

#randomly generate attainment within GCSE grade
simulation_pupils_and_base_UMS <- simulation_pupils_and_grades %>% 
  cbind(simulation_maths_UMS_add_on,
        simulation_Eng_UMS_add_on) %>% 
  mutate_at(vars(eng_gcse_15,maths_gcse_15), 
            funs(case_when(. == "9" ~ 90,
                           . == "8" ~ 80,
                           . == "7" ~ 70,
                           . == "6" ~ 60,
                           . == "5" ~ 50,
                           . == "4" ~ 40,
                           . == "3" ~ 30,
                           . == "2" ~ 20,
                           . == "1" ~ 10,
                           T ~ 0))) %>% 
  mutate(English_UMS = (eng_gcse_15+ simulation_Eng_UMS_add_on) %>%  round(digits = 1),
         Maths_UMS = (maths_gcse_15 + simulation_maths_UMS_add_on) %>%  round(digits = 1)) %>% 
  select(-c(eng_gcse_15,maths_gcse_15, simulation_maths_UMS_add_on,simulation_Eng_UMS_add_on)) 

#assign English and maths choice, ABS pathway, and desired institution type based on each learerns adjusted attainment 
simulated_choices <- simulation_pupils_and_base_UMS %>% 
  #adjust attainmnet 
  mutate(English_UMS_adjusted = English_UMS + english_attainment_adjustment,
         Maths_UMS_adjusted = Maths_UMS + maths_attainment_adjustment) %>% 
  #adjust grades based on adjusted UMS
  mutate_at(vars(English_UMS_adjusted,Maths_UMS_adjusted), funs(case_when(. > 100 ~ 100,
                                                                          . < 0 ~ 0,
                                                                          T ~ .))) %>% 
  mutate(English_adjusted_grade = case_when(English_UMS_adjusted  > 90 ~ "9",
                                            English_UMS_adjusted  > 80 ~ "8",
                                            English_UMS_adjusted  > 70 ~ "7",
                                            English_UMS_adjusted  > 60 ~ "6",
                                            English_UMS_adjusted  > 50 ~ "5",
                                            English_UMS_adjusted  > 40 ~ "4",
                                            English_UMS_adjusted  > 30 ~ "3",
                                            English_UMS_adjusted  > 20 ~ "2",
                                            English_UMS_adjusted  > 10 ~ "1",
                                            T ~ "H.Below G"),
         Maths_adjusted_grade = case_when(Maths_UMS_adjusted  > 90 ~ "9",
                                          Maths_UMS_adjusted  > 80 ~ "8",
                                          Maths_UMS_adjusted  > 70 ~ "7",
                                          Maths_UMS_adjusted  > 60 ~ "6",
                                          Maths_UMS_adjusted  > 50 ~ "5",
                                          Maths_UMS_adjusted  > 40 ~ "4",
                                          Maths_UMS_adjusted  > 30 ~ "3",
                                          Maths_UMS_adjusted  > 20 ~ "2",
                                          Maths_UMS_adjusted  > 10 ~ "1",
                                          T ~ "H.Below G")) %>% 
  #attach dice rolls
  cbind(simulation_maths_choice,
        simulation_Eng_choice,
        ABS_pathway_choice,
        Institution_choice) %>% 
  #join on choice distributions that correspond to the learners prior attainment. 
  left_join(eng_behaviour, by = c("English_adjusted_grade" = "eng_gcse_15")) %>% 
  left_join(maths_behaviour, by = c("Maths_adjusted_grade" = "Math_gcse_15", "Scenario")) %>% 
  left_join(ABS_pathway_eng, by = c("English_adjusted_grade" = "Attainment.(english.L2)")) %>% 
  left_join(ABS_pathway_maths, by = c("Maths_adjusted_grade" = "Attainment.(maths.L2)","Pathway_Scenario")) %>% 
  #compute combined engl and maths threshold for ABS pathway choices
  mutate(Combined_3_majors_3_minors_threshold = (Eng_3_majors_3_minors_threshold + Maths_3_majors_3_minors_threshold)/2,
         Combined_3_majors_2_minors_threshold = (Eng_3_majors_2_minors_threshold + Maths_3_majors_2_minors_threshold)/2,
         Combined_Vocational_double_major_threshold = (Eng_Vocational_double_major_threshold + Maths_Vocational_double_major_threshold)/2,
         Combined_Apprenticeship_threshold = (Eng_Apprenticeship_threshold + Maths_Apprenticeship_threshold)/2,
         Combined_Level_2_ABS_threshold = (Eng_Level_2_ABS_threshold + Maths_Level_2_ABS_threshold)/2,
         Combined_Below_L2_threshold = (Eng_Below_L2_threshold + Maths_Below_L2_threshold)/2) %>% 
  #evaluate learners english ABS subject route
  mutate(Eng_chosen_track = case_when(simulation_Eng_choice < Eng_major_threshold ~ "Major",
                                      simulation_Eng_choice < Eng_minor_threshold ~ "Minor",
                                      simulation_Eng_choice < Eng_L2.5_threshold ~ "L2.5",
                                      simulation_Eng_choice < Eng_resit_threshold ~ "GCSE_retake",
                                      T ~ "Subject_for_life")) %>% 
  #evaluate learners maths ABS subject route
  mutate(Maths_chosen_track = case_when(simulation_maths_choice < Maths_major_threshold ~ "Major",
                                        simulation_maths_choice < Maths_minor_threshold ~ "Minor",
                                        simulation_maths_choice < Maths_L2.5_threshold ~ "L2.5",
                                        simulation_maths_choice < Maths_resit_threshold ~ "GCSE_retake",
                                        T ~ "Subject_for_life")) %>% 
  #evaluate learners ABS pathway based on the combined English and maths thresholds
  mutate(ABS_chosen_pathway = case_when(ABS_pathway_choice < Combined_3_majors_3_minors_threshold ~ "3_majors_3_minors",
                                        ABS_pathway_choice < Combined_3_majors_2_minors_threshold ~ "3_majors_2_minors",
                                        ABS_pathway_choice < Combined_Vocational_double_major_threshold ~ "Vocational_double_major",
                                        ABS_pathway_choice < Combined_Apprenticeship_threshold ~ "Apprenticeship",
                                        ABS_pathway_choice < Combined_Level_2_ABS_threshold ~ "Level_2_ABS",
                                        T ~ "Below_L2")) %>%
  #based on each learners ABS pathway choice, join on the distribution of attended institutions
  left_join(ABS_institution_distribution, by = c("ABS_chosen_pathway" = "ABS_pathway")) %>% 
  mutate(ABS_institution = case_when(Institution_choice < School_threshold ~ "School/SFC",
                                     Institution_choice < Tech.collage_threshold ~ "FE collage",
                                     Institution_choice < Alternative.provision_threshold  ~ "AP",
                                     Institution_choice < None_threshold ~ "None",
                                     T ~ "Other")) %>% 
  #aaply demographic muitliplyer (not used yet in outputs)
  mutate(demographic_FTE = demographic_muiltiplyer)

simulated_choices 

}

simulated_choices <- run_cohort_simulation()

#reduce down the number of columns for furthr analysis 
simulated_choices_output_table <- simulated_choices %>% 
  select(l23c_base_year:OpenDate, URN,English_adjusted_grade,Maths_adjusted_grade,Scenario, Eng_chosen_track:ABS_chosen_pathway,demographic_FTE)

#write.csv(simulated_choices_output_table,"simulated_choices_output_table_5.csv")
#read in manually if user does not have requisite data access. 
#simulated_choices_output_table <- read.csv("simulated_choices_output_table.csv")

# 7.1 explore simulation outputs ----------------------------------------------

#summarise maths routes proportions in ABS simulation
maths_numbers <- simulated_choices_output_table %>% 
  filter(Scenario == "S1_16_18_continuation") %>% 
  group_by(Maths_chosen_track) %>%  
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count),
         subject = "Maths") %>% 
  rename("route" = 1)

#summarise english routes proportions in ABS simulation
english_numbers <-simulated_choices_output_table %>% 
  filter(Scenario == "S1_16_18_continuation") %>% 
  group_by(Eng_chosen_track) %>%  
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count), subject = "English") %>% 
  rename("route" = 1)

#merge maths and english summaries for visualisation
subject_routes <- rbind(maths_numbers,english_numbers) %>% 
  mutate(route = factor(route, levels = c("Subject_for_life","GCSE_retake","L2.5","Minor","Major"))) %>% 
  arrange(subject,desc(route)) %>% 
  group_by(subject) %>% 
  mutate(cumprop = cumsum(prop ),
         position = case_when(row_number() == 1 ~ cumprop/2,
                              T ~ cumprop  - (cumprop - lag(cumprop))/2)) 

#generate plot
ggplot(subject_routes,aes(x = subject, y = prop, fill = route, label = paste0(route,"\n",(prop*100) %>%  str_sub(1,4),"%"))) + 
  geom_bar(stat = "identity", position = "stack") + 
  geom_label(aes(y = position)) +
  coord_flip() + 
  theme_bw() + 
  scale_y_continuous(labels = percent) + 
  labs(title = "Proportion of cohort on each compulsory Maths/English route",
       fill = "ABS route",
       x = "Subject",
       y = "Cumulative % of cohort")

#create sumamry of chosen ABS oathways
ABS_pathway_plot_data <- simulated_choices_output_table %>% 
  filter(Scenario == "S1_16_18_continuation") %>% 
  group_by(ABS_chosen_pathway) %>%  
  summarise(count = n()) %>% 
  arrange(desc(ABS_chosen_pathway)) %>% 
  mutate(prop = count/sum(count)) %>% 
  mutate(cumprop = cumsum(prop ),
         position = case_when(row_number() == 1 ~ cumprop/2,
                              T ~ cumprop  - (cumprop - lag(cumprop))/2)) 

#generate plot
ggplot(ABS_pathway_plot_data,aes(x = "ABS pathways", y = prop, fill = ABS_chosen_pathway , 
                                 label = paste0(ABS_chosen_pathway ,"\n",(prop*100) %>%  str_sub(1,4),"%"))) + 
  geom_bar(stat = "identity", position = "stack") + 
  geom_text(aes(y = position,angle = 90)) +
  coord_flip() + 
  theme_bw() + 
  scale_y_continuous(labels = percent) + 
  labs(title = "Proportion of cohort on each ABS pathway",
       fill = "ABS pathway",
       x = "",
       y = "Cumulative % of cohort")

#summarise chosen ABS pathways by region
route_number_by_region <- simulated_choices_output_table %>% 
  filter(Scenario == "S1_16_18_continuation") %>% 
  group_by(ABS_chosen_pathway,GOR..name.) %>%  
  summarise(count = n()) %>% 
  arrange(GOR..name.,desc(ABS_chosen_pathway)) %>% 
  group_by(GOR..name.) %>% 
  mutate(prop = count/sum(count)) %>%  
  filter(!(GOR..name. %in% c(NA,"Not Applicable")))

#generate plot
ggplot(route_number_by_region ,aes(x = GOR..name., y = prop, fill = ABS_chosen_pathway)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + 
  theme_bw() + 
  scale_y_continuous(labels = percent) + 
  labs(title = "Proportion of cohort on each ABS pathway by Region",
       fill = "ABS pathway",
       x = "Region",
       y = "Cumulative % of cohort")

#summairse ABS route by gender
route_number_by_gender <- simulated_choices_output_table %>% 
  filter(Scenario == "S1_16_18_continuation") %>% 
  group_by(ABS_chosen_pathway,l23s_gender) %>%  
  summarise(count = n()) %>% 
  arrange(l23s_gender,desc(ABS_chosen_pathway)) %>% 
  group_by(l23s_gender) %>% 
  mutate(prop = count/sum(count)) %>% 
  filter(l23s_gender != 9)

#generate plot
ggplot(route_number_by_gender ,aes(x = l23s_gender, y = prop, fill = ABS_chosen_pathway)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + 
  theme_bw() + 
  scale_y_continuous(labels = percent) + 
  labs(title = "Proportion of cohort on each ABS pathway by gender",
       fill = "ABS pathway",
       x = "Gender",
       y = "Cumulative % of cohort")

# 8.2 relative workforce implications ------------------------------------------

#manually apply basic guided learning hour assumptions for ABS and legacy pathways, as well as English and maths routes
#the values are listed in the methodology slide pack "ABS cohort model specification and walk through.pptx"
simulated_choices_output_table_relative_workforce <- simulated_choices_output_table %>% 
  #manually enter pathway GLH
  mutate(legacy_teaching_units = case_when(grouped_qual_16 == "B.ALevelASAVCE" & ABS_chosen_pathway == "3_majors_3_minors" ~ 4,
                                           grouped_qual_16 == "B.ALevelASAVCE" ~ 3,
                                           grouped_qual_16 == "D.Voc_L3" ~ 3,
                                           grouped_qual_16 %in% c("F.L2_Apprenticeship", "C.Adv Apprenticeship") ~ 0,
                                           T ~ 3),
         ABS_teaching_units = case_when(ABS_chosen_pathway == "3_majors_3_minors" ~ (3*1 + 0.5*3)*0.9,
                                        ABS_chosen_pathway == "3_majors_2_minors" ~ (3*1 + 0.5*2)*0.9,
                                        ABS_chosen_pathway == "Vocational_double_major" ~ (2*1 + 1*1 + 0.5*2)*0.9,
                                        ABS_chosen_pathway == "Apprenticeship" ~ 0,
                                        T ~ (3*1 + 0.5*2)*0.9)) %>% 
  #manually enter subject route GLH
  mutate(additional_teaching_units = ABS_teaching_units - legacy_teaching_units) %>% 
  mutate(Legacy_maths_TU = case_when(maths_part_16 == "A.Level 3" ~ 1,
                                     maths_part_16 == "I.No Maths Studied" ~ 0,
                                     T ~ 0.5),
         Legacy_eng_TU = case_when(eng_part_lit_16== "A.Level 3" ~ 1,
                                   eng_part_lit_16 == "I.No English Studied" ~ 0,
                                   T ~ 0.5),
         ABS_maths_TU = case_when(Maths_chosen_track == "Major" ~ 0.9,
                                T~ 0.45),
         ABS_eng_TU = case_when(Eng_chosen_track == "Major" ~ 0.9,
                                T~ 0.45))

#aggregate the GLH attached to each learners ABY pathway and subject choices to 
#compare the relative workforce implications of ABS to the legacy KS5 system
extra_teaching_calc <- simulated_choices_output_table_relative_workforce %>%  
  #remove all learners on WBL or apprenticeship routes - these are out of scope of the ABS.
  #since we have removed the persons on these routes from the legacy and conterfactual scenarios, 
  #and they have the same proportions in each, the effects of their removal should cancel out
  filter(Scenario == "S1_16_18_continuation",
         !(inst_16 %in% c("WBL","None")),
         !(grouped_qual_16 %in% c("F.L2_Apprenticeship","C.Adv Apprenticeship")),
         !(ABS_chosen_pathway %in% c("Apprenticeship")),
         #inst_16 %in% c("FE Coll", "Converter Academy","SFC","Maintained School")
         ) %>% 
  #group_by(inst_16) %>%  
  #aggregate teaching units
  summarise(legacy_TU = sum(legacy_teaching_units),
            ABS_TU = sum(ABS_teaching_units),
            Legacy_maths_TU = sum(Legacy_maths_TU),
            Legacy_eng_TU = sum(Legacy_eng_TU),
            ABS_maths_TU = sum(ABS_maths_TU),
            ABS_eng_TU = sum(ABS_eng_TU))%>% 
  ungroup() %>% 
  #work out various stats to describe the workforce impact
  mutate(additional_teaching = ABS_TU - legacy_TU,
         additional_maths_TU = ABS_maths_TU - Legacy_maths_TU,
         additional_eng_TU = ABS_eng_TU- Legacy_eng_TU,
         additional_non_eng_maths = additional_teaching -additional_maths_TU - additional_eng_TU,
         additional_teaching_prop = (ABS_TU/ legacy_TU - 1) %>%  round(digits = 3) %>% percent(),
         additiona_non_eng_maths_prop = (additional_non_eng_maths/legacy_TU) %>% round(digits = 3) %>%  percent(),
         additional_maths_prop = (additional_maths_TU/legacy_TU) %>% round(digits = 3) %>%  percent(),
         additional_eng_prop = (additional_eng_TU/legacy_TU) %>% round(digits = 3) %>%  percent(),
         relative_maths_prop = (ABS_maths_TU/Legacy_maths_TU - 1) %>% round(digits = 3) %>%  percent(),
         relative_eng_prop = (ABS_eng_TU/Legacy_eng_TU - 1) %>% round(digits = 3) %>%  percent())

#remove columns used for calculations for visualization. 
extra_teaching_calc_summary <- extra_teaching_calc  %>% 
  select(1,2,additional_teaching_prop:relative_eng_prop) 

# 9.1 sensitivity of attainment improvements -----------------------------

#baseline attainment 
simulated_choices_68 <- simulated_choices %>% 
  mutate(attainment_scenario = "68%")

ggplot(simulated_choices_68  ,aes(x = Maths_UMS)) + geom_histogram()
 
simulated_choices_68  %>% 
  mutate(GCSE_maths_pass = case_when(Maths_UMS >= 40 ~ 1, T ~ 0)) %>% 
  group_by(GCSE_maths_pass) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "GCSE_maths_pass", values_from = "count") %>% 
  mutate(prop = `1`/(`0` + `1`)) %>% 
  select(prop) %>% 
  pull()

#80% GCSE passrate
simulated_choices_80 <- run_cohort_simulation(maths_attainment_adjustment = 9,
                                              english_attainment_adjustment = 6.5) %>% 
  mutate(attainment_scenario = "80%")

simulated_choices_80 %>% 
  mutate(GCSE_maths_pass = case_when(Maths_UMS_adjusted >= 40 ~ 1, T ~ 0)) %>% 
  group_by(GCSE_maths_pass) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "GCSE_maths_pass", values_from = "count") %>% 
  mutate(prop = `1`/(`0` + `1`)) %>% 
  select(prop) %>% 
  pull()

simulated_choices_80 %>% 
  mutate(GCSE_english_pass = case_when(English_UMS_adjusted >= 40 ~ 1, T ~ 0)) %>% 
  group_by(GCSE_english_pass) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "GCSE_english_pass", values_from = "count") %>% 
  mutate(prop = `1`/(`0` + `1`)) %>% 
  select(prop) %>% 
  pull()

#90% GCSE passrate
simulated_choices_90 <- run_cohort_simulation(maths_attainment_adjustment = 20,
                                              english_attainment_adjustment = 16) %>% 
  mutate(attainment_scenario = "90%")

simulated_choices_90 %>% 
  mutate(GCSE_maths_pass = case_when(Maths_UMS_adjusted >= 40 ~ 1, T ~ 0)) %>% 
  group_by(GCSE_maths_pass) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "GCSE_maths_pass", values_from = "count") %>% 
  mutate(prop = `1`/(`0` + `1`)) %>% 
  select(prop) %>% 
  pull()

simulated_choices_90 %>% 
  mutate(GCSE_english_pass = case_when(English_UMS_adjusted >= 40 ~ 1, T ~ 0)) %>% 
  group_by(GCSE_english_pass) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "GCSE_english_pass", values_from = "count") %>% 
  mutate(prop = `1`/(`0` + `1`)) %>% 
  select(prop) %>% 
  pull()

#merge runs of the model
attainment_sensitivity_set <- rbind(simulated_choices_68,
                                    simulated_choices_80,
                                    simulated_choices_90) 

#dataset is now huge!
nrow(attainment_sensitivity_set)

#take a sample such that the plot will render in a short timeframe
plot_data <- (attainment_sensitivity_set  %>%
  pivot_longer(cols = c(Maths_UMS_adjusted,English_UMS_adjusted), names_to = "Subject", values_to = "Adjusted_UMS") %>%  
    select(Adjusted_UMS,attainment_scenario,Subject))[runif(20000,0,nrow(attainment_sensitivity_set)*2) %>%  round(digit = 0),]

#plot attainment distributions
ggplot(plot_data,
       aes(x = Adjusted_UMS, colour = attainment_scenario)) + 
  stat_ecdf() + 
  facet_wrap(~Subject, ncol = 1) + 
  geom_vline(xintercept = 40,size = 1,linetype = "dashed") + 
  geom_text(label = "Pass threshold",y = 0.5, x = 35, angle = 90,colour= "Black") +
  scale_y_continuous(breaks = 0:5/5, labels = percent) +
  theme_bw() + 
  labs(title = "GCSE maths/english UMS under passrate scenarios",
       colour = "GCSE maths/english\npassrate (%)")

#number on each track in each scenario
attainment_sensitivity_set %>% 
  filter(Scenario == "S1_16_18_continuation") %>% 
  group_by(ABS_chosen_pathway, attainment_scenario) %>% 
  summarise(count = n()) %>% 
  group_by(attainment_scenario) %>% 
  mutate(prop = count/sum(count)) %>% 
  select(-count) %>% 
  pivot_wider(names_from = "ABS_chosen_pathway", values_from = "prop")

# 9.2 sensitivity of cynical attainment improvements -----------------------------

###NEEDs updating - current has a flat attainemnt adjustment rather than the seperate ones from maths and englihs as above. 

#adjusted cohort function - attainment improvements apply only to those who normally fail
run_cohort_simulation_cynical_attainment_adjustment <- function(attainment_adjustment = 0,
                                  demographic_muiltiplyer = 1.08) {
  
  #roll dice for placement on attainment distribution and eng and maths choice
  simulation_maths_UMS_add_on <- runif(min = 0,max = 10, n = nrow(simulation_pupils_and_grades))
  simulation_Eng_UMS_add_on  <- runif(min = 0,max = 10, n = nrow(simulation_pupils_and_grades))
  simulation_maths_choice <- runif(min = 0,max = 1, n = nrow(simulation_pupils_and_grades))
  simulation_Eng_choice <- runif(min = 0,max = 1, n = nrow(simulation_pupils_and_grades))
  ABS_pathway_choice <- runif(min = 0,max = 1, n = nrow(simulation_pupils_and_grades))
  Institution_choice <- runif(min = 0,max = 1, n = nrow(simulation_pupils_and_grades))
  
  #randomly generate attainment within GCSE grade
  simulation_pupils_and_base_UMS <- simulation_pupils_and_grades %>% 
    cbind(simulation_maths_UMS_add_on,
          simulation_Eng_UMS_add_on) %>% 
    mutate_at(vars(eng_gcse_15,maths_gcse_15), 
              funs(case_when(. == "9" ~ 90,
                             . == "8" ~ 80,
                             . == "7" ~ 70,
                             . == "6" ~ 60,
                             . == "5" ~ 50,
                             . == "4" ~ 40,
                             . == "3" ~ 30,
                             . == "2" ~ 20,
                             . == "1" ~ 10,
                             T ~ 0))) %>% 
    mutate(English_UMS = (eng_gcse_15+ simulation_Eng_UMS_add_on) %>%  round(digits = 1),
           Maths_UMS = (maths_gcse_15 + simulation_maths_UMS_add_on) %>%  round(digits = 1)) %>% 
    select(-c(eng_gcse_15,maths_gcse_15, simulation_maths_UMS_add_on,simulation_Eng_UMS_add_on)) 
  
  #randomly assign English and maths choice based on adjusted attainment 
  simulated_choices <- simulation_pupils_and_base_UMS %>% 
    #adjust attainment only of those who fail in the YPMAD cohort
    mutate(English_UMS_adjusted = case_when(English_UMS < 40 ~ English_UMS + attainment_adjustment,
                                            T ~ English_UMS),
           Maths_UMS_adjusted = case_when(Maths_UMS < 40 ~ Maths_UMS + attainment_adjustment,
                                          T ~ Maths_UMS)) %>% 
    
    #the rest of the functions code is as above in the main cohort model function
    mutate(English_adjusted_grade = case_when(English_UMS_adjusted  > 90 ~ "9",
                                              English_UMS_adjusted  > 80 ~ "8",
                                              English_UMS_adjusted  > 70 ~ "7",
                                              English_UMS_adjusted  > 60 ~ "6",
                                              English_UMS_adjusted  > 50 ~ "5",
                                              English_UMS_adjusted  > 40 ~ "4",
                                              English_UMS_adjusted  > 30 ~ "3",
                                              English_UMS_adjusted  > 20 ~ "2",
                                              English_UMS_adjusted  > 10 ~ "1",
                                              T ~ "H.Below G"),
           Maths_adjusted_grade = case_when(Maths_UMS_adjusted  > 90 ~ "9",
                                            Maths_UMS_adjusted  > 80 ~ "8",
                                            Maths_UMS_adjusted  > 70 ~ "7",
                                            Maths_UMS_adjusted  > 60 ~ "6",
                                            Maths_UMS_adjusted  > 50 ~ "5",
                                            Maths_UMS_adjusted  > 40 ~ "4",
                                            Maths_UMS_adjusted  > 30 ~ "3",
                                            Maths_UMS_adjusted  > 20 ~ "2",
                                            Maths_UMS_adjusted  > 10 ~ "1",
                                            T ~ "H.Below G")) %>% 
    cbind(simulation_maths_choice,
          simulation_Eng_choice,
          ABS_pathway_choice,
          Institution_choice) %>% 
    left_join(eng_behaviour, by = c("English_adjusted_grade" = "eng_gcse_15")) %>% 
    left_join(maths_behaviour, by = c("Maths_adjusted_grade" = "Math_gcse_15", "Scenario")) %>% 
    left_join(ABS_pathway_eng, by = c("English_adjusted_grade" = "Attainment.(english.L2)")) %>% 
    left_join(ABS_pathway_maths, by = c("Maths_adjusted_grade" = "Attainment.(maths.L2)","Pathway_Scenario")) %>% 
    mutate(Combined_3_majors_3_minors_threshold = (Eng_3_majors_3_minors_threshold + Maths_3_majors_3_minors_threshold)/2,
           Combined_3_majors_2_minors_threshold = (Eng_3_majors_2_minors_threshold + Maths_3_majors_2_minors_threshold)/2,
           Combined_Vocational_double_major_threshold = (Eng_Vocational_double_major_threshold + Maths_Vocational_double_major_threshold)/2,
           Combined_Apprenticeship_threshold = (Eng_Apprenticeship_threshold + Maths_Apprenticeship_threshold)/2,
           Combined_Level_2_ABS_threshold = (Eng_Level_2_ABS_threshold + Maths_Level_2_ABS_threshold)/2,
           Combined_Below_L2_threshold = (Eng_Below_L2_threshold + Maths_Below_L2_threshold)/2) %>% 
    mutate(Eng_chosen_track = case_when(simulation_Eng_choice < Eng_major_threshold ~ "Major",
                                        simulation_Eng_choice < Eng_minor_threshold ~ "Minor",
                                        simulation_Eng_choice < Eng_L2.5_threshold ~ "L2.5",
                                        simulation_Eng_choice < Eng_resit_threshold ~ "GCSE_retake",
                                        T ~ "Subject_for_life")) %>% 
    mutate(Maths_chosen_track = case_when(simulation_maths_choice < Maths_major_threshold ~ "Major",
                                          simulation_maths_choice < Maths_minor_threshold ~ "Minor",
                                          simulation_maths_choice < Maths_L2.5_threshold ~ "L2.5",
                                          simulation_maths_choice < Maths_resit_threshold ~ "GCSE_retake",
                                          T ~ "Subject_for_life")) %>% 
    mutate(ABS_chosen_pathway = case_when(ABS_pathway_choice < Combined_3_majors_3_minors_threshold ~ "3_majors_3_minors",
                                          ABS_pathway_choice < Combined_3_majors_2_minors_threshold ~ "3_majors_2_minors",
                                          ABS_pathway_choice < Combined_Vocational_double_major_threshold ~ "Vocational_double_major",
                                          ABS_pathway_choice < Combined_Apprenticeship_threshold ~ "Apprenticeship",
                                          ABS_pathway_choice < Combined_Level_2_ABS_threshold ~ "Level_2_ABS",
                                          T ~ "Below_L2")) %>%
    left_join(ABS_institution_distribution, by = c("ABS_chosen_pathway" = "ABS_pathway")) %>% 
    mutate(ABS_institution = case_when(Institution_choice < School_threshold ~ "School/SFC",
                                       Institution_choice < Tech.collage_threshold ~ "FE collage",
                                       Institution_choice < Alternative.provision_threshold  ~ "AP",
                                       Institution_choice < None_threshold ~ "None",
                                       T ~ "Other")) %>% 
    mutate(demographic_FTE = demographic_muiltiplyer)
  
  simulated_choices 
  
}

#baseline attainment 
simulated_choices_cynical_68 <- simulated_choices %>% 
  mutate(attainment_scenario = "68%")

ggplot(simulated_choices_cynical_68  ,aes(x = Maths_UMS)) + geom_histogram()

simulated_choices_cynical_68  %>% 
  mutate(GCSE_maths_pass = case_when(Maths_UMS >= 40 ~ 1, T ~ 0)) %>% 
  group_by(GCSE_maths_pass) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "GCSE_maths_pass", values_from = "count") %>% 
  mutate(prop = `1`/(`0` + `1`)) %>% 
  select(prop) %>% 
  pull()

#80% GCSE passrate
simulated_choices_cynical_80 <- run_cohort_simulation_cynical_attainment_adjustment(attainment_adjustment = 10) %>% 
  mutate(attainment_scenario = "80%")

simulated_choices_cynical_80 %>% 
  mutate(GCSE_maths_pass = case_when(Maths_UMS_adjusted >= 40 ~ 1, T ~ 0)) %>% 
  group_by(GCSE_maths_pass) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "GCSE_maths_pass", values_from = "count") %>% 
  mutate(prop = `1`/(`0` + `1`)) %>% 
  select(prop) %>% 
  pull()

#90% GCSE pass rate
simulated_choices_cynical_90 <- run_cohort_simulation_cynical_attainment_adjustment(attainment_adjustment= 20) %>% 
  mutate(attainment_scenario = "90%")

simulated_choices_cynical_90 %>% 
  mutate(GCSE_maths_pass = case_when(Maths_UMS_adjusted >= 40 ~ 1, T ~ 0)) %>% 
  group_by(GCSE_maths_pass) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = "GCSE_maths_pass", values_from = "count") %>% 
  mutate(prop = `1`/(`0` + `1`)) %>% 
  select(prop) %>% 
  pull()

#merge output tables
attainment_sensitivity_set_cynical <- rbind(simulated_choices_cynical_68,
                                    simulated_choices_cynical_80,
                                    simulated_choices_cynical_90) 

plot_cynical_data <- (attainment_sensitivity_set_cynical  %>%
                pivot_longer(cols = c(Maths_UMS_adjusted,English_UMS_adjusted), names_to = "Subject", values_to = "Adjusted_UMS")
)[c(40000:50000,6040000:6050000,4040000:4050000),]

ggplot(plot_cynical_data,
       aes(x = Adjusted_UMS, colour = attainment_scenario)) + 
  stat_ecdf() + 
  facet_wrap(~Subject, ncol = 1) + 
  geom_vline(xintercept = 40,size = 1,linetype = "dashed") + 
  geom_text(label = "Pass threshold",y = 0.5, x = 35, angle = 90,colour= "Black") +
  theme_bw() + 
  labs(title = "ABS under GCSE maths/english passrate scenarios",
       colour = "GCSE maths/english\npassrate (%)")

#number on each track 
attainemnt_pathway_plot_data <- attainment_sensitivity_set_cynical %>% 
  filter(Scenario == "S1_16_18_continuation") %>% 
  group_by(ABS_chosen_pathway, attainment_scenario) %>% 
  summarise(count = n()) %>% 
  group_by(attainment_scenario) %>% 
  mutate(prop = count/sum(count)) %>% 
  select(-count) 

#plot 
ggplot(attainemnt_pathway_plot_data %>% 
         mutate(attainment_scenario = factor(attainment_scenario, levels = c("68%","80%","90%") %>%  rev())),
       aes(y = prop, x = attainment_scenario, fill = ABS_chosen_pathway)) + 
  geom_bar(stat = "identity", position = "stack")  + 
  theme_bw() + 
  scale_y_continuous(labels = percent) + 
  labs(title = "ABS pathways according to maths/english GCSE pass rate in 2033/34",
       x = "Maths/English pass rate",
       fill = "ABS pathway") + 
  coord_flip()

# 10.1 ABS provider distribution -----------------------------------------------

#here we generate a table showing the distribution of provider types attended according the ABS model prediction of ABS pathways for the 2016/17 cohort. 
instituion_route_distribution <- simulated_choices_output_table %>% 
  filter(Scenario == "S1_16_18_continuation") %>% 
  mutate(inst_type = case_when(inst_16 %in% c("WBL","none") ~ "None",
                               inst_16 %in% c("Other KS45 source", "Other ILR Sourced") ~ "Other",
                               inst_16 %in% c("Special School","PRU") ~ "Alternative provision",
                               inst_16 %in% c("FE Coll", "CTC") ~ "Tech collage",
                               T ~ "School") %>% 
           factor(levels = c("School","Tech collage","Alternative provision","None","Other"))) %>% 
  group_by(inst_type,grouped_qual_16) %>% 
  summarise(count = n()) %>%
  group_by(grouped_qual_16) %>% 
  mutate (prop = count/sum(count)) %>% 
  select(-count) %>% 
  pivot_wider(names_from = "inst_type", values_from = "prop") %>% 
  mutate_all(funs(ifelse(is.na(.),0,.)))

# write.csv(instituion_route_distribution ,"instituion_route_distribution.csv")








