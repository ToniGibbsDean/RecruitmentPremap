# by event data 
library(REDCapDM)
library(tidyverse)

source("Code/Functions/removeNonValids().r")
source("Code/Functions/getThoseGivingConsent().r")

#functions
getReportingPeriodColumns<-function(data) {
    data %>%
        mutate(inThisPeriod=prescreendate>reportingPeriodStart) %>%
        group_by(inThisPeriod) %>%
        summarise(n=n())
}

# params
reportingPeriodStart<-as_date("2024-09-15")
#

#load
dataset_api<-readRDS( "Outputs/dataset_api_output.RDS")
transformed_data<-readRDS("Outputs/transformed_data_output.RDS")

#get main df, remove tests and add genuine UI
d<-transformed_data$data$df[[1]] %>% removeTests()
d$UI<-1:dim(d)[1] # add genuinely Unique ID

dat<-d %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    removeNonValids()

###################
# check all dupes removed
##################

length(dat$email)==length(unique(dat$email))

length(dat$record_id)==length(unique(dat$record_id))

###################
# make table
###################

#number complete (use voicescreen1 as metric for compelteness)
dat %>% 
    filter(!is.na(voicescreen1)) %>%
    getReportingPeriodColumns

#number eligible
dat %>%
    filter(
        elig_determination_aim_1_map_psychosis_group=="Yes" |
        elig_determination_aim_1_mad_depression_group=="Yes" |
        elig_determination_aim_2_chr_group=="Yes" |
        elig_determination_aim_2_hc_group=="Yes") %>% 
    filter(
        elig_determination_ineligible=="No" & 
        elig_determination_full_hysterectomy_screen_but_not_eligible=="No" &
        elig_determination_withdrawn=="No" &
        elig_determination_potential_future_eligible=="No") %>%
    getReportingPeriodColumns


#number consented
consentedIds<-dat %>% getThoseGivingConsent
dat %>%
    filter(record_id %in% consentedIds) %>%
    getReportingPeriodColumns

# number complting SIPS
dataset_api$data %>%
    as_tibble %>%
    filter(redcap_event_name=="screening_arm_1") %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% dat$record_id) %>%
    filter(record_id %in% consentedIds) %>%
    filter(sips_complete==2) %>%
    getReportingPeriodColumns

# GET IDS for SIPS with no Consent
IDswithdrawm <- dat %>% filter(elig_determination_withdrawn=="Yes") %>% pull(record_id)

IDS_sips_noConsent<-dataset_api$data %>%
    as_tibble %>%
    filter(redcap_event_name=="screening_arm_1") %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% dat$record_id) %>%
    filter(!record_id %in% consentedIds) %>%
    filter(!record_id %in% IDswithdrawm) %>%
    filter(sips_complete==2) %>%
    pull(record_id)

write.csv(IDS_sips_noConsent, "Outputs/IDs_sips_noConsent.csv")



# aim1
aim1<-transformed_data$data$df[[2]] %>% 
    as_tibble %>%
    filter(record_id %in% dat$record_id) %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    left_join(select(dat, record_id, prescreendate))

length(aim1$record_id)==length(unique(aim1$record_id))

aim1 %>%
    getReportingPeriodColumns

aim1 %>%
    left_join(select(dat, record_id, elig_determination_aim_1_map_psychosis_group, elig_determination_aim_1_mad_depression_group)) %>%
    mutate(diagnosis = case_when(
        elig_determination_aim_1_map_psychosis_group=="Yes" & elig_determination_aim_1_mad_depression_group=="Yes" ~ "both",
        elig_determination_aim_1_map_psychosis_group=="Yes" & elig_determination_aim_1_mad_depression_group=="No" ~ "MAP",
        elig_determination_aim_1_map_psychosis_group=="No" & elig_determination_aim_1_mad_depression_group=="Yes" ~ "MAD",
        elig_determination_aim_1_map_psychosis_group=="No" & elig_determination_aim_1_mad_depression_group=="No" ~ "neither",
        .default=NA)) %>%
    mutate(inThisPeriod=prescreendate>reportingPeriodStart) %>%
    group_by(inThisPeriod, diagnosis) %>%
  #summarise(n=n())
    filter(!diagnosis=="neither") %>%
    filter(!record_id %in% IDswithdrawm) %>%
  summarise(n=n())
    pull(record_id) 

    ###issue that IDs of neither show people with withdrawn, and people with AIM 2 status - way to see all blank? 

# aim2
aim2<-transformed_data$data$df[[3]] %>% 
    as_tibble %>%
    filter(record_id %in% dat$record_id) %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    left_join(select(dat, record_id, prescreendate))

length(aim2$record_id)==length(unique(aim2$record_id))

aim2 %>%
    getReportingPeriodColumns

aim2 %>%
    left_join(select(dat, record_id, elig_determination_aim_2_chr_group, elig_determination_aim_2_hc_group)) %>%
    mutate(diagnosis = case_when(
        elig_determination_aim_2_chr_group=="Yes" & elig_determination_aim_2_hc_group=="Yes" ~ "both",
        elig_determination_aim_2_chr_group=="Yes" & elig_determination_aim_2_hc_group=="No" ~ "CHR",
        elig_determination_aim_2_chr_group=="No" & elig_determination_aim_2_hc_group=="Yes" ~ "HC",
        elig_determination_aim_2_chr_group=="No" & elig_determination_aim_2_hc_group=="No" ~ "neither",
        .default=NA))  %>%
    mutate(inThisPeriod=prescreendate>reportingPeriodStart) %>%
    group_by(inThisPeriod, diagnosis) %>%
    summarise(n=n())

# number withdrawn
#number eligible
dat %>%
    filter(elig_determination_withdrawn=="Yes") %>%
    getReportingPeriodColumns