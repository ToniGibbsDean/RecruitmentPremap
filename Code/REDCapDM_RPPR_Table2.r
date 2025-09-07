# by event data 
library(REDCapDM)
library(tidyverse)

source("Code/Functions/removeNonValids().r")
source("Code/Functions/getThoseGivingConsent().r")

#load
dataset_api<-readRDS( "Outputs/dataset_api_output.RDS")
transformed_data<-readRDS("Outputs/transformed_data_output.RDS")

#get main df, remove tests and add genuine UI
d<-transformed_data$data$df[[1]] %>% removeTests()
d$UI<-1:dim(d)[1] # add genuinely Unique ID

dat<-d %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    removeNonValids()

IDswithdrawm <- dat %>% filter(elig_determination_withdrawn=="Yes") %>% pull(record_id)

###################
# check all dupes removed
##################
length(dat$email)==length(unique(dat$email))
length(dat$record_id)==length(unique(dat$record_id))


# aim1
aim1<-transformed_data$data$df[[2]] %>% 
    as_tibble %>%
    filter(record_id %in% dat$record_id) %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    left_join(select(dat, record_id, prescreendate)) 

length(aim1$record_id)==length(unique(aim1$record_id))

############################
#######CONSENT ISSUE#######
############################
aim1_onlyInclConsented<-transformed_data$data$df[[2]] %>% 
    as_tibble %>%
    filter(record_id %in% dat$record_id) %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    left_join(select(dat, record_id, prescreendate, sign_consent2_v3, formerlyenrolled_2_enrolled_using_old_consent_form)) %>%
    getThoseGivingConsent 
    
IDsNoConsentAim1<-aim1 %>% filter(!record_id %in% aim1_onlyInclConsented) %>% filter(!record_id %in% IDswithdrawm)  %>% pull(record_id)

IDswithConsentAim1<-aim1 %>% filter(record_id %in% aim1_onlyInclConsented) %>% filter(!record_id %in% IDswithdrawm)  %>% pull(record_id)

write.csv(IDsNoConsentAim1, "Outputs/IDs_NoConsentAim1.csv")

############################
#######CONSENT ISSUE#######
############################

# get last comms per person under aim1
lastAim1Comms<-dataset_api$data %>%
        as_tibble %>%
    filter(redcap_event_name=="aim_1_arm_1") %>%
    filter(redcap_repeat_instrument == "communications") %>%
    filter(record_id %in% aim1$record_id) %>%
    group_by(record_id) %>%
    filter(redcap_repeat_instance==max(redcap_repeat_instance)) %>%
    select(record_id, emaildate)

# get number completeing iraos instruments, split by most recent comms
dataset_api$data %>%
        as_tibble %>%
    filter(redcap_event_name=="aim_1_arm_1") %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% aim1$record_id) %>%
    select(-emaildate) %>% # essential to remove columns before left joining them in from other rows (raw database has all columns but with NAs in most rows)
    left_join((lastAim1Comms)) %>%
    mutate(iraos_1_complete = iraos_1_complete==2, iraos_2_complete = iraos_2_complete==2, iraos_3_complete = iraos_3_complete==2, iraos_4_complete = iraos_4_complete==2) %>%
    mutate(iraosCompletion=iraos_1_complete + iraos_2_complete + iraos_3_complete + iraos_4_complete) %>%
    mutate(commsInLast6Months=emaildate<(today()-months(6))) %>%
    mutate(commsInLast6Months=replace_na(commsInLast6Months, 0)) %>%
    group_by(commsInLast6Months,
            iraosCompletion) %>% 
    summarise(n=n())


IDsNoCommsNoIROAS <- dataset_api$data %>% 
        as_tibble %>%
    filter(redcap_event_name=="aim_1_arm_1") %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% aim1$record_id) %>%
    select(-emaildate) %>% # essential to remove columns before left joining them in from other rows (raw database has all columns but with NAs in most rows)
    left_join((lastAim1Comms)) %>%
    mutate(iraos_1_complete = iraos_1_complete==2, iraos_2_complete = iraos_2_complete==2, iraos_3_complete = iraos_3_complete==2, iraos_4_complete = iraos_4_complete==2) %>%
    mutate(iraosCompletion=iraos_1_complete + iraos_2_complete + iraos_3_complete + iraos_4_complete) %>%
    mutate(commsInLast6Months=emaildate<(today()-months(6))) %>%
    mutate(commsInLast6Months=replace_na(commsInLast6Months, 0)) %>%
    group_by(commsInLast6Months,
            iraosCompletion) %>%
    filter(commsInLast6Months==FALSE & iraosCompletion==0 | iraosCompletion==1 | iraosCompletion==2) %>%
    filter(!record_id %in% IDswithdrawm) %>%
    pull(record_id)

IDsCommsNoIROAS_waitlist <- dataset_api$data %>% 
        as_tibble %>%
    filter(redcap_event_name=="aim_1_arm_1") %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% aim1$record_id) %>%
    select(-emaildate) %>% # essential to remove columns before left joining them in from other rows (raw database has all columns but with NAs in most rows)
    left_join((lastAim1Comms)) %>%
    mutate(iraos_1_complete = iraos_1_complete==2, iraos_2_complete = iraos_2_complete==2, iraos_3_complete = iraos_3_complete==2, iraos_4_complete = iraos_4_complete==2) %>%
    mutate(iraosCompletion=iraos_1_complete + iraos_2_complete + iraos_3_complete + iraos_4_complete) %>%
    mutate(commsInLast6Months=emaildate<(today()-months(6))) %>%
    mutate(commsInLast6Months=replace_na(commsInLast6Months, 0)) %>%
    group_by(commsInLast6Months,
            iraosCompletion) %>%
    filter(commsInLast6Months==TRUE & iraosCompletion==0) %>%
    filter(!record_id %in% IDswithdrawm) %>%
    pull(record_id) 

write.csv(IDsNoCommsNoIROAS, "Outputs/IDs_IROASLostToFollowUP_NoIROASNoComms.csv")
write.csv(IDsCommsNoIROAS_waitlist, "Outputs/IDs_IROASwaitlist_NoIROASwithComms.csv")
