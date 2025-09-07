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


# aim2
aim2<-transformed_data$data$df[[3]] %>% 
    as_tibble %>%
    filter(record_id %in% dat$record_id) 
    
aim2_nonRepeatedInstruments<-aim2    %>%
    filter(is.na(redcap_repeat_instrument))
length(aim2_nonRepeatedInstruments$record_id)==length(unique(aim2_nonRepeatedInstruments$record_id))


#hormone kit completion
dataset_api$data %>% 
    as_tibble %>%
    filter(redcap_event_name=="aim_2_arm_1") %>%
    filter(record_id %in% aim2$record_id) %>%
    filter(redcap_repeat_instrument=="hormone_kits") %>%
    group_by(record_id) %>%
    filter(redcap_repeat_instance==max(redcap_repeat_instance)) %>%
    mutate(hormoneCompletion=hormone_kits_complete==2) %>%
    ungroup() %>%
    group_by(redcap_repeat_instance, hormoneCompletion) %>%
    summarise(n=n())


#evaluation survey completion
dataset_api$data %>% 
    as_tibble %>%
    filter(redcap_event_name=="aim_2_arm_1") %>%
    filter(record_id %in% aim2$record_id) %>%
    filter(redcap_repeat_instrument=="evaluation_surveys") %>%
    group_by(record_id) %>%
    filter(redcap_repeat_instance==max(redcap_repeat_instance)) %>%
    mutate(evalCompletion=evaluation_surveys_complete==2) %>%
    ungroup() %>%
    group_by(redcap_repeat_instance, evalCompletion) %>%
    summarise(n=n())

#symp diary completion
dataset_api$data %>% 
    as_tibble %>%
    filter(redcap_event_name=="aim_2_arm_1") %>%
    filter(record_id %in% aim2$record_id) %>% 
    filter(redcap_repeat_instrument=="symptom_diary") %>%
    group_by(record_id) %>%
    filter(redcap_repeat_instance==max(redcap_repeat_instance)) %>%
    mutate(symDiaryCompletion=symptom_diary_complete==2) %>%
    ungroup() %>%
    group_by(redcap_repeat_instance, symDiaryCompletion) %>%
    summarise(n=n())



# auditory ch task completion
dataset_api$data %>% 
    as_tibble %>%
    filter(redcap_event_name=="aim_2_arm_1") %>%
    filter(record_id %in% aim2$record_id) %>% 
    filter(is.na(redcap_repeat_instrument)) %>%
    mutate(evaluation_1_audch_task_complete = evaluation_1_audch_task_complete==2, evaluation_2_audch_task_complete = evaluation_2_audch_task_complete==2, evaluation_3_audch_task_complete = evaluation_3_audch_task_complete==2, evaluation_4_audch_task_complete = evaluation_4_audch_task_complete==2, evaluation_5_audch_task_complete= evaluation_5_audch_task_complete==2) %>%
    mutate(audchCompletion=evaluation_1_audch_task_complete + evaluation_2_audch_task_complete + evaluation_3_audch_task_complete + evaluation_4_audch_task_complete+ evaluation_5_audch_task_complete) %>%
    group_by(audchCompletion) %>% 
    summarise(n=n())
    

#eeg completion
dataset_api$data %>% 
    as_tibble %>% 
    filter(redcap_event_name=="aim_2_in_person_arm_1") %>%
    filter(record_id %in% aim2$record_id) %>% 
    filter(is.na(redcap_repeat_instrument)) %>%
    mutate(eegCompletion=aim_2_eeg_tasks_complete==2) %>%
    group_by(eegCompletion) %>%
    summarise(n=n())

#fmri completion
dataset_api$data %>% 
    as_tibble %>% 
    filter(redcap_event_name=="aim_2_in_person_arm_1") %>%
    filter(record_id %in% aim2$record_id) %>% 
    filter(is.na(redcap_repeat_instrument)) %>%
    mutate(scannerProtocolCompletion=scanner_protocol_complete==2) %>%
    group_by(scannerProtocolCompletion) %>% 
    summarise(n=n())