# by event data 
library(REDCapDM)
library(tidyverse)

source("Code/Functions/removeNonValids().r")
source("Code/Functions/getThoseGivingConsent().r")

#----------------------------
# Download the data from redcap
# ----------------------------
# dataset_api <- redcap_data(uri = "https://redcap.research.yale.edu/redcap_v15.0.38/API/",
#                            token = "6F44F9B012E9497C8CCCC366D9297CED") 
# saveRDS(dataset_api, "Outputs/dataset_api_output.RDS")
# ---------------------------------

#load and transform
 dataset_api<-readRDS( "Outputs/dataset_api_output.RDS")

# transformed_data <- rd_transform(dataset_api,
#                         final_format = "by_event")

# saveRDS(transformed_data, "Outputs/transformed_data_output.RDS")
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

#number statred
NUM_uniqueStarts<-dim(dat)[1]
#numebr exlcuedd NA
NA
#number incomplete (use voicescreen1 as metric for compelteness)
NUM_screeningIncomplete<-dat %>% select(email, voicescreen1) %>%
    filter(!complete.cases(.)) %>%
    pull(email) %>%
    length

#number complete (use voicescreen1 as metric for compelteness)
ScreenCompleteEmails<-dat %>% select(email, voicescreen1) %>%
    filter(complete.cases(.)) %>%
    pull(email)
length(ScreenCompleteEmails)


# eligibility startable
NUM_screeningComplete<-length(ScreenCompleteEmails)

# deemed ineligible unless specifically marked eligible
NUM_ineligible<-dat %>% 
    filter(email %in%ScreenCompleteEmails) %>%
    filter(
        elig_determination_ineligible=="Yes" | 
        elig_determination_full_hysterectomy_screen_but_not_eligible=="Yes" |
        elig_determination_withdrawn=="Yes" |
        elig_determination_potential_future_eligible=="Yes") %>%
        nrow

# PGAs not decided
# if no answers given
NUM_eligiblityUndecided<-dat %>%
    filter(email %in% ScreenCompleteEmails) %>% 
    filter(
        elig_determination_ineligible=="No" &
        elig_determination_full_hysterectomy_screen_but_not_eligible=="No" &
        elig_determination_withdrawn=="No" &
        elig_determination_potential_future_eligible=="No" &
        elig_determination_aim_1_map_psychosis_group=="No" & 
        elig_determination_aim_1_mad_depression_group=="No" &
        elig_determination_aim_2_chr_group=="No" &
        elig_determination_aim_2_hc_group=="No")    %>%
        nrow

# PGAs deemed eligible for at least one group in at least one aim
# only deemed eligible if marked eligible and NOT marked ineligible
eligibleActual_df<-dat %>%
    filter(
        elig_determination_aim_1_map_psychosis_group=="Yes" |
        elig_determination_aim_1_mad_depression_group=="Yes" |
        elig_determination_aim_2_chr_group=="Yes" |
        elig_determination_aim_2_hc_group=="Yes") %>% 
    filter(
        elig_determination_ineligible=="No" & 
        elig_determination_full_hysterectomy_screen_but_not_eligible=="No" &
        elig_determination_withdrawn=="No" &
        elig_determination_potential_future_eligible=="No")

eligible_df<-(eligibleActual_df) %>% filter(email %in% ScreenCompleteEmails) 
NUM_eligible<-nrow(eligible_df)
NUM_eligibleActual<-nrow(eligibleActual_df)

#checksum
NUM_screeningComplete==NUM_eligible+NUM_eligiblityUndecided+NUM_ineligible

# number contacted at least once
contacted_df<-d %>% as_tibble %>%
    filter(redcap_repeat_instrument=="communications") %>%
    filter(record_id %in% eligible_df$record_id) %>%
    filter(!is.na(emaildate))
    
NUM_contacted<-contacted_df %>%
    pull(record_id) %>%
    unique %>%
    length

#number eligible not contcted at all
NUM_uncontacted<-NUM_eligible-NUM_contacted

# total contacted actual
contactedActual_df<-d %>% as_tibble %>%
    filter(redcap_repeat_instrument=="communications") %>%
    filter(record_id %in% dat$record_id) %>%
    filter(!is.na(emaildate))
    
NUM_contactedActual<-contactedActual_df %>%
    pull(record_id) %>%
    unique %>%
    length

# number scheduled

scheduling_df<-d %>% as_tibble %>%
    filter(redcap_repeat_instrument=="scheduling") %>%
    filter(record_id %in% contacted_df$record_id) %>%
    filter(!is.na(interviewdate)) 

NUM_scheduled<-scheduling_df %>%
    pull(record_id) %>%
    unique %>%
    length

#number of actualScheduled
schedulingActual_df<-d %>% as_tibble %>%
    filter(redcap_repeat_instrument=="scheduling") %>%
    filter(record_id %in% dat$record_id) %>%
    filter(!is.na(interviewdate)) 

NUM_scheduledActual<-schedulingActual_df %>%
    pull(record_id) %>%
    unique %>%
    length


# number to withdraw as last contact over 6 months ago and no scheduling

ids_last_contacted_morethan_6months_ago<-contacted_df %>%
    group_by(record_id) %>%
    summarise(emaildate=last(emaildate)) %>%
    filter(emaildate< today() - months(6)) %>%
    pull(record_id)

NUM_lostinscheduling<-length(unique(ids_last_contacted_morethan_6months_ago[!ids_last_contacted_morethan_6months_ago %in% scheduling_df$record_id]))


#number awaiting scheduling
ids_last_contacted_lessthan_6months_ago<-contacted_df %>%
    group_by(record_id) %>%
    summarise(emaildate=last(emaildate)) %>%
    filter(emaildate >= today() - months(6)) %>%
    pull(record_id)

NUM_unscheduled<-length(unique(ids_last_contacted_lessthan_6months_ago[!ids_last_contacted_lessthan_6months_ago %in% scheduling_df$record_id]))

#check addition
NUM_contacted==NUM_scheduled+NUM_lostinscheduling+NUM_unscheduled


# consent starting == number eligible - making an exception for final 2 columns (consent and SIPS) to highlight process failures
NUM_eligible

# number eligible without consent
NUM_unconsenting<-eligible_df %>%
    filter(is.na(sign_consent2_v3) & formerlyenrolled_2_enrolled_using_old_consent_form=="No" &
     is.na(sign_consent1_v3) & formerlyenrolled_enrolled_using_old_consent_form=="No") %>%
     pull(record_id) %>%
     length

# number of eligible giving consent

consenting_ids<-getThoseGivingConsent(eligible_df) 
NUM_consenting<-length(consenting_ids)
NUM_consentingActual<-getThoseGivingConsent(dat) %>% length

# checksum
NUM_eligible==NUM_unconsenting+NUM_consenting

#starting sips == those consenting and eligible
NUM_consenting

# num consenting and not started
NUM_notstartedSips<-eligible_df %>%
    filter(record_id %in% consenting_ids) %>%
    filter(is.na(sipsdate)) %>% 
    pull(record_id) %>% 
    unique %>%
    length()

NUM_startedSipsAndIncomplete<-dataset_api$data %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% consenting_ids) %>%
    filter(sips_complete==0 & !is.na(sipsdate)) %>%
    nrow


# num completed
SIPsComplete_ids<-dataset_api$data %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% consenting_ids) %>%
    filter(sips_complete==2) %>%
    pull(record_id)
 
NUM_completingSips<- unique(SIPsComplete_ids) %>% length()

# checksum
NUM_consenting==NUM_notstartedSips+NUM_startedSipsAndIncomplete+NUM_completingSips

NUM_completingSipsActual<-dataset_api$data %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% dat$record_id) %>%
    filter(sips_complete==2) %>%
    pull(record_id) %>% 
    unique %>%
    length()

IDs_completingSipsActual<-dataset_api$data %>%
    filter(is.na(redcap_repeat_instrument)) %>%
    filter(record_id %in% dat$record_id) %>%
    filter(sips_complete==2) %>%
    pull(unique(record_id)) 


OutputTable<-data.frame(
    "Prescreening" = c(NUM_uniqueStarts , NA , NUM_screeningIncomplete ,  NUM_screeningComplete , NA),
    "EligibilityAssessment"= c(NUM_screeningComplete, NUM_ineligible, NUM_eligiblityUndecided , NUM_eligible, NUM_eligibleActual),
    "Communication"= c(NUM_eligible, NA, NUM_uncontacted, NUM_contacted, NUM_contactedActual),
    "Scheduling"= c(NUM_contacted, NUM_lostinscheduling,  NUM_unscheduled, NUM_scheduled , NUM_scheduledActual),
    "Consenting"= c(NUM_eligible , NA, NUM_unconsenting, NUM_consenting, NUM_consentingActual),
    "SIPsInterviewing"= c (NUM_consenting, NA, NUM_notstartedSips, NUM_completingSips, NUM_completingSipsActual)
)

rownames(OutputTable)<-c("Entering", "Excluded", "InPool", "Complete", "ActualComplete")

OutputTable

write.csv(OutputTable, "Outputs/OutputTable.csv")



