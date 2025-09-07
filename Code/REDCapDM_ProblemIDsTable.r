library(tidyverse)

a<-read.csv("Outputs/IDs_sips_noConsent.csv", col.names=c("UI", "IDs")) %>% select(IDs) %>% mutate(IDs_sips_noConsent=IDs)
b<-read.csv("Outputs/IDs_NoConsentAim1.csv", col.names=c("UI", "IDs")) %>% select(IDs) %>% mutate(IDs_NoConsentAim1=IDs)
c<-read.csv("Outputs/IDs_IROASLostToFollowUP_NoIROASNoComms.csv", col.names=c("UI", "IDs")) %>% select(IDs) %>% mutate(IDs_IROASLostToFollowUP_NoIROASNoComms=IDs)
d<-read.csv("Outputs/IDs_IROASwaitlist_NoIROASwithComms.csv", col.names=c("UI", "IDs")) %>% select(IDs) %>% mutate(IDs_IROASwaitlist_NoIROASwithComms=IDs)

full_join(full_join(full_join(a, b), c), d) %>% select(-IDs) %>% write.csv(., "Outputs/ProblemIDsTable.csv")
