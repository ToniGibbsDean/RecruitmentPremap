getThoseGivingConsent<-function(df){
    aim2_consented_ids<-df %>% 
    filter(!is.na(sign_consent2_v3) | formerlyenrolled_2_enrolled_using_old_consent_form=="Yes" ) %>%
    pull(record_id)

    aim1_consented_ids<-df %>% 
        filter(!is.na(sign_consent1_v3) | formerlyenrolled_enrolled_using_old_consent_form=="Yes")%>%
        pull(record_id)

    c(aim1_consented_ids, aim2_consented_ids) %>% unique %>%  return()
}