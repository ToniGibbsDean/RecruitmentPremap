#function to remove NAs in prescreeen date and email; and to remove repeated emails with the most NA values 
# i.e. completion attempts that were subsequently improved upon.

removeNonValids<-function(d){
    # filter non-started entries without prescreen dates or emails
    dat<-d %>% as_tibble %>%
        filter(!is.na(prescreendate)) %>%
        filter(!is.na(email)) %>%
        mutate(
            numNAs=rowSums(is.na(.))        
            )

    # filter out repeated emails based on numNAs
    rep_emails<-unique(dat$email[duplicated(dat$email)])

    UIsToRemove<-dat %>% 
        filter(email %in% rep_emails) %>% 
        group_by(email) %>%
        slice(-which.max(numNAs)) %>%
        pull(UI)

    dat<-dat %>% filter(!UI %in% UIsToRemove) %>% select(-numNAs)

    return(dat)
}

#function to remove staff tests

removeTests<-function(d){
    # remove staff testing
    dat<-d %>%
        filter( !grepl("yale.edu", email)) %>%
        filter( !grepl("test", record_id))
    return(dat)
}
