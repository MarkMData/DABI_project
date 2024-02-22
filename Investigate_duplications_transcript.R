library(tidyverse)

transcript <- read.csv('transcript.csv')

colnames(transcript)
dim(transcript)
dim(transcript[duplicated(transcript),])
dim(transcript[duplicated(transcript) |
                 duplicated(transcript, fromLast = TRUE), ])

duplicates<-transcript[duplicated(transcript),]
duplicates

duplicates<-transcript[duplicated(transcript),] %>% select(person_id)
duplicates

transcript %>% filter(person_id==duplicates[1,1])
transcript %>% filter(person_id==duplicates[2,1])
transcript %>% filter(person_id==duplicates[3,1])
transcript %>% filter(person_id==duplicates[4,1])
transcript %>% filter(person_id==duplicates[5,1])
transcript %>% filter(person_id==duplicates[200,1])
transcript %>% filter(person_id==duplicates[379,1])

# investigate number of offers received 
transcript %>% filter(person_id %in% duplicates$person_id, offer_received==1) %>%  
  # group by offer and person_id
  group_by(person_id, offer_id) %>% 
  count() %>% 
  # ungroup and group only by person_id
  ungroup() %>% 
  group_by(person_id) %>% 
  # max number of same offer received by each person
  summarise(max_n=max(n)) %>% 
  ungroup() %>% 
  # min of all the maximum offers receive
  summarise(min(max_n))
