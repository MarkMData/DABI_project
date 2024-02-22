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
