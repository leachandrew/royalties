sproule_data<-read_xlsx("sproule_2022.xlsx",sheet = "North American Oil",range = "B8:P22")%>%
  slice(-c(1)) %>% 
  clean_names()%>%
  mutate(year=gsub(" Act","",year))

names(sproule_data)<-gsub("x1_","",names(sproule_data))
names(sproule_data)<-gsub("x2_","",names(sproule_data))
names(sproule_data)<-gsub("x3_","",names(sproule_data))
