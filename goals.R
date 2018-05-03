Goals<-read.csv("Data/Goals/Goals Tracker.csv")

#Objective1

Goal11<-read.csv("Data/Goals/Goal1Objective1.csv") %>% replace_na(list(Progress = " "))
Goal12 <-read.csv("Data/Goals/Goal1Objective2.csv") %>% replace_na(list(Progress = " "))
Goal13 <-read.csv("Data/Goals/Goal1Objective3.csv") %>% replace_na(list(Progress = " "))
Goal14 <-read.csv("Data/Goals/Goal1Objective4.csv") %>% replace_na(list(Progress = " "))

#objective2

Goal21 <-read.csv("Data/Goals/Goal2Objective1.csv") %>% replace_na(list(Progress = " "))
Goal22 <-read.csv("Data/Goals/Goal2Objective2.csv") %>% replace_na(list(Progress = " "))
Goal23 <-read.csv("Data/Goals/Goal2Objective3.csv") %>% replace_na(list(Progress = " "))
Goal24 <-read.csv("Data/Goals/Goal2Objective4.csv") %>% replace_na(list(Progress = " "))
Goal25 <-read.csv("Data/Goals/Goal2Objective5.csv") %>% replace_na(list(Progress = " "))
Goal26 <-read.csv("Data/Goals/Goal2Objective6.csv") %>% replace_na(list(Progress = " "))


#objective3
Goal31 <-read.csv("Data/Goals/Goal3Objective1.csv") %>% replace_na(list(Progress = " "))
Goal32 <-read.csv("Data/Goals/Goal3Objective2.csv") %>% replace_na(list(Progress = " "))
Goal33 <-read.csv("Data/Goals/Goal3Objective3.csv") %>% replace_na(list(Progress = " "))
