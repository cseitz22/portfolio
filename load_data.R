# load packages

library(tidyverse)
library(tidyr)


# create good data files

adults <- read_csv("RawDataAdults.csv", show_col_types = FALSE)
kids12 <- read_csv("RawDataChildren12.csv", show_col_types = FALSE)
kids6 <- read_csv("RawDataChildren6.csv", show_col_types = FALSE)

block_num <- rep(c(1,2,3,4,5,6), each=60, times=27)
adults <- cbind(adults, block_num)

age <- (rep(c("adult"), times=9720))
adults<- cbind(adults, age)

#kids12
block_num1 <- rep(c(1,2,3,4,5,6), each=60, times=6)
kids12 <- cbind(kids12, block_num1)

age1 <- (rep(c("child"), times=2160))
kids12<- cbind(kids12, age1)

#kids6
block_num2 <- rep(c(1,2,3,4,5,6), each=60, times=4)
kids6 <- cbind(kids6, block_num2)

age2 <- (rep(c("child"), times=1440))
kids6<- cbind(kids6, age2)

adults$rt <- adults$rt *1000
kids12$rt <- kids12$rt *1000
kids6$rt <- kids6$rt *1000

##need to add something for distractor absent trials - I think I added this
adults <- adults %>%
  mutate(relative_hp_dist=
           case_when(dist_loc==0 ~ 4,
                     dist_loc==1 & hp_loc==1 ~ 0,
                     dist_loc==1 & hp_loc==2 ~ 1,
                     dist_loc==1 & hp_loc==3 ~ 2,
                     dist_loc==1 & hp_loc==4 ~ 3,
                     dist_loc==1 & hp_loc==5 ~ 2,
                     dist_loc==1 & hp_loc==6 ~ 1,
                     dist_loc==2 & hp_loc==1 ~ 1,
                     dist_loc==2 & hp_loc==2 ~ 0,
                     dist_loc==2 & hp_loc==3 ~ 1,
                     dist_loc==2 & hp_loc==4 ~ 2,
                     dist_loc==2 & hp_loc==5 ~ 3,
                     dist_loc==2 & hp_loc==6 ~ 2,
                     dist_loc==3 & hp_loc==1 ~ 2,
                     dist_loc==3 & hp_loc==2 ~ 1,
                     dist_loc==3 & hp_loc==3 ~ 0,
                     dist_loc==3 & hp_loc==4 ~ 1,
                     dist_loc==3 & hp_loc==5 ~ 2,
                     dist_loc==3 & hp_loc==6 ~ 3,
                     dist_loc==4 & hp_loc==1 ~ 3,
                     dist_loc==4 & hp_loc==2 ~ 2,
                     dist_loc==4 & hp_loc==3 ~ 1,
                     dist_loc==4 & hp_loc==4 ~ 0,
                     dist_loc==4 & hp_loc==5 ~ 1,
                     dist_loc==4 & hp_loc==6 ~ 2,
                     dist_loc==5 & hp_loc==1 ~ 2,
                     dist_loc==5 & hp_loc==2 ~ 3,
                     dist_loc==5 & hp_loc==3 ~ 2,
                     dist_loc==5 & hp_loc==4 ~ 1,
                     dist_loc==5 & hp_loc==5 ~ 0,
                     dist_loc==5 & hp_loc==6 ~ 1,
                     dist_loc==6 & hp_loc==1 ~ 1,
                     dist_loc==6 & hp_loc==2 ~ 2,
                     dist_loc==6 & hp_loc==3 ~ 3,
                     dist_loc==6 & hp_loc==4 ~ 2,
                     dist_loc==6 & hp_loc==5 ~ 1,
                     dist_loc==6 & hp_loc==6 ~ 0,
           ))

adults <- adults %>%
  mutate(relative_target=
           case_when(target_loc==1 & hp_loc==1 ~ 0,
                     target_loc==2 & hp_loc==1 ~ 1,
                     target_loc==3 & hp_loc==1 ~ 2,
                     target_loc==4 & hp_loc==1 ~ 3,
                     target_loc==5 & hp_loc==1 ~ 2,
                     target_loc==6 & hp_loc==1 ~ 1,
                     target_loc==1 & hp_loc==2 ~ 1,
                     target_loc==2 & hp_loc==2 ~ 0,
                     target_loc==3 & hp_loc==2 ~ 1,
                     target_loc==4 & hp_loc==2 ~ 2,
                     target_loc==5 & hp_loc==2 ~ 3,
                     target_loc==6 & hp_loc==2 ~ 2,
                     target_loc==1 & hp_loc==3 ~ 2,
                     target_loc==2 & hp_loc==3 ~ 1,
                     target_loc==3 & hp_loc==3 ~ 0,
                     target_loc==4 & hp_loc==3 ~ 1,
                     target_loc==5 & hp_loc==3 ~ 2,
                     target_loc==6 & hp_loc==3 ~ 3,
                     target_loc==1 & hp_loc==4 ~ 3,
                     target_loc==2 & hp_loc==4 ~ 2,
                     target_loc==3 & hp_loc==4 ~ 1,
                     target_loc==4 & hp_loc==4 ~ 0,
                     target_loc==5 & hp_loc==4 ~ 1,
                     target_loc==6 & hp_loc==4 ~ 2,
                     target_loc==1 & hp_loc==5 ~ 2,
                     target_loc==2 & hp_loc==5 ~ 3,
                     target_loc==3 & hp_loc==5 ~ 2,
                     target_loc==4 & hp_loc==5 ~ 1,
                     target_loc==5 & hp_loc==5 ~ 0,
                     target_loc==6 & hp_loc==5 ~ 1,
                     target_loc==1 & hp_loc==6 ~ 1,
                     target_loc==2 & hp_loc==6 ~ 2,
                     target_loc==3 & hp_loc==6 ~ 3,
                     target_loc==4 & hp_loc==6 ~ 2,
                     target_loc==5 & hp_loc==6 ~ 1,
                     target_loc==6 & hp_loc==6 ~ 0,))


adults$relative_target[adults$relative_target == 0] <- "high_prob"
adults$relative_target[adults$relative_target == 1] <- "lp_1"
adults$relative_target[adults$relative_target == 2] <- "lp_2"
adults$relative_target[adults$relative_target == 3] <- "lp_3"

adults$relative_hp_dist[adults$relative_hp_dist == 0] <- "high_prob"
adults$relative_hp_dist[adults$relative_hp_dist == 1] <- "lp_1"
adults$relative_hp_dist[adults$relative_hp_dist == 2] <- "lp_2"
adults$relative_hp_dist[adults$relative_hp_dist == 3] <- "lp_3"
adults$relative_hp_dist[adults$relative_hp_dist == 4] <- "absent"


#kids12

kids12 <- kids12 %>%
  mutate(relative_hp_dist=
           case_when(dist_loc==0 ~ 4,
                     dist_loc==1 & hp_loc==1 ~ 0,
                     dist_loc==1 & hp_loc==2 ~ 1,
                     dist_loc==1 & hp_loc==3 ~ 2,
                     dist_loc==1 & hp_loc==4 ~ 3,
                     dist_loc==1 & hp_loc==5 ~ 2,
                     dist_loc==1 & hp_loc==6 ~ 1,
                     dist_loc==2 & hp_loc==1 ~ 1,
                     dist_loc==2 & hp_loc==2 ~ 0,
                     dist_loc==2 & hp_loc==3 ~ 1,
                     dist_loc==2 & hp_loc==4 ~ 2,
                     dist_loc==2 & hp_loc==5 ~ 3,
                     dist_loc==2 & hp_loc==6 ~ 2,
                     dist_loc==3 & hp_loc==1 ~ 2,
                     dist_loc==3 & hp_loc==2 ~ 1,
                     dist_loc==3 & hp_loc==3 ~ 0,
                     dist_loc==3 & hp_loc==4 ~ 1,
                     dist_loc==3 & hp_loc==5 ~ 2,
                     dist_loc==3 & hp_loc==6 ~ 3,
                     dist_loc==4 & hp_loc==1 ~ 3,
                     dist_loc==4 & hp_loc==2 ~ 2,
                     dist_loc==4 & hp_loc==3 ~ 1,
                     dist_loc==4 & hp_loc==4 ~ 0,
                     dist_loc==4 & hp_loc==5 ~ 1,
                     dist_loc==4 & hp_loc==6 ~ 2,
                     dist_loc==5 & hp_loc==1 ~ 2,
                     dist_loc==5 & hp_loc==2 ~ 3,
                     dist_loc==5 & hp_loc==3 ~ 2,
                     dist_loc==5 & hp_loc==4 ~ 1,
                     dist_loc==5 & hp_loc==5 ~ 0,
                     dist_loc==5 & hp_loc==6 ~ 1,
                     dist_loc==6 & hp_loc==1 ~ 1,
                     dist_loc==6 & hp_loc==2 ~ 2,
                     dist_loc==6 & hp_loc==3 ~ 3,
                     dist_loc==6 & hp_loc==4 ~ 2,
                     dist_loc==6 & hp_loc==5 ~ 1,
                     dist_loc==6 & hp_loc==6 ~ 0,
           ))

kids12 <- kids12 %>%
  mutate(relative_target=
           case_when(target_loc==1 & hp_loc==1 ~ 0,
                     target_loc==2 & hp_loc==1 ~ 1,
                     target_loc==3 & hp_loc==1 ~ 2,
                     target_loc==4 & hp_loc==1 ~ 3,
                     target_loc==5 & hp_loc==1 ~ 2,
                     target_loc==6 & hp_loc==1 ~ 1,
                     target_loc==1 & hp_loc==2 ~ 1,
                     target_loc==2 & hp_loc==2 ~ 0,
                     target_loc==3 & hp_loc==2 ~ 1,
                     target_loc==4 & hp_loc==2 ~ 2,
                     target_loc==5 & hp_loc==2 ~ 3,
                     target_loc==6 & hp_loc==2 ~ 2,
                     target_loc==1 & hp_loc==3 ~ 2,
                     target_loc==2 & hp_loc==3 ~ 1,
                     target_loc==3 & hp_loc==3 ~ 0,
                     target_loc==4 & hp_loc==3 ~ 1,
                     target_loc==5 & hp_loc==3 ~ 2,
                     target_loc==6 & hp_loc==3 ~ 3,
                     target_loc==1 & hp_loc==4 ~ 3,
                     target_loc==2 & hp_loc==4 ~ 2,
                     target_loc==3 & hp_loc==4 ~ 1,
                     target_loc==4 & hp_loc==4 ~ 0,
                     target_loc==5 & hp_loc==4 ~ 1,
                     target_loc==6 & hp_loc==4 ~ 2,
                     target_loc==1 & hp_loc==5 ~ 2,
                     target_loc==2 & hp_loc==5 ~ 3,
                     target_loc==3 & hp_loc==5 ~ 2,
                     target_loc==4 & hp_loc==5 ~ 1,
                     target_loc==5 & hp_loc==5 ~ 0,
                     target_loc==6 & hp_loc==5 ~ 1,
                     target_loc==1 & hp_loc==6 ~ 1,
                     target_loc==2 & hp_loc==6 ~ 2,
                     target_loc==3 & hp_loc==6 ~ 3,
                     target_loc==4 & hp_loc==6 ~ 2,
                     target_loc==5 & hp_loc==6 ~ 1,
                     target_loc==6 & hp_loc==6 ~ 0,))


kids12$relative_target[kids12$relative_target == 0] <- "high_prob"
kids12$relative_target[kids12$relative_target == 1] <- "lp_1"
kids12$relative_target[kids12$relative_target == 2] <- "lp_2"
kids12$relative_target[kids12$relative_target == 3] <- "lp_3"

kids12$relative_hp_dist[kids12$relative_hp_dist == 0] <- "high_prob"
kids12$relative_hp_dist[kids12$relative_hp_dist == 1] <- "lp_1"
kids12$relative_hp_dist[kids12$relative_hp_dist == 2] <- "lp_2"
kids12$relative_hp_dist[kids12$relative_hp_dist == 3] <- "lp_3"
kids12$relative_hp_dist[kids12$relative_hp_dist == 4] <- "absent"


#kids6

kids6 <- kids6 %>%
  mutate(relative_hp_dist=
           case_when(dist_loc==0 ~ 4,
                     dist_loc==1 & hp_loc==1 ~ 0,
                     dist_loc==1 & hp_loc==2 ~ 1,
                     dist_loc==1 & hp_loc==3 ~ 2,
                     dist_loc==1 & hp_loc==4 ~ 3,
                     dist_loc==1 & hp_loc==5 ~ 2,
                     dist_loc==1 & hp_loc==6 ~ 1,
                     dist_loc==2 & hp_loc==1 ~ 1,
                     dist_loc==2 & hp_loc==2 ~ 0,
                     dist_loc==2 & hp_loc==3 ~ 1,
                     dist_loc==2 & hp_loc==4 ~ 2,
                     dist_loc==2 & hp_loc==5 ~ 3,
                     dist_loc==2 & hp_loc==6 ~ 2,
                     dist_loc==3 & hp_loc==1 ~ 2,
                     dist_loc==3 & hp_loc==2 ~ 1,
                     dist_loc==3 & hp_loc==3 ~ 0,
                     dist_loc==3 & hp_loc==4 ~ 1,
                     dist_loc==3 & hp_loc==5 ~ 2,
                     dist_loc==3 & hp_loc==6 ~ 3,
                     dist_loc==4 & hp_loc==1 ~ 3,
                     dist_loc==4 & hp_loc==2 ~ 2,
                     dist_loc==4 & hp_loc==3 ~ 1,
                     dist_loc==4 & hp_loc==4 ~ 0,
                     dist_loc==4 & hp_loc==5 ~ 1,
                     dist_loc==4 & hp_loc==6 ~ 2,
                     dist_loc==5 & hp_loc==1 ~ 2,
                     dist_loc==5 & hp_loc==2 ~ 3,
                     dist_loc==5 & hp_loc==3 ~ 2,
                     dist_loc==5 & hp_loc==4 ~ 1,
                     dist_loc==5 & hp_loc==5 ~ 0,
                     dist_loc==5 & hp_loc==6 ~ 1,
                     dist_loc==6 & hp_loc==1 ~ 1,
                     dist_loc==6 & hp_loc==2 ~ 2,
                     dist_loc==6 & hp_loc==3 ~ 3,
                     dist_loc==6 & hp_loc==4 ~ 2,
                     dist_loc==6 & hp_loc==5 ~ 1,
                     dist_loc==6 & hp_loc==6 ~ 0,
           ))

kids6 <- kids6 %>%
  mutate(relative_target=
           case_when(target_loc==1 & hp_loc==1 ~ 0,
                     target_loc==2 & hp_loc==1 ~ 1,
                     target_loc==3 & hp_loc==1 ~ 2,
                     target_loc==4 & hp_loc==1 ~ 3,
                     target_loc==5 & hp_loc==1 ~ 2,
                     target_loc==6 & hp_loc==1 ~ 1,
                     target_loc==1 & hp_loc==2 ~ 1,
                     target_loc==2 & hp_loc==2 ~ 0,
                     target_loc==3 & hp_loc==2 ~ 1,
                     target_loc==4 & hp_loc==2 ~ 2,
                     target_loc==5 & hp_loc==2 ~ 3,
                     target_loc==6 & hp_loc==2 ~ 2,
                     target_loc==1 & hp_loc==3 ~ 2,
                     target_loc==2 & hp_loc==3 ~ 1,
                     target_loc==3 & hp_loc==3 ~ 0,
                     target_loc==4 & hp_loc==3 ~ 1,
                     target_loc==5 & hp_loc==3 ~ 2,
                     target_loc==6 & hp_loc==3 ~ 3,
                     target_loc==1 & hp_loc==4 ~ 3,
                     target_loc==2 & hp_loc==4 ~ 2,
                     target_loc==3 & hp_loc==4 ~ 1,
                     target_loc==4 & hp_loc==4 ~ 0,
                     target_loc==5 & hp_loc==4 ~ 1,
                     target_loc==6 & hp_loc==4 ~ 2,
                     target_loc==1 & hp_loc==5 ~ 2,
                     target_loc==2 & hp_loc==5 ~ 3,
                     target_loc==3 & hp_loc==5 ~ 2,
                     target_loc==4 & hp_loc==5 ~ 1,
                     target_loc==5 & hp_loc==5 ~ 0,
                     target_loc==6 & hp_loc==5 ~ 1,
                     target_loc==1 & hp_loc==6 ~ 1,
                     target_loc==2 & hp_loc==6 ~ 2,
                     target_loc==3 & hp_loc==6 ~ 3,
                     target_loc==4 & hp_loc==6 ~ 2,
                     target_loc==5 & hp_loc==6 ~ 1,
                     target_loc==6 & hp_loc==6 ~ 0,))


kids6$relative_target[kids6$relative_target == 0] <- "high_prob"
kids6$relative_target[kids6$relative_target == 1] <- "lp_1"
kids6$relative_target[kids6$relative_target == 2] <- "lp_2"
kids6$relative_target[kids6$relative_target == 3] <- "lp_3"

kids6$relative_hp_dist[kids6$relative_hp_dist == 0] <- "high_prob"
kids6$relative_hp_dist[kids6$relative_hp_dist == 1] <- "lp_1"
kids6$relative_hp_dist[kids6$relative_hp_dist == 2] <- "lp_2"
kids6$relative_hp_dist[kids6$relative_hp_dist == 3] <- "lp_3"
kids6$relative_hp_dist[kids6$relative_hp_dist == 4] <- "absent"
