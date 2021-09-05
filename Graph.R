library(ggplot2)
library(tidyverse)


### 1 #############
dat <- table(datZJ$AgeGroup) %>% as.data.frame
str(dat)

ggplot(dat,aes(Var1,Freq)) + geom_histogram(stat='identity',na.rm = T,binwidth = 10)


### 2 #############
ggplot(datZJ,aes(age)) + geom_histogram(na.rm = T, 
                                        breaks = c(10,20,30,40,50,60,70),
                                        closed = "right",
                                        fill = "steelblue",
                                        colour = "black") +
  labs(title = "Age Frequency of enterobiasis in ZheJiang") +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70)) + 
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60)) +
  theme(plot.title = element_text(hjust = 0.5,size = 15,face = "bold.italic",
                                  colour = "skyblue",lineheight = 1.2),
        axis.text = element_text(size = 11,face = "bold",vjust = 0.6),
        axis.text.x = element_text(angle = 45),
        axis.title.x = element_text(size = 12,face = "bold"),
        axis.title.y=element_text(size=12,face = "bold"))

### 3 ###########
ggplot(datZJ,aes(age)) + geom_histogram(na.rm = T, 
                                        breaks = c(10,20,30,40,50,60,70),
                                        closed = "right",
                                        fill = "steelblue",
                                        colour = "black") +
  labs(title = "Age Frequency of enterobiasis in ZheJiang") +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70)) + 
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60)) + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,size = 15,face = "bold.italic",
                                  colour = "black",lineheight = 1.2),
        axis.text = element_text(size = 11,face = "bold",vjust = 0.6),
        axis.text.x = element_text(angle = 45),
        axis.title.x = element_text(size = 12,face = "bold"),
        axis.title.y=element_text(size=12,face = "bold"))

ggsave(filename = "AgeFre.png",width = 6,height = 6)
















