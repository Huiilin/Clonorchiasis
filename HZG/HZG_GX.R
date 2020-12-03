setwd("F:/YU")
datA <- read.csv("file",header = TRUE,check.names = FALSE,
                 na.strings = "")
str(datA)

colnames(datA) <- gsub("[[:digit:]]","",colnames(datA))
colnames(datA) <- gsub("[.]","",colnames(datA))

names(datA)

## find colnames position
which(names(datA) == "从哪些途径知道") #13
which(names(datA) == "培训机构") # 18
which(names(datA) == "是否使用规范及频率") # 32

which(names(datA) == "如使用，什么情况下使用？") # 33
which(names(datA) == "如未使用，原因") # 39
which(names(datA) == "标准实施后，参加培训次数") # 43

which(names(datA) == "贵单位宣贯方式") # 45
which(names(datA) == "您更喜欢宣贯方式") # 51

which(names(datA) == "贵单位华支睾吸虫检测手段") # 59
which(names(datA) == "诊断依据") # 64
which(names(datA) == "流行病学诊断依据") # 70

which(names(datA) == "混淆疾病") # 71
which(names(datA) == "是否适用") # 76

which(names(datA) == "影响因素") # 78

## combine colnames
colnames(datA)[13:17] <- paste(colnames(datA)[13],datA[1,c(13:17)],sep = "-")
colnames(datA)[c(18,22,26)] <- paste(colnames(datA)[18],datA[1,c(18,22,26)],sep = "-")
colnames(datA)[19:21] <- paste(datA[1,18],datA[1,c(19:21)],sep = "-")
colnames(datA)[22:25] <- paste(datA[1,22],datA[1,c(22:25)],sep = "-")
colnames(datA)[26:31] <- paste(datA[1,26],datA[1,c(26:31)],sep = "-")
colnames(datA)[33:38] <- paste(colnames(datA)[33],datA[1,c(33:38)],sep = "-")
colnames(datA)[39:42] <- paste(colnames(datA)[39],datA[1,c(39:42)],sep = "-")
colnames(datA)[45:50] <- paste(colnames(datA)[45],datA[1,c(45:50)],sep = "-")
colnames(datA)[51:58] <- paste(colnames(datA)[51],datA[1,c(51:58)],sep = "-")
colnames(datA)[59:63] <- paste(colnames(datA)[59],datA[1,c(59:63)],sep = "-")
colnames(datA)[64:69] <- paste(colnames(datA)[64],datA[1,c(64:69)],sep = "-")
colnames(datA)[71:75] <- paste(colnames(datA)[71],datA[1,c(71:75)],sep = "-")
colnames(datA)[78:83] <- paste(colnames(datA)[78],datA[1,c(78:83)],sep = "-")

## new data
datA <- datA[-1,]
str(datA)

## frequency
table(datA$性别)
datA$年龄 <- as.numeric(datA$年龄)
max(datA$年龄,na.rm = T)
table(is.na(datA$年龄))
datA$Age <- cut(datA$年龄,breaks = c(10,20,30,40,50,60),right = T)
table(datA$Age)

datA$单位类别 <- as.factor(datA$单位类别)
str(datA$单位类别)
table(is.na(datA$单位类别))
table(datA$单位类别)

table(datA$单位性质)
table(datA$职称)
table(datA$学历)
table(datA$您是否从事寄生虫病工作)
table(datA$职业是)

datA$年限 <- as.numeric(datA$年限)
max(datA$年限,na.rm = T)
min(datA$年限,na.rm = T)
datA$WorkingYears <- cut(datA$年限,
                         breaks = c(0,10,20,30,40,50,90),
                         right = T)
table(datA$WorkingYears)
table(is.na(datA$年限))

table(is.na(datA$工作内容))
table(datA$工作内容)

##chi-square test
str(datA$是否知道华支睾吸虫诊断标准)
str(datA$性别)
datA$是否知道华支睾吸虫诊断标准 <- as.factor(datA$是否知道华支睾吸虫诊断标准)
datA$性别 <- as.factor(datA$性别)

chisq.test(datA$是否知道华支睾吸虫诊断标准,datA$性别)
# chisq.test(datA$是否知道华支睾吸虫诊断标准,datA$Age)
fisher.test(datA$是否知道华支睾吸虫诊断标准,datA$Age)

fisher.test(datA$是否知道华支睾吸虫诊断标准,datA$单位类别)

fisher.test(datA$是否知道华支睾吸虫诊断标准,datA$单位性质)

fisher.test(datA$是否知道华支睾吸虫诊断标准,datA$职称)

fisher.test(datA$是否知道华支睾吸虫诊断标准,datA$学历)

chisq.test(datA$是否知道华支睾吸虫诊断标准,datA$您是否从事寄生虫病工作)

fisher.test(datA$是否知道华支睾吸虫诊断标准,datA$职业是)

fisher.test(datA$是否知道华支睾吸虫诊断标准,datA$WorkingYears)

datA$工作内容 <- as.factor(datA$工作内容)
fisher.test(datA$是否知道华支睾吸虫诊断标准,datA$工作内容)

## question 6
a = which(names(datA) ==  "诊断依据-流行病学史" )
b = which(names(datA) == "诊断依据-分子生物学检测")

m = nrow(datA)

for (i in 1:m) {
  if(all(datA[i,a:b] == "TRUE")) {
    datA$sixOne[i] <- 1
  }
  else {datA$sixOne[i] <- 0}
}


for (i in 1:m) {
  ifelse(datA[i,70] == 1,datA$sixTwo[i] <- 1, datA$sixTwo[i] <- 0)
}
table(datA$sixTwo)

for (i in 1:m) {
  if (all(datA[i,71:74] == "TRUE")) {
    datA$sixThree[i] <- 1
  } else {datA$sixThree[i] <- 0}
}
table(datA$sixThree)

sixAll <- data.frame(datA$sixOne,datA$sixTwo,datA$sixThree,check.names = F)


for (i in 1:m) {
  if (rowSums(sixAll[i,1:3]) == 3) {
    sixAll$All[i] <- 3
  } else if (rowSums(sixAll[i,1:3]) == 2) {
    sixAll$All[i] <- 2
  } else if (rowSums(sixAll[i,1:3]) == 1) {
    sixAll$All[i] <- 1
  } else {sixAll$All[i] <- 0}
}

##test
datA <- datA[,c(84:85,2,4:9,11,1,3,10,12:83,86:88)]
names(datA)

for (j in 1:10) {
  aa = fisher.test(datA[,j],sixAll$All)
  print(paste(names(datA[j]),aa$p.value))
}

for (j in 1:10) {
  aa = fisher.test(datA[,j],datA$是否知道华支睾吸虫诊断标准)
  print(paste(names(datA[j]),aa$p.value))
}

datA$是否使用规范及频率 <- as.factor(datA$是否使用规范及频率)

for (j in 1:10) {
  aa = fisher.test(datA[,j],datA$是否使用规范及频率)
  print(paste(names(datA[j]),aa$p.value))
}

datA$单位类别 <- as.factor(datA$单位类别)

## question 4
library(tidyverse)
Q4 <- matrix(0,4,6)
colnames(Q4) <- c("Wechat","Website","Folding","PromotionalPanel",
                  "DataCompilation","Else")
m = ncol(Q4)

n = 0
for (i in 47:52) {
  n = n + 1
  a = table(datA[,4],datA[,i]) %>% as.matrix
  b = t(a[,"TRUE"])
  Q4[,n] <- b
}

fisher.test(Q4,simulate.p.value=TRUE)


## question 4.2
Q5 <- matrix(0,5,8)
rownames(Q5) <- levels(datA$Age)
colnames(Q5) <- c("lecture","brochure","wechat","DataComp","vedio",
                  "PromoChart","ContinuingEdu","Else")

n = 0
for (i in 53:60) {
  n = n + 1
  a = table(datA$Age,datA[,i]) %>% as.matrix
  b = t(a[,"TRUE"])
  Q5[,n] <- b
}

fisher.test(Q5,simulate.p.value=TRUE)

## question 5
Q6 <- matrix(0,4,5)
colnames(Q6) <- c("JiaTeng","ELISA","LiXin","LaXian","Else")

n = 0
for (i in 61:65) {
  n = n + 1
  a = table(datA$单位类别,datA[,i]) %>% as.matrix
  b = t(a[,"TRUE"])
  Q6[,n] <- b
}
fisher.test(Q6,simulate.p.value=TRUE)

## question 5, workingYears
Q7 <- matrix(0,6,5)
rownames(Q7) <- levels(datA$WorkingYears)
colnames(Q7) <- c("JiaTeng","ELISA","LiXin","LaXian","Else")

n = 0
for (i in 61:65) {
  n = n + 1
  a = table(datA$WorkingYears,datA[,i]) %>% as.matrix
  b = t(a[,"TRUE"])
  Q7[,n] <- b
}
fisher.test(Q7,simulate.p.value=TRUE)

write.csv(Q4, file = "q4.csv",row.names = T)

## Q8
table(is.na(datA[,79]))
table(datA[,79])

## Q9
table(is.na(datA[,80]))
table(datA[,80])

write.csv(Q5, file = "q5.csv",row.names = T)
write.csv(Q6, file = "q6.csv",row.names = T)
write.csv(Q7, file = "q7.csv",row.names = T)
