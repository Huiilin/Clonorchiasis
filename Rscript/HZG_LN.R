wd <- getwd()
setwd(wd)

options(stringsAsFactors = F)
options(scipen = 200)
library(tidyverse)

## read in data
datLN <- read.csv(file = "华支睾吸虫病_辽宁/华支睾吸虫-辽宁.csv",
                  header = T,check.names = F)
names(datLN)

colnames(datLN) <- gsub("[[:digit:]]","",colnames(datLN))
colnames(datLN) <- gsub("[.]","",colnames(datLN))

## change colnames
colnames(datLN)[13:17] <- paste("从哪些途径知道",colnames(datLN)[13:17],sep = "-")
colnames(datLN)[c(18,22,26)] <- paste("培训机构",
                                      colnames(datLN)[c(18,22,26)],sep = "-")
colnames(datLN)[19:21] <- paste("卫生健康委",colnames(datLN)[19:21],sep = "-")
colnames(datLN)[23:25] <- paste("疾控中心",colnames(datLN)[23:25],sep = "-")
colnames(datLN)[27:30] <- paste("医院",colnames(datLN)[27:30],sep = "-")
colnames(datLN)[31] <- paste("培训机构",colnames(datLN)[31],sep = "-")
colnames(datLN)[33:38] <- paste("如使用，什么情况下使用",
                                colnames(datLN)[33:38],sep = "-")
colnames(datLN)[39:42] <- paste("如未使用，原因",colnames(datLN)[39:42],sep = "-")
colnames(datLN)[45:50] <- paste("贵单位宣贯方式",
                                colnames(datLN)[45:50],sep = "-")
colnames(datLN)[51:58] <- paste("您更喜欢宣贯方式",
                                colnames(datLN)[51:58],sep = "-")
colnames(datLN)[59:63] <- paste("贵单位华支睾吸虫检测手段",
                                colnames(datLN)[59:63],sep = "-")
colnames(datLN)[64:69] <- paste("诊断依据",colnames(datLN)[64:69],sep = "-")
colnames(datLN)[71:75] <- paste("混淆疾病",colnames(datLN)[71:75],sep = "-")
colnames(datLN)[78:83] <- paste("影响因素",colnames(datLN)[78:83],sep = "-")

str(datLN)

## cut Age
str(datLN$年龄)
max(datLN$年龄,na.rm = T)
min(datLN$年龄,na.rm = T)
table(is.na(datLN$年龄))

datLN$Age <- cut(datLN$年龄,breaks = c(10,20,30,40,50,60),right = T)
table(datLN$Age)

## cut workingYears
str(datLN$年限)
max(datLN$年限,na.rm = T)
min(datLN$年限,na.rm = T)

datLN$workYears <- cut(datLN$年限,breaks = c(0,10,20,30,40),right = T)
table(datLN$workYears)

## frequency 
m = c(2,4:9,11,12,76,77)

for (i in m){
  datLN[,i] <- as.factor(datLN[,i])
  a = table(datLN[,i]) %>% as.matrix
  b = t(a)
  rownames(b) <- colnames(datLN)[i]
  print(b)
}

### Q2.1
for (i in 13:17){
  datLN[,i] <- as.factor(datLN[,i])
  a = table(datLN[,i]) %>% as.matrix
  b = t(a)
  rownames(b) <- colnames(datLN)[i]
  print(b)
}

### Q3.1
for (i in 33:38){
  datLN[,i] <- as.factor(datLN[,i])
  a = table(datLN[,i]) %>% as.matrix
  b = t(a)
  rownames(b) <- colnames(datLN)[i]
  print(b)
}

### Q3.2
for (i in 39:42){
  datLN[,i] <- as.factor(datLN[,i])
  a = table(datLN[,i]) %>% as.matrix
  b = t(a)
  rownames(b) <- colnames(datLN)[i]
  print(b)
}

### Q4.1
for (i in 45:50){
  datLN[,i] <- as.factor(datLN[,i])
  a = table(datLN[,i]) %>% as.matrix
  b = t(a)
  rownames(b) <- colnames(datLN)[i]
  print(b)
}

### Q4.2
for (i in 51:58){
  datLN[,i] <- as.factor(datLN[,i])
  a = table(datLN[,i]) %>% as.matrix
  b = t(a)
  rownames(b) <- colnames(datLN)[i]
  print(b)
}

### Q9
for (i in 78:83){
  datLN[,i] <- as.factor(datLN[,i])
  a = table(datLN[,i]) %>% as.matrix
  b = t(a)
  rownames(b) <- colnames(datLN)[i]
  print(b)
}

## chisquare-test
m = c(2,4:9,11,84:85)

### with Q2
for (i in m){
  datLN[,i] <- as.factor(datLN[,i])
  datLN[,12] <- as.factor(datLN[,12])
  a = fisher.test(datLN[,i],datLN[,12])
  b = paste(colnames(datLN)[i],colnames(datLN)[12],sep = " & ")
  b1 = paste(b,a$p.value,sep = ": ")
  print(b1)
}

### with Q6
m = nrow(datLN)

#### 6.1
a = which(names(datLN) ==  "诊断依据-流行病学史" )
b = which(names(datLN) == "诊断依据-分子生物学检测")

for (i in 1:m) {
  if(all(datLN[i,a:b] == "TRUE")) {
    datLN$sixOne[i] <- 1
  }else {datLN$sixOne[i] <- 0}
}
table(datLN$sixOne)

#### 6.2
for (i in 1:m) {
  ifelse(datLN[i,70] == 1,datLN$sixTwo[i] <- 1, datLN$sixTwo[i] <- 0)
}
table(datLN$sixTwo)

#### 6.3
a = which(names(datLN) ==  "混淆疾病-病毒性肝炎" )
b = which(names(datLN) == "混淆疾病-片形吸虫病")

for (i in 1:m) {
  if (all(datLN[i,a:b] == "TRUE")) {
    datLN$sixThree[i] <- 1
  } else {datLN$sixThree[i] <- 0}
}
table(datLN$sixThree)

sixAll <- data.frame(datLN$sixOne,datLN$sixTwo,datLN$sixThree,check.names = F)

#### combine
for (i in 1:m) {
  if (rowSums(sixAll[i,1:3]) == 3) {
    sixAll$All[i] <- 3
  } else if (rowSums(sixAll[i,1:3]) == 2) {
    sixAll$All[i] <- 2
  } else if (rowSums(sixAll[i,1:3]) == 1) {
    sixAll$All[i] <- 1
  } else {sixAll$All[i] <- 0}
}

###test
m = c(2,4:9,11,84:85)

for (i in m){
  aa = fisher.test(datLN[,i],sixAll$All,simulate.p.value = T)
  b = paste(colnames(datLN)[i],"Q6",sep = " & ")
  b1 = paste(b,aa$p.value,sep = ": ")
  print(b1)
}


### with Q3
rm(a,b,b1,i,m,aa)
a = which(names(datLN) ==  "是否使用规范及频率" )

m = c(2,4:9,11,84:85)

for (i in m){
  datLN[,a] <- as.factor(datLN[,a])
  aa = fisher.test(datLN[,i],datLN[,a])
  b = paste(colnames(datLN)[i],colnames(datLN)[a],sep = " & ")
  b1 = paste(b,aa$p.value,sep = ": ")
  print(b1)
}

## Q4 with employerType
rm(a,b,b1,i,m,aa)
a = which(names(datLN) ==  "标准实施后，参加培训次数" )
a1 = which(names(datLN) ==  "贵单位举办培训次数")

fisher.test(datLN[,4],datLN[,a],simulate.p.value = T)
fisher.test(datLN[,4],datLN[,a1],simulate.p.value = T)

## Q4.1 with employerType
rm(a,b,b1,i,m,aa)
Q41 <- matrix(0,4,6)
colnames(Q41) <- c("Wechat","Website","Folding","PromotionalPanel",
                  "DataCompilation","Else")
m = ncol(Q41)

a = which(names(datLN) ==  "贵单位宣贯方式-微信（QQ)" )
a1 = which(names(datLN) ==  "其他")

n = 0
for (i in a:a1) {
  n = n + 1
  dd = table(datLN[,4],datLN[,i]) %>% as.matrix
  b = t(dd[,"TRUE"])
  Q41[,n] <- b
}

fisher.test(Q41,simulate.p.value=TRUE)


## Q4.2 with Age
rm(a,b,b1,i,m,aa,a1,dd,n)

Q42 <- matrix(0,5,8)
rownames(Q42) <- levels(datLN$Age)
colnames(Q42) <- c("lecture","brochure","wechat","DataComp","vedio",
                  "PromoChart","ContinuingEdu","Else")

m = ncol(Q42)

a = which(names(datLN) ==  "您更喜欢宣贯方式-专题讲座" )
a1 = which(names(datLN) ==  "您更喜欢宣贯方式-其他")

n = 0
for (i in a:a1) {
  n = n + 1
  dd = table(datLN$Age,datLN[,i]) %>% as.matrix
  b = t(dd[,"TRUE"])
  Q42[,n] <- b
}
fisher.test(Q42,simulate.p.value=TRUE)


## Q5 with employerType
rm(a,b,b1,i,m,aa,a1,dd,n)

Q5 <- matrix(0,4,5)
colnames(Q5) <- c("JiaTeng","ELISA","LiXin","LaXian","Else")

a = which(names(datLN) ==  "贵单位华支睾吸虫检测手段-改良加藤厚涂片" )
a1 = which(names(datLN) ==  "贵单位华支睾吸虫检测手段-其他")

n = 0
for (i in a:a1) {
  n = n + 1
  datLN[,i] <- as.factor(datLN[,i])
  dd = table(datLN$单位类别,datLN[,i]) %>% as.matrix
  b = t(dd[,"TRUE"])
  Q5[,n] <- b
}

fisher.test(Q5,simulate.p.value=TRUE)

## Q5 with workYears
rm(a,b,b1,i,m,aa,a1,dd,n)

Q5w <- matrix(0,4,5)
rownames(Q5w) <- levels(datLN$workYears)
colnames(Q5w) <- c("JiaTeng","ELISA","LiXin","LaXian","Else")

n = 0
for (i in a:a1) {
  n = n + 1
  dd = table(datLN$workYears,datLN[,i]) %>% as.matrix
  b = t(dd[,"TRUE"])
  Q5w[,n] <- b
}

fisher.test(Q5w,simulate.p.value=TRUE)















































