# Running the intercoder Reliability Analysis for JA & CT Dec 14th 2017 

d <- read.csv(paste(p.rawdata,"IntercoderReliability.csv", sep = ""), stringsAsFactors = FALSE)
d[is.na(d)]<-""  # this makes NA's into blanks since they will later be changed

# there should only be 15 rows of information
d<-d[c(1:15), ] 

# Lets make a vector to place all of the values into 
kappa.scores <- rep(NA, 10)


# for Intrinsic Motivation
# first change all NA's to 0's since a blank implies it wasn't present. We need comparable categories.
# the kappa analysis is unable to run NA's and thus gives values such as NaN
intrin <- d[,c(1:2)]

for(i in 1:nrow(intrin)){
  if(is.na(intrin[i,1] == TRUE)) {intrin[i,1] <- 0}
  if(is.na(intrin[i,2] == TRUE)) {intrin[i,2] <- 0}
}
kap.intrin <- kappa2(intrin)
kappa.scores[1] <- kap.intrin$value  # 0.324, gives the value of kappa test

# for extrinsic motivation, applied to a more general test structure
col.ja <- 3
col.ct <- 4
indic <- d[,c(col.ja,col.ct)]

for(i in 1:nrow(indic)){
  if(is.na(indic[i,1] == TRUE)) {indic[i,1] <- 0}
  if(is.na(indic[i,2] == TRUE)) {indic[i,2] <- 0}
}
kap.indic <- kappa2(indic)
kappa.scores[2] <- kap.indic$value  # 0.609

# for CrowdingIn, Note: NA's have to be an extra category (not 0), since blank doesn't equal 0 in this instance
head(d)
col.ja <- 11
col.ct <- 9
indic <- d[,c(col.ja,col.ct)]

for(i in 1:nrow(indic)){
  if(is.na(indic[i,1] == TRUE)) {indic[i,1] <- 2}
  if(is.na(indic[i,2] == TRUE)) {indic[i,2] <- 2}
}
kap.indic <- kappa2(indic)
kappa.scores[3] <- kap.indic$value  # 0.375

# for CrowdingIn, Note: NA's have to be an extra category (not 0), since blank doesn't equal 0 in this instance
colnames(d)
col.ja <- 12
col.ct <- 10
indic <- d[,c(col.ja,col.ct)]

for(i in 1:nrow(indic)){
  if(is.na(indic[i,1] == TRUE)) {indic[i,1] <- 2}
  if(is.na(indic[i,2] == TRUE)) {indic[i,2] <- 2}
}
kap.indic <- kappa2(indic)
kappa.scores[4] <- kap.indic$value  # 0.589

# for Environmental Success (Env_), Note: NA's have to be an extra category (not 0), since blank doesn't equal 0 in this instance
colnames(d)
col.ja <- 19
col.ct <- 13
indic <- d[,c(col.ja,col.ct)]  # ensure to look at indic, to make sure the correct columns are being observed

for(i in 1:nrow(indic)){
  if(is.na(indic[i,1] == TRUE)) {indic[i,1] <- 2}
  if(is.na(indic[i,2] == TRUE)) {indic[i,2] <- 2}
}
kap.indic <- kappa2(indic)
kap.indic$value  # 1.000 reads NaN, but this is due to 100% agreement
kappa.scores[5] <- 1

# for Economic Success(Econ_), Note: NA's have to be an extra category (not 0), since blank doesn't equal 0 in this instance
colnames(d)
col.ja <- 21
col.ct <- 15
indic <- d[,c(col.ja,col.ct)]

for(i in 1:nrow(indic)){
  if(is.na(indic[i,1] == TRUE)) {indic[i,1] <- 2}
  if(is.na(indic[i,2] == TRUE)) {indic[i,2] <- 2}
}
kap.indic <- kappa2(indic)  # I am confused, since 10 agree out of 15, thus 66% agreement, and chance would be 33%, thus it is way better than chance. 
# maybe I just need to do more research into how it is actually calculated
kappa.scores[6] <- kap.indic$value  # 0.390

# for Social Success (Soc_), Note: NA's have to be an extra category (not 0), since blank doesn't equal 0 in this instance
colnames(d)
col.ja <- 23
col.ct <- 17
indic <- d[,c(col.ja,col.ct)]  # ensure to look at indic, to make sure the correct columns are being observed

for(i in 1:nrow(indic)){
  if(is.na(indic[i,1] == TRUE)) {indic[i,1] <- 2}
  if(is.na(indic[i,2] == TRUE)) {indic[i,2] <- 2}
}
kap.indic <- kappa2(indic)
kappa.scores[7] <- kap.indic$value  # 3.545

# for Environmental Success Level (Env_  _level), Note: NA's have to be an extra category (not 0), since blank doesn't equal 0 in this instance
# this should be weighted, since low-med, is less than low-high, yet this can be done at a later time
colnames(d)
col.ja <- 20
col.ct <- 14
indic <- d[,c(col.ja,col.ct)]  # ensure to look at indic, to make sure the correct columns are being observed

for(i in 1:nrow(indic)){
  if(is.na(indic[i,1] == TRUE)) {indic[i,1] <- 2}
  if(is.na(indic[i,2] == TRUE)) {indic[i,2] <- 2}
}
kap.indic <- kappa2(indic)
kappa.scores[8] <- kap.indic$value  # 0.370

# for Econ Success Level (Econ_  _level), Note: NA's have to be an extra category (not 0), since blank doesn't equal 0 in this instance
colnames(d)
col.ja <- 22
col.ct <- 16
indic <- d[,c(col.ja,col.ct)]  # ensure to look at indic, to make sure the correct columns are being observed

# some rows are blank, and thus cannot be compared, this agreement is taken care of in initial agreemnt of success observation
for(i in 1:nrow(indic)){
  if(indic[i,1] == "") {indic[i,1] <- NA}
  if(indic[i,2] == "") {indic[i,2] <- NA}
}
kap.indic <- kappa2(indic)
kappa.scores[9] <- kap.indic$value  # 0, agreement is worse than zero

# for Social Success Level (Soc_  _level), Note: NA's have to be an extra category (not 0), since blank doesn't equal 0 in this instance
colnames(d)
col.ja <- 24
col.ct <- 18
indic <- d[,c(col.ja,col.ct)]  # ensure to look at indic, to make sure the correct columns are being observed

# some rows are blank, and thus cannot be compared, this agreement is taken care of in initial agreemnt of success observation
# we need to makes these blanks into NAs so they are not considered 
for(i in 1:nrow(indic)){
  if(indic[i,1] == "") {indic[i,1] <- NA}
  if(indic[i,2] == "") {indic[i,2] <- NA}
}
kap.indic <- kappa2(indic)
kappa.scores[10] <- kap.indic$value  # 0.429


# finding the mean of the kappa scores
mean(kappa.scores)  # 0.444
sd(kappa.scores)  # 0.256

# mean of the kappa scores without the Success Outcomes
mean(kappa.scores[1:7])  # 0.520 
sd(kappa.scores[1:7])  # 0.240
