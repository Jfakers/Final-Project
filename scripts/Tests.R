# TESTS, predominantly Chi-Squared for predominant variables of interest ----
d <- read.csv(paste(p.data, "AggregateSimData.csv", sep = ""), strip.white = TRUE, stringsAsFactors = F)
d[is.na(d)]<-""  # this makes NA's into blanks since they will later be changed
d <- d[,-1]

# NOTES: we are not able to do tests to determine whether there are other variables that can explain or results,


# Economic Success and Intrinsic Motivation
response.var <- d$Economic.Success
present <- grep("1|0", response.var)
econ.fisher.intrin <- fisher.test(table(d$Economic.Success[present], d[present,19]))  # p-value, 0.0578
# Economic Success and Extrinsic Motivation
econ.fisher.extrin <- fisher.test(table(d$Economic.Success[present], d[present,20]))  # p-value, 0.2655

# Social Success and Intrinsic Motivation
response.var <- d$Social.Success
present <- grep("1|0", response.var)
soc.fisher.intrin <- fisher.test(table(d$Social.Success[present], d[present,19]))  # p-value, 1
# Social Success and Extrinsic Motivation
soc.fisher.extrin <- fisher.test(table(d$Social.Success[present], d[present,20]))  # p-value, 0.7028


# for crowding in and Intrinsic motivation
response.var <- d[,21]
present <- grep("1|0", response.var)
c.in.fisher.intrin <- fisher.test(table(d[present,21], d[present,19]))  # p-val = 1
# for crowding in and extrinsic motivation
c.in.fisher.extrin <- fisher.test(table(d[present,21], d[present,20]))  # p-val = 0.5788

# for crowding out and Intrinsic motivation
response.var <- d[,22]
present <- grep("1|0", response.var)
c.out.fisher.intrin <- fisher.test(table(d[present,22], d[present,19]))  # p-val = 0.467
# for crowding out and extrinsic motivation
c.out.fisher.extrin <- fisher.test(table(d[present,22], d[present,20]))  # p-val = 0.4533

# creating a table to place values into
colnames(d)
Explanatory.Variable <- colnames(d)[c(19,20,19,20,19,20,19,20)]
Response.Variable <- colnames(d)[c(26,26,28,28,21,21,22,22)]
Sample.Size <- c(77,77,71,71,62,62,67,67)  #length of present, from each trial
Proportion.Occurance <- c(37,40,32,37,28,30,31,39)/Sample.Size
P.Val.Fisher.Test <- c(econ.fisher.intrin$p.value,econ.fisher.extrin$p.value,soc.fisher.intrin$p.value,soc.fisher.extrin$p.value,c.in.fisher.intrin$p.value,c.in.fisher.extrin$p.value,c.out.fisher.intrin$p.value,c.out.fisher.extrin$p.value)

Fisher.Table <- cbind(Explanatory.Variable,Response.Variable,Sample.Size,Proportion.Occurance,P.Val.Fisher.Test)

write.csv(Fisher.Table, paste(p.analysis, "Fisher.Table.csv", sep = ""))
