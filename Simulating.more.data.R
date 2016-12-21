# we are going to simulate more data----
# NEED TO FIX THE SEED & save intermediate data alterations
set.seed(42)
d.sim <- d

# Lets start by defining "" as zero's in choice based variables (PES type, Payments, etc.)
d.sim.bin <- d.sim[4:18]
d.sim.bin[d.sim.bin != "1"] <- "0"
d.sim[4:18] <- d.sim.bin

# create extra rows to place data into, by making a matrix to be later combined
mat <- matrix(ncol=ncol(d), nrow=(100-nrow(d))) 

# determining the probablity of x outcome
#table(d.sim[,4])  # biodiversity conservation
#prob.0 <- (table(d.sim[,4])[1] / length(d.sim[,4]))*10  # prob of 0
#prob.1 <- (table(d.sim[,4])[2] / length(d.sim[,4]))*10  # prob of 1

# choosing a random number, and assigning it based on ^ prob
# for (i in 1:(100-nrow(d))){
#  ran <- sample(1:10, 1)
#  if(ran < prob.0){
#    mat[i,4] <- "0"
#  }else (mat[i,4] <- "1")
#}
#mat[,4]

# now make a super loop to do this for columns 4:18
for (n in 4:18){
  prob.0 <- (table(d.sim[,n])[1] / length(d.sim[,n]))*10  # prob of 0
  prob.1 <- (table(d.sim[,n])[2] / length(d.sim[,n]))*10  # prob of 1
  for (i in 1:(100-nrow(d))){
    ran <- sample(1:10, 1)
    if(ran < prob.0){
      mat[i,n] <- "0"
    }else (mat[i,n] <- "1")
  }
}

# now lets randomize the remaining columns
# column 1 & 23 don't need to be randomized

# randomizing columns 19 & 20, intrinsic and extrinsic motivation. we need the blanks to be left in, so that we can make 
# 4 options in the future (intrin, extrin, both, unclear)
for(n in c(19,20)){
  for (i in 1:(100-nrow(d))){
    ran <- sample(d[,n], 1, replace = T)
    mat[i,n] <- ran
  }
}
#randomizing column 2, project start date
# st.date <- as.numeric(d$Project.Start.Date)
# hist(st.date)
# mean.st.date <- mean(st.date)
# sd.st.date <- sd(st.date)
# mat[,2] <- rnorm(100-nrow(d), mean.st.date, sd.st.date) < 2016
# figure out a way to not get values greater than 2016

# Lets scrap the normal distribution and pull random integers
for (i in 1:(100-nrow(d))){
  ran <- sample(1991:2016, 1)  #1991:2016 determined by the range of pre-existed.
  mat[i,2] <- ran
}
# THIS ISN'T THE MOST REPRESENTATIVE, BUT IT IS EASIER, AND I NEED TO BE QUICK

# simulating column 3, participation level
for (i in 1:(100-nrow(d))){
    ran <- sample(d[,3], 1, replace = T)
    mat[i,3] <- ran
}

# simulating columns 21, 22, Crowding-in and crowding-out, and success (except for 24, which has 100% success observed)
# blanks are significant since they indicate inadequate data which actually occurs
# NOTE: probability function only work if all options occur in data ("", "0", "1")

for (n in c(21,22,26,28)){
  prob. <- (table(d[,n])[1] / length(d.sim[,n]))*10  # prob of ""
  prob.0 <- (table(d[,n])[2] / length(d.sim[,n]))*10  # prob of 0
  prob.1 <- (table(d[,n])[3] / length(d.sim[,n]))*10  # prob of 1
  for (i in 1:(100-nrow(d))){
    ran <- sample(1:10, 1, replace=TRUE)
    if(ran > prob. & ran < (prob. + prob.0)){
      mat[i,n] <- "0"
    }else (mat[i,n] <- "1")
    if(ran < prob.){
      mat[i,n] <- ""
    }
  }
}

# simulating data for Enviro success, c 24, which is 100%
for (i in 1:(100-nrow(d))){
  mat[i,24] <- "1"
}

# simulating columns 25, 27, 29, 
# I AM ASSUMING THAT ROWS OF SUCCESS LVL AND SUCCESS ARE NO LONGER INTERDEPENDENT 

# blanks are significant since they indicate inadequate data which actually occurs
# colnames(d)
# for (n in c(25,27,29){
#  prob.none <- (length(grep("none",d[,n])) / length(d.sim[,n]))*10  # prob of "none"
#  prob.low <- (length(grep("low",d[,n])) / length(d.sim[,n]))*10  # prob of "low"
#  prob.med <- (length(grep("med",d[,n])) / length(d.sim[,n]))*10  # prob of "med"
#  prob.high <- (length(grep("high",d[,n])) / length(d.sim[,n]))*10  # prob of "high"
#  prob. <- 10-(prob.high+prob.low+prob.med+prob.none)


# attempting an alternate method based on bootstrapping code
# THIS IS SOOOOOOO MUCH BETTER

for(n in c(25,27,29)){
  for (i in 1:(100-nrow(d))){
    ran <- sample(d[,n], 1, replace = T)
    mat[i,n] <- ran
  }
}

mat <- as.data.frame(mat)
colnames(mat) <- colnames(d)
d.comp <- rbind(d.sim, mat)

# lets save this in the data folder
# write.csv(happy.race.table, paste(p.analysis, "Happy.Race.Table.csv", sep = ""))
