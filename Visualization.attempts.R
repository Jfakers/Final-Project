# re-structuring the data from wide to long.. ----
# this will be challenging, since I currently have up to five columns that are serving as one response to a questions
# i.e. I want PES type = "Biodiversity Conservation" "Watershed Services" "Landscape Beauty" "Carbon Sequestration" "Agro-Forestry"                                                                    
# and so I will go about this by teaming up article number with one of these variables.
# an additional challenge is that each explanatory variable that I have, often has more than one response

# alternatively, maybe I do not need to do it this way.
# potentially I can look at correlations based off of my current set-up (i.e. if "biodiversity conservation" column = 1, how does success line-up)
# how do 1's vs blanks currently appear

# length(d$Biodiversity.Conservation)  #15, and is a series of "" & "1"
# mosaicplot(table(d$Biodiversity.Conservation == "1", d$Economic.Success != ""))



# PSEUDO-REPLICATION DATA VISUALIZATIONS ---- 

d<-d.comp  #making d = to the simulated data frame

# LOOKING SOLELY AT SUCCESSES (PRESENT) AND NOT PRESENT. 
# isolating when a variable is present (i.e. it equals 1)
explan.var <- d$Biodiversity.Conservation
# finding which rows contain "1"
present <- grep("1", explan.var)
barplot(table(d$Biodiversity.Conservation[present], d$Economic.Success[present]))
# NEED TO GET A WAY TO REMOVE THE "" FROM MY FINAL DATA.......

# could I do a glm on this?


# LETS TRY AND BUILD A LOOP FOR THIS
# first, identify the variables (columns that are measured in either presence of not)
d2 <- d[,c(4:20)] # excluding crowding in and out since they are measured in not occuring as well.
colnames(d)
d2 <- d[,c(4:8)] # focusing on PES type (for now)
par(mfrow = c(2,3))  # 2 rows, 3 columns
for(i in 1:length(d2)){
  explan.var <- d2[,i]
  present <- grep("1", explan.var)
  barplot(table(d2[present,i], d$Economic.Success[present]), main = colnames(d2)[i])
}

# plot for social success level, focusing on PES type
d2 <- d[,c(4:8)] # focusing on PES type (for now)
par(mfrow = c(2,3))  # 2 rows, 3 columns
for(i in 1:length(d2)){
  explan.var <- d2[,i]
  present <- grep("1", explan.var)
  barplot(table(d2[present,i], d$soc.suc.lvl[present]), main = colnames(d2)[i])
}
unique(d$soc.suc.lvl)

# LOOKING AT SUCCESSES (PRESENT) AND NOT PRESENT, MOSAIC PLOT, CHI-SQUARED STYLE.

# LETS TRY AND BUILD A LOOP FOR THIS
# first, identify the variables, and denote not present with 0

d2 <- d[,c(4:8)] # focusing on PES type (for now)
d2[d2 == ""] <- "0"  # changing all blanks to zeros

par(mfrow = c(2,3))  # 2 rows, 3 columns
for(i in 1:length(d2)){
  response.var <- d$Economic.Success
  present <- grep("1|0", response.var)
  mosaicplot(table(d2[present,i], d$Economic.Success[present]), main = colnames(d2)[i], xlab = colnames(d2)[i], ylab = "Economic.Success")
}


# lets make some graphics ----
# Lets do a barplot visualization of this for Economic Success

response.var <- d$Economic.Success
present <- grep("1|0", response.var)
barplot(table(d$Economic.Success[present], d[present,30]), main = colnames(d2)[i], xlab = colnames(d)[30], ylab = "Economic.Success", legend.text = T)
table(d$Economic.Success[present], d[present,30])

# Lets make a visualation for all three successs
colnames(d)
par(mfrow = c(1,3))  # rows, columns
for(n in c(24,26,28)){
  response.var <- d[,n]
  present <- grep("1|0", response.var)
  barplot(table(d[present,n], d[present,30]), main = colnames(d)[n], xlab = colnames(d)[30], ylab = colnames(d)[n], legend.text = T, cex.names = 0.7)
}
# Lets do grouped rather than stacked barplot
par(mfrow = c(1,3))  # rows, columns
for(n in c(24,26,28)){
  response.var <- d[,n]
  present <- grep("1|0", response.var)
  barplot(table(d[present,n], d[present,30]), main = colnames(d)[n], xlab = colnames(d)[30], ylab = colnames(d)[n], legend.text = T, cex.names = 0.7, beside=T)
}

# Given that n changes, lets do it in proportion.
# subtitute table for prop.table and specify margin = 2 for column based proportions
par(mfrow = c(1,3))  # rows, columns
for(n in c(24,26,28)){
  response.var <- d[,n]
  present <- grep("1|0", response.var)
  barplot(prop.table(table(d[present,n], d[present,30]), margin = 2), main = colnames(d)[n], xlab = colnames(d)[30], ylab = colnames(d)[n], legend.text = T, cex.names = 0.7, beside=T)
}


# FAIRLY GOOD, YET IT WOULD ALSO BE HELPFUL TO LOOK AT THE SUCCESS LEVELS
# lets do it proportionally since that looks better
# NOTE: THE RESULTS WILL NOT LINE UP WITH THOSE ABOVE SINCE THEY WERE SIMULATED DIFFERENTLY. BUT THIS WILL NOT BE THE CASE FOR THE REAL DATA.
for(n in c(25,27,29)){
  response.var <- d[,n]
  present <- grep("none|low|med|high", response.var)
  barplot(prop.table(table(d[present,n], d[present,30]), margin = 2), main = colnames(d)[n], xlab = colnames(d)[30], ylab = colnames(d)[n], legend.text = T, cex.names = 0.7, beside=T)
}

# BARPLOTS OF MOTIVATION AND CROWDING
dev.off()
for(n in c(31)){
  response.var <- d[,n]
  present <- grep("in|out|both", response.var)
  barplot(prop.table(table(d[present,n], d[present,30]), margin = 2), main = colnames(d)[n], xlab = colnames(d)[30], ylab = colnames(d)[n], legend.text = T, cex.names = 0.7, beside=T)
}

# BARPLOTS OF MOTIVATION AND CROWDING IN
dev.off()
for(n in c(21)){
  n<-21
  response.var <- d[,n]
  present <- grep("1|0", response.var)
  barplot(prop.table(table(d[present,n], d[present,30]), margin = 2), main = colnames(d)[n], xlab = colnames(d)[30], ylab = colnames(d)[n], legend.text = T, cex.names = 0.7, beside=T)
}

# BARPLOTS OF MOTIVATION AND CROWDING OUT
dev.off()
colnames(d)
d[,22]
for(n in c(22)){
  response.var <- d[,n]
  present <- grep("1|0", response.var)
  barplot(prop.table(table(d[present,n], d[present,30]), margin = 2), main = colnames(d)[n], xlab = colnames(d)[30], ylab = colnames(d)[n], legend.text = T, cex.names = 0.7, beside=T)
}

