#Visualizations ----

labels <- c("No", "Yes")  # to place on the legends

# Group Proportional Barplots
pdf(paste(p.figures, "Successes.pdf", sep = ""))
par(mfrow = c(1,2))  # rows, columns
for(n in c(26,28)){
  response.var <- d[,n]
  present <- grep("1|0", response.var)
  barplot(prop.table(table(d[present,n], d[present,30])[c(1:2),c(1:3)], margin = 2), main = colnames(d)[n], xlab = "Motivation", ylab = colnames(d)[n], ylim=c(0,1.0), cex.names = 0.7, beside=T, col=c("dimgray", "azure3"))
  legend("top",labels, fill=present,col = c('dimgray', 'azure3'))
} 
dev.off()

# PDF of Motivation Barplots
pdf(paste(p.figures, "Crowding.pdf", sep = ""))
par(mfrow = c(1,2))  # rows, columns
# BARPLOTS OF MOTIVATION AND CROWDING IN
for(n in c(21)){
  n<-21
  response.var <- d[,n]
  present <- grep("1|0", response.var)
  barplot(prop.table(table(d[present,n], d[present,30])[,-4], margin = 2), main = colnames(d)[n], xlab = "Motivation", ylab = colnames(d)[n], cex.names = 0.7, ylim=c(0,1.0), beside=T, col=c("dimgray", "azure3"))
  legend("topleft",labels, fill=present,col = c('dimgray', 'azure3'))
}

# BARPLOTS OF MOTIVATION AND CROWDING OUT
for(n in c(22)){
  response.var <- d[,n]
  present <- grep("1|0", response.var)
  barplot(prop.table(table(d[present,n], d[present,30])[,-4], margin = 2), main = colnames(d)[n], xlab = "Motivation", ylab = colnames(d)[n], cex.names = 0.7, ylim=c(0,1.0), beside=T, col=c("dimgray", "azure3"))
  legend("topleft",labels, fill=present, col = c('dimgray', 'azure3'))
}
dev.off()