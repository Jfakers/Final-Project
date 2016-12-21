# aggregating data columns---- 

d <- d.comp


# Lets look at at motivation data since that is the most important, for now, & it may be easier to deal with..
# NOTE: making it into an aggregate allows for the elimination of insufficient data

# aggregating motivation ----
intrin <- d[,19]
extrin <- d[,20]
# define a new vector to place meta motive decision into
motive <- rep(NA, 100)
for (i in 1:nrow(d)){
  if(intrin[i] == "" & extrin[i] == "")(motive[i] <- "notsuff")
  if(intrin[i] == "1" & extrin[i] == "")(motive[i] <- "intrin")
  if(intrin[i] == "" & extrin[i] == "1")(motive[i] <- "extrin")
  if(intrin[i] == "1" & extrin[i] == "1")(motive[i] <- "both")
}
# add motive into the d, dataframe
d<-cbind(d,motive)
# colnames(d)  = 30

# aggregating crowding in/out----
# lets make an aggregate for the crowding in/out outcome, c 21 & 22
colnames(d)
crowding <- rep(NA, 100)
intrin <- d[,21]  # just reusing the code for eaze
extrin <- d[,22]  # just reusing the code for eaze
for (i in 1:nrow(d)){
  if((intrin[i] == "0"|intrin[i] == "") & (extrin[i] == "0"|extrin[i] == ""))(crowding[i] <- "notsuff")
  if(intrin[i] == "1" & (extrin[i] == "0"|extrin[i] == ""))(crowding[i] <- "in")
  if((intrin[i] == "0"|intrin[i] == "") & extrin[i] == "1")(crowding[i] <- "out")
  if(intrin[i] == "1" & extrin[i] == "1")(crowding[i] <- "both")
}
d<-cbind(d,crowding)

# NOTE: SOME RESOLUTION IS LOST WITH THIS METHOD, SINCE IF BOTH IN/OUT AREN'T OBSERVED (I.E. = "0") OR DATA IS INSUFFICIENT, 
# IT IS STILL RECORDED AS INSUFFICIENT DATA

# aggregating PES type----  
# colnames(d)  # 4:8
# pes.type <- rep(NA, 100)
# bc <- d[,4]
# ws <- d[,5]
# lb <- d[,6]
# cs <- d[,7]
# af <- d[,8]

#for (i in 1:nrow(d)){
#  if(bc[i] == "1" & ws[i] == "1" & lb[i] == "1" & cs[i] == "1" & af[i] == "1")(pes.type[i] <- "notsuff")
#}
# THIS IS NOT POSSIBLE, SINCE WE HAVE 31 DIFFERENT POSSIBILITIES

# BEAN'S IDEA IS TO CONSIDER THINGS IN THE PSEUDOREPLICATION WAY, BUT REMOVE THINGS IN THE LATER TEST.
# WILL DISCUSS THIS MORE WITH THOR IN THE AM
# JUSTIFICATION: (FROM BEAN'S) SINCE WE ARE NOT VIEWING IT IN THE LONG FORMAT (AND THEREFORE HAVING MORE THAN 100 DATA POINTS), WE ARE FINE


# add motive into the d, dataframe
# d<-cbind(d,pes.type)
