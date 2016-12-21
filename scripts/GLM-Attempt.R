# Attempts at GLM ----
d <- read.csv(paste(p.data, "AggregateSimData.csv", sep = ""), strip.white = TRUE, stringsAsFactors = F)
d[is.na(d)]<-""  # this makes NA's into blanks since they will later be changed
d <- d[,-1]

# FILL IN BLANKS TO 0 FOR INTRINSIC AND EXTRINSIC, SINCE THESE DENOTE SOMETHING for these purposes
explan.var <- d$Intrinsic
explan.var[explan.var != "1"] <- "0"
d$Intrinsic <- explan.var

explan.var <- d$Extrinsic
explan.var[explan.var != "1"] <- "0"
d$Extrinsic <- explan.var


# for economic success, binary = logit link function
response.var <- as.numeric(d$Economic.Success)
# present <- grep("1|0", response.var) # appears to work even with the NAs
explan.var <- d$Intrinsic
explan.var[explan.var != "1"] <- "0"
glm1 <- glm(response.var ~ explan.var, na.action = na.omit, family=binomial(link="logit"))
summary(glm1)
glm1$aic

# USING AIC, SINCE WE HAVE A PROPORTIONALLY SMALL SAMPLES SIZE (~100), SINCE n/K < 40, (WAY LESS IN OUR STUDY)
# WE WILL USE THE SECOND ORDER AIC, 
# need Package ‘MuMIn’, and the function is AICc(), RAN ON FIRST MAIN.R
# models are chosen based upon having the lowers AIC scores

# Lets try setting up a FORWARD STEPWISE AIC of our variables 
# first, our variables of interest include column, colnames(d) d[,c(2:10,12:20) 11 only has 5 samples
# NOTE, I MAY NEED TO MAKE SOME DECISIONS REGARDING WHAT VARIABLES TO INCLUDE, BASED ON SAMPLES INCLUDED (I.E. <10 OF SO)

# so lets try to write it out 

# taken from interweb \/ & is loaded and referenced on Main.R
# stepAICc(glm(response.var~1), response.var ~ explan.var[1]+explan.var[2]+explan.var[3]+ explan.var[4]+explan.var[5]+explan.var[6]+explan.var[7]+explan.var[8]+explan.var[9]+explan.var[10]+explan.var[11]+explan.var[12]+explan.var[13]+explan.var[14]+explan.var[15]+explan.var[16]+explan.var[17]+explan.var[18]+explan.var[19],direction = "forward")
# formatnumber 2
response.var <- as.numeric(d$Economic.Success)
step.Econ.suc <- stepAICc(glm(response.var~1, na.action = na.omit, family=binomial(link="logit")), response.var ~ d$Project.Start.Date+d$Participation.pretty+d$Biodiversity.Conservation+d$Watershed.Services+d$Landscape.Beauty+d$Carbon.Sequestration+d$`Agro-Forestry`+d$`Area.Based.(land/resource.cap.by.land.unit)`+d$`Public.(state.payed)`+d$`Use-restricting.(reward.conservation.and.capping)`+d$`Asset-building.(restoration)`+d$Cash+d$Land.rights+d$`Infrastructure.(health.and.education.services)`+d$Individual+d$Communal+d$Intrinsic+d$Extrinsic, direction = "forward")
warnings()
Econ.suc.glm$call # glm(formula = response.var ~ d$Carbon.Sequestration + d$`Public.(state.payed)` +  d$`Private.(Buyers.pay.directly)` + d$`Area.Based.(land/resource.cap.by.land.unit)` + d$Biodiversity.Conservation + d$Extrinsic + d$Individual, family = binomial(link = "logit"), na.action = na.omit)
# we get 50 warnings that all say: glm.fit: fitted probabilities numerically 0 or 1 occurred...
# INTERNET SAYS THAT: "If you have a variable which perfectly separates zeroes and ones in target variable" THIS ERROR OCCURS.
# This is referred to as complete separation (or quasi-complete seperation)
# If we assume that this relationship is not true within the population, and not only in our sample, we could apply some form of penalized regressions, yet these are rather complicated so we're going to approach it differently.

# Analyzing our co-efficient estimates (for their size) can give some indication of samples that may demonstrate quasi, or complete seperation
# With the fixed seed, we did not have the warning pop up when running the 'called' model
# lets see the significance of this model ^
glm.Econ.suc <- glm(response.var ~ d$Carbon.Sequestration + d$`Public.(state.payed)` + d$`Area.Based.(land/resource.cap.by.land.unit)` + d$Biodiversity.Conservation + d$Extrinsic + d$Individual, family = binomial(link = "logit"), na.action = na.omit)
summary(glm.Econ.suc)  # p-value increases with the addition of the last variable, thus suggesting that it is an overfit. Given that none of these models were significant, I won't go through organizing them. 

# THE INTERNET SUGGESTS RUNNING logistf() as a way of combatting the complete or quasi-complete seperation
response.var <- as.numeric(d$Economic.Success)
step.Econ.suc <- stepAICc(glm(response.var~1, na.action = na.omit, family=binomial(link="logit")), response.var ~ d$Project.Start.Date+d$Participation.pretty+d$Biodiversity.Conservation+d$Watershed.Services+d$Landscape.Beauty+d$Carbon.Sequestration+d$`Agro-Forestry`+d$`Area.Based.(land/resource.cap.by.land.unit)`+d$`Public.(state.payed)`+d$`Use-restricting.(reward.conservation.and.capping)`+d$`Asset-building.(restoration)`+d$Cash+d$Land.rights+d$`Infrastructure.(health.and.education.services)`+d$Individual+d$Communal+d$Intrinsic+d$Extrinsic, direction = "forward")
#ISN'T WORKING WHEN stepAICc(logistf(res...     SO WE'LL JUST try to find the bad variable manually.
glm.Econ.suc <- glm(formula = response.var ~ d$Carbon.Sequestration + d$`Public.(state.payed)` + d$`Area.Based.(land/resource.cap.by.land.unit)` + 
                      d$Biodiversity.Conservation + d$Extrinsic + d$Individual, 
                    family = binomial(link = "logit"), na.action = na.omit)
summary(glm.Econ.suc)


response.var <- as.numeric(d$Social.Success)
step.Social.suc <- stepAICc(glm(response.var~1, na.action = na.omit, family=binomial(link="logit")), response.var ~ d$Project.Start.Date+d$Participation.pretty+d$Biodiversity.Conservation+d$Watershed.Services+d$Landscape.Beauty+d$Carbon.Sequestration+d$`Agro-Forestry`+d$`Area.Based.(land/resource.cap.by.land.unit)`+d$`Public.(state.payed)`+d$`Private.(Buyers.pay.directly)`+d$`Use-restricting.(reward.conservation.and.capping)`+d$`Asset-building.(restoration)`+d$Cash+d$Land.rights+d$`Infrastructure.(health.and.education.services)`+d$Individual+d$Communal+d$Intrinsic+d$Extrinsic, direction = "forward")
# still has 50 warnings

#  I HAVE RUN OUT OF TIME TO DEAL WITH THIS, AND WILL REVISIT IT WHEN IT COMES TO RUNNING THE TEST FOR THE ACTUAL DATA, OR WHILE I WORK ON IT. 
