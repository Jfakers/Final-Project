# Paths and raw data changes ----

# assign working directory to an object
wkdr <- getwd()

#list of folders we want
folders <- c("raw data", "analysis", "figures", "scripts", "data")

#create these folders if we dont have them already
for(i in 1:length(folders)){
  if(file.exists(folders[i]) == "FALSE") {
    dir.create(folders[i])
  }
}

# create paths to the new folders in the working directory
p.rawdata <- paste(wkdr, "/raw data/", sep = "")
p.analysis <- paste(wkdr, "/analysis/", sep = "")
p.figures <- paste(wkdr, "/figures/", sep = "")
p.scripts <- paste(wkdr, "/scripts/", sep = "")
p.data <- paste(wkdr, "/data/", sep = "")


# read the data file we want into an object called data

data <- read.csv(paste(p.rawdata, "MyDataSheet.csv", sep = ""), stringsAsFactors = FALSE)

# Column names are broken up in a bad way... Since there was a main heading over top of column names,
# so colnames(data) -> x , x.1, x.2, etc. which isn't ideal.. 
# exchanging column names for cell names in row 1
colnames(data) <- data[1,]


# clean up all unwanted column names
d <- data[,-c(3:24,26,27,24,34:37,48,51:53,56,64:68)]
# omitted columns, data?, Article, Authors (4:11), MultipleCase, #Chosen, Continent/Region, Sub-Region, Scale... , National HDI... , Governance Voice Maybe, Governance Corruption Maybe, Project Description, 
# Ecosystem Type, Eco-Category, TBD, Who's incharge... , NA, NA, **Payment Notes**, Unclear, Incentive Likert ... , Comment on Decision, Comments on Decision, Measurement of Social Success, Success Notes, General Notes*, Coder, Questions
# NOTE: may include some of these columns into the future
# Taken out in second wring: YearPub., Journal., Country, Project name
colnames(d)
# renaming success measurement columns so that they are different
d[1,26] <- "enviro.suc.lvl"
d[1,28] <- "econ.suc.lvl"
d[1,30] <- "soc.suc.lvl"
# re-run colnames to change the data name
colnames(d) <- d[1,]

# replacing spaces with periods in the new titles
colnames(d) <- gsub(" ", ".", colnames(d))

# clean up all unwanted rows (i.e. do not have sufficient data)
d <- d[-grep("n", d[,1]),]

# remove first row, and column, since it is just the columns repeated
d <- d[-1,-1]
nrow(d)  # should be 15

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
  explan.var <- d[,i]
  present <- grep("1", explan.var)
  barplot(table(d2[present,i], d$Economic.Success[present]), )
}

# How to make a barplot of many things.. 
