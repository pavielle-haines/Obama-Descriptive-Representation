###################SECTION 1: INSTALL LIBRARIES (ONLY NEEDS TO BE BE RUN ONCE, BEFORE ANYTHING ELSE; DOES NOT NEED TO BE RUN EACH TIME THE FILE IS REOPENED)###################

# INSTALL REQUIRED LIBRARIES ---------------------------------

#This code was written using R 3.3.1

#This step ensures that all the libraries required to run the anlayses are installed to your R program. You only need to run this section of the code once.

install.packages("foreign")
install.packages("gmodels")
install.packages("plyr")
install.packages("MASS")
install.packages("aod")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("Zelig")
install.packages("ZeligChoice")
install.packages("coefplot")
install.packages("reshape2")
install.packages("gplots")
install.packages("survey")
install.packages("xlsx")
install.packages("RColorBrewer")
install.packages("irr")
install.packages("clusterSEs")
install.packages("dplyr")
install.packages("tibble")
install.packages("PRESS")
install.packages("wnominate") #Select "n" for compliation source
install.packages("devtools")
devtools::install_github("voteview/Rvoteview")
install.packages("pscl")
install.packages("gtools")
install.packages("plotrix")
install.packages("gplots")




###################SECTION 2: CALL LIBRARIES AND DATA (MUST BE RUN EACH TIME THE FILE IS OPENED, BEFORE SECTIONS 3, 4, AND 5)###################

# CALL REQUIRED LIBRARIES -------------------------------------

library(foreign) #Call libraries
library(gmodels)
library(plyr)
library(MASS)
library(aod)
library(Hmisc)
library(ggplot2)
library(Zelig)
library(ZeligChoice)
library(coefplot)
library(reshape2)
library(gplots)
library(survey)
library(RColorBrewer)
library(irr)
library(clusterSEs)
library(dplyr)
library(tibble)
library(forecast)
library(wnominate)
library(devtools)
library(Rvoteview)
library(pscl)
library(gtools)
library(plotrix)
library(gplots)




# READ IN DATA ------------------------------------------------------------

#You must set the working directory to the source file location before reading in the data

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Read in intial data files
adminspending <- read.csv("obamaadminspending.csv", header = TRUE)

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE)
termspeeches <- read.csv("obamatermrhetoric.csv", header = TRUE)

rollcall <- readRDS("obamarollcallobject.rds")


head(yearlyspending) #View first few rows of data
head(adminspending)

head(yearlyspeeches)
head(termspeeches)

head(rollcall)




###################SECTION 3: ORGANIZE DATA (MAY BE RUN TO RE-CREATE DERIVED VARIABLES, BUT NOT REQUIRED TO RUN SECTIONS 4 AND 5)###################

# CALCULATE VARIABLES FOR YEARLY SPENDING PER PERSON AND AS A PERCENT OF THE DOMESTIC BUDGET--------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data file

#Note that the dataset already contains the following variables.They are recreated in the code below to show how they were calculated and gaurantee their accuracy.

yearlyspending$perpoor <- yearlyspending$poverty/yearlyspending$poor #Create variable for per poor person poverty spending

yearlyspending$perunemployed <- yearlyspending$poverty/yearlyspending$unemployed #Create variable for per unemployed person poverty spending

yearlyspending$perblack <- yearlyspending$civil/yearlyspending$blacks #Create variable for per black person civil rights spending

yearlyspending$povertydom <- yearlyspending$povertydisc/yearlyspending$domestic #Create variable for proposed poverty spending as a percentage of the proposed domestic budget

yearlyspending$civildom <- yearlyspending$civil/yearlyspending$domestic #Create variable for proposed civil rights spending as a percentage of the proposed domestic budget

head(yearlyspending) #View data frame

write.csv(yearlyspending, file = "obamayearlyspending.csv") #Export updated data file; doing so will replace the existing file



# CREATE DATASET FOR AVERAGE SPENDING PER PRESIDENTIAL ADMINSITRATION ------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data file

adminspending <- aggregate(yearlyspending, by = list(yearlyspending$president), FUN = mean) #Aggregate by president (there will be warnings for non-numeric columns returning NA's; these do not affect the aggregation of the rest of the columns and can be ignored).

print(adminspending) #View new dataframe; "Group.1" is a new variable indicating each president will be renamed; Note the NA's for the "President" variable, which will be replaced with names; Note the decimal values for variables meant to be whole numbers (e.g. "budgetid", "congressdem", "budgetyear", "term", etc.), which will be removed since they are not required to analyze this datset.

adminspending$president <- adminspending$Group.1 #Rename "Group.1" variable "president"

adminspending <- subset(adminspending, select = c("president", "presidentid", "poverty", "civil", "perpoor", "perunemployed", "perblack", "povertydom", "civildom")) #Subset only required variables

adminspending <- arrange(adminspending, presidentid) #Arrange rows sequentially by president

print(adminspending) #View the dataset


write.csv(adminspending, file = "obamaadminspending.csv") #Export the new data file; doing so will replace the existing file



# CALCULATE AND ADD FREQUENCY VARIABLES FOR YEARLY RHETORIC ---------------------------

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE) #Reread in required data file


#Note that the dataset already contains the following variables.They are recreated in the code below to show how they were calculated and gaurantee their accuracy.

yearlyspeeches$ppovertyword <- yearlyspeeches$povertyword - yearlyspeeches$npovertyword #Calculate the total number of positive/neutral poverty words

yearlyspeeches$pcivilword <- yearlyspeeches$civilword - yearlyspeeches$ncivilword #Calculate the total number of positive/neutral civil rights words


yearlyspeeches$povertyper <- (yearlyspeeches$povertyword/yearlyspeeches$words)*10000 #Calculate poverty keywords per 10,000

yearlyspeeches$npovertyper <- (yearlyspeeches$npovertyword/yearlyspeeches$words)*10000 #Calculate negative poverty keywords per 10,000

yearlyspeeches$ppovertyper <- yearlyspeeches$povertyper - yearlyspeeches$npovertyper #Calculate positive/neautral poverty keywords per 10,000


yearlyspeeches$middleper <- (yearlyspeeches$middleword/yearlyspeeches$words)*10000 #Calculate middle class keywords per 10,000


yearlyspeeches$civilper <- (yearlyspeeches$civilword/yearlyspeeches$words)*10000 #Calculate total civil rights keywords per 10,000

yearlyspeeches$ncivilper <- (yearlyspeeches$ncivilword/yearlyspeeches$words)*10000 #Calculate negative civil rights keywords per 10,000

yearlyspeeches$pcivilper <- yearlyspeeches$civilper - yearlyspeeches$ncivilper #Calculate positive/neutral civil rights keywords per 10,000


head(yearlyspeeches) #View dataset


write.csv(yearlyspeeches, file = "obamayearlyrhetoric.csv") #Export the updated data file; doing so will replace the existing file



# CREATE DATASET FOR AVERAGE PER TERM RHETORIC -----------------------

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE) #Reread in required data file

termspeeches <- aggregate(yearlyspeeches, by= list(yearlyspeeches$president, yearlyspeeches$term), FUN = mean) #Aggregate by term (there will be warnings for non-numeric columns returning NA's; these do not affect the aggregation of the rest of the columns and can be ignored).

print(termspeeches) #View new dataframe; "Group.1" is a new variable indicating each president and will be renamed; "Group.2" is a new variable indicating each presidential term and will be removed; Note the NA's for the "President" variable, which will be replaced with names; Note the decimal values for variables meant to be whole numbers (e.g. "budgetid", "congressdem", "speechyear", "term", etc.), which will be removed since they are not required to analyze this datset.

termspeeches$president <- termspeeches$Group.1 #Rename "Group.1" variable "president"

termspeeches$congressdem <- ifelse(termspeeches$congressdem >= .5, 1, 0) #Modify "congressdem" to a binary indicator for whether Democrats controlled both the House and Senate for at least half the years of a term

termspeeches$electionspeech <- ifelse(termspeeches$electionspeech > 0, 1, 0) #Modify "electionspeech" to a binary indicator for whether the president was up for reelection at the end of a term

termspeeches <- subset(termspeeches, select = -c(speechid, speechyear, refyear,Group.1, Group.2)) #Remove uncessary variables

termspeeches <- arrange(termspeeches, presidentid, term) #Order results chronologically by presidential term

print(termspeeches) #View the dataset


write.csv(termspeeches, file = "obamatermrhetoric.csv") #Export the new data file; it will replace the existing data file



# CREATE DATASET FOR AVERAGE PER PRESIDENTIAL ADMINISTRATION RHETORIC -----------------------

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE) #Reread in required data file

adminspeeches <- aggregate(yearlyspeeches, by = list(yearlyspeeches$president), FUN = mean) #Aggregate by president (there will be warnings for non-numeric columns returning NA's; these do not affect the aggregation of the rest of the columns and can be ignored).

print(adminspeeches) #View new dataframe; "Group.1" is a new variable indicating each president and will be renamed; Note the NA's for the "President" variable, which will be replaced with names; Note the decimal values for variables meant to be whole numbers (e.g. "budgetid", "congressdem", "speechyear", "term", etc.), which will be removed since they are not required to analyze this datset.

adminspeeches$president <- adminspeeches$Group.1 #Rename "Group.1" variable "president"

adminspeeches <- subset(adminspeeches, select = c(president, presidentid, povertyper, ppovertyper, npovertyper, civilper, pcivilper, middleper)) #Keep only required variables

adminspeeches <- arrange(adminspeeches, presidentid) #Order results chronologically by presidents

print(adminspeeches) #View the dataset

write.csv(adminspeeches, file = "obamaadminrhetoric.csv") #Export new data file; it will replace the existing data file




###################SECTION 4: ANALYSES IN MAIN PAPER ###################

# PLOT PER TERM POVERTY AND MIDDLE CLASS RHETORIC (FIGURE 1) ------------------------------------------

speechesterm <- read.csv("obamatermrhetoric.csv", header = TRUE) #Reread in necessary dataset

head(speechesterm) #View dataset


dev.off() #Reset graphical parameters to default
par(mar=c(2,1.8,1,0)) #Set graph margins
par(family = "sans") #Set text


pospov <- speechesterm$ppovertyper #Convert variables to be graphed into vectors
negpov <- speechesterm$npovertyper
middle <- speechesterm$middleper


mydata <- cbind(rbind(pospov, negpov, 0), rbind(0, 0, middle)) [,c(1, 15, 2, 16, 3, 17, 4, 18, 5, 19, 6, 20, 7, 21, 8, 22, 9, 23, 10, 24, 11, 25, 12, 26, 13, 27, 14, 28)] #Organize the data for stacked and grouped bars

mycolors <- c("gray38", "gray9", "gray99") #Establish colors for graphing


bp <- barplot(mydata, xlim = c(1.1, 39.75), ylim = c(0,40), main = "", space = c(0,0, 1, 0, 1, 0, 1,0, 1,0, 1,0, 1,0, 1,0, 1,0, 1,0, 1,0, 1, 0, 1, 0, 1, 0), ylab = "", yaxt = "n", col = mycolors, cex.axis = .75) #Create bargraph
abline(h=0, col = "black", lty = 1, lwd = 1.5) #Add line for x-axis
axis(2, tck = -.013, at = c(0, 10, 20, 30, 40), c("", "", "", "", "")) #Add line and ticks to y-axis
mtext(side = 2, text = "Keywords Per 10,000", line = 1., cex = .75) #Add y-axis label
mtext(side = 2, at = c(0, 10, 20, 30, 40), text = c("0", "10", "20", "30", "40"), line = .3, cex = .5) #Add y-axis values
mtext(side = 1, line = .4, at = c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40), text = c("Johnson\nTerm 2", "Nixon\nTerm 1","Nixon\nTerm 2", "Ford\nTerm 1", "Carter\nTerm 1", "Reagan\nTerm 1","Reagan\nTerm 2", "H.W. Bush\nTerm 1", "Clinton\nTerm 1","Clinton\nTerm 2", "G.W. Bush\nTerm 1", "G.W. Bush\nTerm 2", "Obama\nTerm 1", "Obama\nTerm 2"), cex = .5) #Add x-axis labels
mycolors <- c("gray38", "gray9", "black") #Establish colors for the legend
legend("topleft", c("Positive/Neutral Poverty Mentions", "Negative Poverty Mentions", "Middle Class Mentions"), col = mycolors, pch = c(15, 15, 0), bty = "n", cex = .75) #Add legend



# PLOT PER TERM CIVIL RIGHTS RHETORIC (FIGURE 2) ------------------------------------------

speechesterm <- read.csv("obamatermspeeches.csv", header = TRUE) #Reread in necessary dataset

head(speechesterm) #View the dataset


dev.off() #Reset graphical parameters to default
par(mar=c(2,1.8,1,0)) #Set graph margins
par(family = "sans") #Set text

civilpos <- speechesterm$pcivilper  #Convert variables to be graphed into vectors
civilneg <- speechesterm$ncivilper


mydata <- cbind(rbind(civilpos, civilneg)) #Organize the data for stacked bars

mycolors <- c("gray38", "gray9") #Establish colors for graphing


bp <- barplot(mydata, xlim = c(1.75, 27.25), ylim = c(0, 10), main = "", yaxt = 'n', col = mycolors, space = c(1)) #Create bargraph
abline(h=0, col = "black", lty = 1) #Add line for x-axis
axis(2, tck = -.015, at = c(0, 2, 4, 6, 8, 10), c("", "", "", "", "", "")) #Add line and ticks for y-axis
mtext(side = 2, text = "Keywords Per 10,000", line = 1.1, cex = .75) #Add y-axis label
mtext(side = 2, at = c(0, 2, 4, 6, 8, 10), text = c("0", "2", "4", "6", "8", "10"), line = .2, cex = .5) #Add y-axis values
mtext(side = 1, line = .4, at = bp, text = c("Johnson\nTerm 2", "Nixon\nTerm 1","Nixon\nTerm 2", "Ford\nTerm 1", "Carter\nTerm 1", "Reagan\nTerm 1","Reagan\nTerm 2", "H.W. Bush\nTerm 1", "Clinton\nTerm 1","Clinton\nTerm 2", "G.W. Bush\nTerm 1", "G.W. Bush\nTerm 2", "Obama\nTerm 1", "Obama\nTerm 2"), cex = .5) #Add x-axis labels
mycolors <- c("gray38", "gray9", "black") #Establish colors for the legend
legend("topleft", c("Positive/Neutral African American & Civil Rights Mentions", "Negative African American & Civil Rights Mentions"), col = mycolors, pch = c(15, 15), bty = "n", cex = .75) #Add legend




# COMPARE OBAMA’S RHETORIC TO PREDCESSORS' (DESCRIPTION OF FIGURES 1 AND 2) ---------------------------------

print(termspeeches) #Get Obama's averages and compare to predecessors'

print(adminspeeches) #Get Obama's averages and compare to predecessors'



# REGRESSION ANALYSES FOR POVERTY RHETORIC (TABLE 1; NOTE: THE CODE WILL TAKE A WHILE TO RUN)----------------------------------------

termspeeches <- read.csv("obamatermrhetoric.csv", header = TRUE) #Reread in necessary dataset

termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to a factor

#Create a dataframe of formulas to input into a loop
m0 <- "povertyper ~ obamadummy"
m1 <- "povertyper ~ obamadummy + percentunemployed"
m2 <- "povertyper ~ obamadummy + percentunemployed + gdpgrowth + percentpoor"

formulas <- rbind(m0, m1, m2) #Create a vector of formulas

#Create an empty list and iterating variable for loop
povword.regressions <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = termspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = termspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  povword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  povword.boot.out$Variable <- rownames(povword.boot.out) #Make a column out of the rownames
  povword.boot.out$se <- (povword.boot.out$ci.CI.higher-povword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  povword.boot.out <- cbind(coeffs, povword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  povword.boot.out <- subset(povword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(povword.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = termspeeches) #Run model using .lm to get adjusted R^2
  povword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  povword.boot.out$N <- nrow(termspeeches) #Add N to dataframe
  povword.regressions[[i]] <- povword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(povword.regressions)




# REGRESSION ANALYSES FOR CIVIL RIGHTS RHETORIC (TABLE 1; NOTE: THE CODE WILL TAKE A WHILE TO RUN)----------------------------------------

termspeeches <- read.csv("obamatermrhetoric.csv", header = TRUE) #Reread in necessary dataset

termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to a factor

#Create a dataframe of formulas to input into loop
m0 <- "civilper ~ obamadummy"
m1 <- "civilper ~ obamadummy + presdem"
m2 <- "civilper ~ obamadummy + percentunemployed + presdem + congressdem"

formulas <- rbind(m0, m1, m2)

#Define empty list and iterating variable for loop
civword.regressions <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = termspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = termspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civword.boot.out$Variable <- rownames(civword.boot.out) #Make a column out of the rownames
  civword.boot.out$se <- (civword.boot.out$ci.CI.higher-civword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civword.boot.out <- cbind(coeffs, civword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civword.boot.out <- subset(civword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civword.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = termspeeches) #Run model using .lm to get adjusted R^2
  civword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civword.boot.out$N <- nrow(termspeeches) #Add N to dataframe
  civword.regressions[[i]] <- civword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(civword.regressions)



# REGRESSION ANALYSES FOR MIDDLE CLASS RHETORIC (TABLE 1; NOTE: THE CODE WILL TAKE A WHILE TO RUN)----------------------------------------

termspeeches <- read.csv("obamatermrhetoric.csv", header = TRUE) #Reread in necessary dataset

termspeeches$presidentidid <- as.factor(termspeeches$presidentid) #Set clustering variable to a factor

#Create a dataframe of formulas to input into loop
m0 <- "middleper ~ obamadummy"
m1 <- "middleper ~ obamadummy + gdp"
m2 <- "middleper ~ obamadummy + percentunemployed + gdpgrowth + percentpoor + congressdem + gdp + deficit + electionspeech"

formulas <- rbind(m0, m1, m2)

#Define empty list and iterating variable for loop
midword.regressions <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = termspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = termspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  midword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  midword.boot.out$Variable <- rownames(midword.boot.out) #Make a column out of the rownames
  midword.boot.out$se <- (midword.boot.out$ci.CI.higher-midword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  midword.boot.out <- cbind(coeffs, midword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  midword.boot.out <- subset(midword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(midword.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = termspeeches) #Run model using .lm to get adjusted R^2
  midword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  midword.boot.out$N <- nrow(termspeeches) #Add N to dataframe
  midword.regressions[[i]] <- midword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(midword.regressions)



# FINAL REGRESSION RESULTS FOR RHETORIC COMPILED (TABLE 1; MUST FIRST RUN PREVIOUS THREE SUBSECTIONS) --------------------

print(povword.regressions)

print(civword.regressions)

print(midword.regressions)



# PLOT PER ADMINISTRTAION AVERAGE ANTI-POVERTY SPENDING (FIGURE 3) -------------------------------

adminspending <- read.csv("obamaadminspending.csv", header = TRUE)  #Reread in necessary dataset

dev.off() #Reset graphical parameters to default
par(mar=c(5.1,4.1,4.1,.75)) #Establish aesthetic parameters of figure; margins
par(mfrow=c(3,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "sans") #Set text

tot <- adminspending$poverty #Define variables to graph as vectors
poor <- adminspending$perpoor
unemploy <- adminspending$perunemployed
domestic <- adminspending$povertydom * 100


#Plot first bargraph: per term average of total anti-poverty spending
bp <- barplot(tot, ylim = c(0, 200000), yaxt = "n", cex.main = 1.19, main = "Yearly Average for Total Proposed Anti-Poverty Spending by President", ylab = "Millions of 2016 Dollars", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "G.W. Bush", "Obama"), col = "gray")
abline(h=0, col = "black", lwd = 1.5) #Create x-axis line
axis(2, at = c(0, 50000, 100000, 150000, 200000), labels = c("0K", "", "100K", "", "200K")) #Create y-axis labels
amount <- adminspending$poverty
print(amount)
amount.lab <- c("46,157", "36,701", "83,886", "52,012", "69,069", "94,579", "121,509", "172,920")
text(bp, amount-9500, label = amount.lab, cex = .8)


#Plot second bargraph: per term average for anti-poverty spending per poor and unemployed persons
mydata <- as.matrix(rbind(poor, unemploy)) #Organize data so that bars for per poor and per unemployed can be graphed side-by-side
legcol <- gray.colors(2, start = .8, end = .3, gamma = 2.2, alpha = NULL) #Set colors for bargraph

bp <- barplot(mydata, ylim = c(0, 20000), cex.main = 1.19, beside = TRUE, space = c(0, .35), yaxt = "n", main = "Yearly Average for Proposed Anti-Poverty Spending Per Person by President", ylab = "2016 Dollars", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "G.W. Bush", "Obama"), col = legcol)
abline(h=0, col = "black", lwd = 1.5) #Create x-axis line
legend("topleft", bty = "n", c("Per Poor Person","Per Unemployed Person"), col = legcol, pch = c(16, 16)) #Create legend
axis(2, at = c(0, 5000, 10000, 15000, 20000), labels = c("0K", "", "10K", "", "20K")) #Create y-axis label
amount <- mydata
print(amount)
amount.lab <- c("1,896", "10,762", "1,437", "4,749", "3,204", "12,563", "1,585", "6,332", "1,983", "8,739", "2,685", "13,932", "3,360", "15,678", "3,820", "15,586")
text(bp, amount-820, label = amount.lab, col = c("black", "white"), cex = .8)


#Plot third bargraph: per term average for anti-poverty spending as a percent of the proposed domestic budget
bp <- barplot(domestic, ylim = c(0, 30), cex.main = 1.19, main = "Yearly Average for Proposed Discretionary Anti-Poverty Spending as a Percent\nof the Proposed Discretionary Domestic Budget by President", ylab = "Percentage", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "G.W. Bush", "Obama"), col = "gray")
abline(h=0, col = "black", lwd = 1.5) #Create x-axis line
amount <- domestic
print(amount)
amount.lab <- c("10.95", "7.99", "11.48", "9.30", "15.76", "22.78", "24.48", "26.89")
text(bp, amount-1.35, label = amount.lab, cex = .8)



# PLOT PER ADMINISTRTAION AVERAGE CIVIL RIGHTS SPENDING (FIGURE 4) -------------------------------

adminspending <- read.csv("obamaadminspending.csv", header = TRUE) #Reread in necessary dataset

dev.off() #Reset graphical parameters to default
par(mar=c(5.1,4.1,4.1,.75)) #Establish aesthetic parameters of figure; margins
par(mfrow=c(3,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "sans") #Set text

tot <- adminspending$civil #Define variables to graph
blacks <- adminspending$perblack
domestic <- adminspending$civildom * 100


#Plot first bargraph: per term average of total civil rights spending
bp <- barplot(tot, ylim = c(0, 2000), main = "Yearly Average for Total Proposed Minority & Civil Rights Spending by President", ylab = "Millions of 2016 Dollars",  cex.main = 1.19, names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "G.W. Bush", "Obama"), col = "gray")
abline(h=0) #Create x-axis line
amount <- tot
print(amount)
amount.lab <- c("633", "838", "1,077", "912", "1,032", "1,129", "1,438", "1,725")
text(bp, amount-85, label = amount.lab, cex = .8)


#Plot second bargraph: per term average for civil rights spending black person
bp <- barplot(blacks, ylim = c(0, 50), main = "Yearly Average for Proposed Minority & Civil Rights Spending Per African American by President", ylab = "2016 Dollars", cex.main = 1.19, names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "G.W. Bush", "Obama"), col = "gray")
abline(h=0) #Create x-axis line
amount <- blacks
print(amount)
amount.lab <- c("27.35", "34.58", "42.13", "32.39", "33.06", "32.90", "37.65", "39.47")
text(bp, amount-2.2, label = amount.lab, cex = .8)


#Plot third bargraph: per term average for civil rights spending as a percent of the proposed domestic budget
bp <- barplot(domestic, ylim = c(0, .33), main = "Yearly Average for Proposed Discretionary Minority & Civil Rights Spending\nas a Percent of the Proposed Discretionary Domestic Budget by President",  cex.main = 1.19, ylab = "Percentage", names.arg = c("Nixon", "Ford","Carter", "Reagan", "H.W. Bush", "Clinton", "G.W. Bush", "Obama"), col = "gray")
abline(h=0) #Create x-axis line
amount <- domestic
print(amount)
amount.lab <- c("0.15", "0.13", "0.14", "0.16", "0.23", "0.27", "0.28", "0.30")
text(bp, amount-.014, label = amount.lab, cex = .8)



# COMPARE OBAMA’S SPENDING TO PREDCESSORS' (DESCRIPTION OF FIGURES 3 AND 4) ---------------------------------

adminspending <- read.csv("obamaadminspending.csv", header = TRUE) #Reread in necessary dataset

print(adminspending) #Get Obama's averages and compare to predecessors'

obama <- subset(adminspending, president == "Obama") #Calculate percent change between W. Bush and Obama
bush <- subset(adminspending, president == "W. Bush")
test <- (obama$poverty - bush$poverty)/bush$poverty
print(test)



# REGRESSION ANALYSES FOR POVERTY SPENDING (TABLE 2; NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

yearlyspending$presidentid <- as.factor(yearlyspending$presidentid) #Set clustering variable as a factor

#Create a dataframe of formulas to input into loop
m0 <- "poverty ~ obamadummy"
m1 <- "poverty ~ obamadummy + gdp + electionbudget"
m2 <- "poverty ~ obamadummy + gdp + electionbudget + gdpgrowth + percentpoor + percentunemployed + congressdem + presdem + congressdem*presdem"
m3 <- "poverty ~ obamadummy + gdp + electionbudget + gdpgrowth + percentpoor + percentunemployed + congressdem + presdem + congressdem*presdem + deficit"

formulas <- rbind(m0, m1, m2, m3)

#Define empty list and iterating variable for loop
poverty.regressions <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspending) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspending, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  poverty.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  poverty.boot.out$Variable <- rownames(poverty.boot.out) #Make a column out of the rownames
  poverty.boot.out$se <- (poverty.boot.out$ci.CI.higher-poverty.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  poverty.boot.out <- cbind(coeffs, poverty.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  poverty.boot.out <- subset(poverty.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(poverty.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspending) #Run model using .lm to get adjusted R^2
  poverty.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  poverty.boot.out$N <- nrow(yearlyspending) #Add N to dataframe
  poverty.regressions[[i]] <- poverty.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 


#View final regressions  
print(poverty.regressions)




# REGRESSION ANALYSES FOR CIVIL RIGHTS SPENDING (TABLE 2; NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in necessary dataset

yearlyspending$presidentidid <- as.factor(yearlyspending$presidentid) #Set clustering variable to factor

#Create a dataframe of formulas to input into loop
m0 <- "civil ~ obamadummy"
m1 <- "civil ~ obamadummy + gdp"
m2 <- "civil ~ obamadummy + gdp + electionbudget + gdpgrowth + percentpoor + percentunemployed + congressdem + presdem + congressdem*presdem + deficit"

formulas <- rbind(m0, m1, m2)

#Define empty list and iterating variable for loop
civil.regressions <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspending) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspending, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civil.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civil.boot.out$Variable <- rownames(civil.boot.out) #Make a column out of the rownames
  civil.boot.out$se <- (civil.boot.out$ci.CI.higher-civil.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civil.boot.out <- cbind(coeffs, civil.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civil.boot.out <- subset(civil.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civil.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspending) #Run model using .lm to get adjusted R^2
  civil.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civil.boot.out$N <- nrow(yearlyspending) #Add N to dataframe
  civil.regressions[[i]] <- civil.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(civil.regressions)



# FINAL SPENDING REGRESSION ANALYSES RESULTS FOR SPENDING COMPILED (TABLE 2; MUST FIRST RUN THE TWO PREVIOUS SUBSECTIONS)--------------------------------------------

print(poverty.regressions)
print(civil.regressions)





# CREATE FUNCTION FOR CALCULATING PREDICTED R2 IN FORECASTING MODELS (MUST BE RUN BEFORE ANY SUBSECTIONS RELATED TO FIGURE 5) --------

pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  return(pred.r.squared)
}

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}



# CALCULATE OBAMA'S PREDICTED VS. ACTUAL ANTI-POVERTY SPENDING (DESCRIPTION OF FIGURE 5; NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

yearlyspending$congresspres <- yearlyspending$congressdem*yearlyspending$presdem #Create an interaction variable since prediction procedure disallows interactions within the model

exceptobama <- subset(yearlyspending, president != "Obama") #Remove Obama from the dataset

exceptobama$presidentid <- as.factor(exceptobama$presidentid) #Make clustering variable a factor

#Create a dataframe of formulas to input into loop
m0 <- "poverty ~  gdp + deficit + electionbudget + congressdem + presdem + congressdem*presdem"

formulas <- rbind(m0)

#Define empty list and iterating variable for loop
poverty.regressions.pred <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = exceptobama) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = exceptobama, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  poverty.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  poverty.boot.out$Variable <- rownames(poverty.boot.out) #Make a column out of the rownames
  poverty.boot.out$se <- (poverty.boot.out$ci.CI.higher-poverty.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  poverty.boot.out <- cbind(coeffs, poverty.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  poverty.boot.out <- subset(poverty.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(poverty.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = exceptobama) #Run model using .lm to get adjusted R^2
  poverty.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  poverty.boot.out$PredR2 <- pred_r_squared(m)
  poverty.boot.out$N <- nrow(exceptobama) #Add N to dataframe
  poverty.regressions.pred[[i]] <- poverty.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 

#View the results
print(poverty.regressions.pred) #These are the models presented in the OA

#Predict Obama's spending using the model. Note that clustering is not necessary for the prediction because the dataset being fed into the model contains only one president.

obama <- subset(yearlyspending, presidentid == 8) #Create a dataset with only Obama

povertypredict <- lm(poverty ~  gdp + deficit + electionbudget + congressdem + presdem + congresspres, data = exceptobama) #Call the regression model using a function that can generate predictions, but not clustering

mydata <- data.frame(gdp = obama$gdp, deficit = obama$deficit, electionbudget = obama$electionbudget, congressdem = obama$congressdem, presdem = obama$presdem, congresspres = obama$congresspres) #Generate data from Obama's years to use in the model

povertyprediction <- predict(povertypredict, mydata, interval = "confidence") #Get predictions for Obama

povertyprediction <- data.frame(povertyprediction) #Store predictions in a data frame

povertyprediction$actual <- obama$poverty #Add Obama's actual spending to the prediction dataframe

povertyprediction$budgetyear <- obama$budgetyear #Add Obama's budget years to the prediction dataframe 

povertyprediction #View Obama's predicted versus actual poverty spending




# CALCULATE OBAMA'S PREDICTED VS. ACTUAL CIVIL RIGHTS SPENDING (DESCRIPTION OF FIGURE 5; NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

exceptobama <- subset(yearlyspending, president != "Obama") #Remove Obama from the dataset

exceptobama$presidentid <- as.factor(exceptobama$presidentid) #Make clustering variable a factor

#Create a dataframe of formulas to input into loop
m0 <- "civil ~  gdp"

formulas <- rbind(m0)

#Define empty list and iterating variable for loop
civil.regressions.pred <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = exceptobama) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = exceptobama, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civil.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civil.boot.out$Variable <- rownames(civil.boot.out) #Make a column out of the rownames
  civil.boot.out$se <- (civil.boot.out$ci.CI.higher-civil.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civil.boot.out <- cbind(coeffs, civil.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civil.boot.out <- subset(civil.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civil.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = exceptobama) #Run model using .lm to get adjusted R^2
  civil.boot.out$PredR2 <- pred_r_squared(m)
  civil.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civil.boot.out$N <- nrow(exceptobama) #Add N to dataframe
  civil.regressions.pred[[i]] <- civil.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 


#View the results
print(civil.regressions.pred) #These are the models presented in the OA

#Predict Obama's spending using the model. Note that clustering is not necessary for the prediction because the dataset being fed into the model contains only one president.

obama <- subset(yearlyspending, presidentid == 8) #Create a dataset with only Obama

civilpredict <- lm(civil ~ gdp, data = exceptobama) #Call the regression model using a function that can generate predictions, but not clustering

mydata <- data.frame(gdp = obama$gdp) #Generate data from Obama's years to use in the model

civilprediction <- predict(civilpredict, mydata, interval = "confidence") #Get predictions for Obama

civilprediction <- data.frame(civilprediction) #Store predictions in a data frame

civilprediction$actual <- obama$civil #Add Obama's actual spending to the prediction dataframe

civilprediction$budgetyear <- obama$budgetyear #Add Obama's budget years to the prediction dataframe 

civilprediction #View Obama's predicted versus actual civil spending




# PLOT OBAMAS PREDICTED VS. ACTUAL SPENDING (FIGURE 5)------------------

povertyprediction #View the dataframes that will be used for graphing
civilprediction

dev.off() #Reset graphical parameters to default
par(mar=c(4.1,4.1,2.75,.75)) #Set margins
par(mfrow=c(2,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "sans") #Set text

plotCI(povertyprediction$budgetyear, povertyprediction$fit, ylim = c(50000, 250000), yaxt ="n", ylab = "Millions of 2016 Dollars", xlab = "Year", li = povertyprediction$lwr, ui = povertyprediction$upr, cex.axis = .85, cex.lab = .85) #Plot predictions with predictions invtervals
points(povertyprediction$budgetyear, povertyprediction$actual, pch = 16) #Add points for predicted spending
lines(povertyprediction$budgetyear, povertyprediction$fit, lty = 2) #Add a line for predicted spending
lines(povertyprediction$budgetyear, povertyprediction$actual, lty = 1) #Add line for spending spending
legend("topleft", bty = "n", lty = c(1, 1), c("Obama's Predicted Spending", "Obama's Actual Spending"), pch = c(21, 16), cex = .85) #Add a legend
axis(2, at = c(50000,100000, 150000, 200000, 250000), c("50K", "100K", "150K", "200K", "250K"), cex.lab = .85, cex.axis = .85) #Add the x-axis labels
title("Anti-Poverty Spending", line = .75, cex.main = .9)


plotCI(civilprediction$budgetyear, civilprediction$fit, ylim = c(1300,1900), ylab = "Millions of 2016 Dollars", xlab = "Year", li = civilprediction$lwr, ui = civilprediction$upr, cex.axis = .85, cex.lab = .85) #Plot predictions with predictions invtervals
points(civilprediction$budgetyear, civilprediction$actual, pch = 16) #Add points for predicted spending
lines(civilprediction$budgetyear, civilprediction$fit, lty = 2) #Add a line for predicted spending
lines(civilprediction$budgetyear, civilprediction$actual, lty = 1) #Add line for actual spending
legend("topleft", bty = "n", lty = c(1, 1), c("Obama's Predicted Spending", "Obama's Actual Spending"), pch = c(21, 16), cex = .85) #Add a legend
title("Civil Rights Spending", line = .75, cex.main = .9)






###################SECTION 5: ANALYSES IN ONLINE APPENDIX ###################

# INTER-CODER RELIABILITY (OA-C) --------------------------------------------------

reliability <- read.csv("obamaicreliability.csv", header = TRUE) #Read in required data file

print(reliability) #View the data

reliablepov <- subset(reliability, rhetoric == "poverty") #Create subsets by category of rhetoric
reliablenegpov <- subset(reliability, rhetoric == "negpoverty")
reliablecivil <- subset(reliability, rhetoric == "civil")
reliablenegcivil <- subset(reliability, rhetoric == "negcivil")
reliablemiddle <- subset(reliability, rhetoric == "middle")


#Test reliability of poverty rhetoric coding
reliablepov$test <- ifelse(reliablepov$coder1 == reliablepov$coder2, "agree", "disagree") #Calculate percent agreement
table(reliablepov$test)
prop.table(table(reliablepov$test))

reliablepovkripp <- as.matrix(t(subset(reliablepov, select = c("coder1", "coder2")))) #Calculate Kripendorff Alpha
reliablepovkripp <- kripp.alpha(reliablepovkripp, method=c("ordinal"))
reliablepovkripp


#Test reliability of negative poverty rhetoric coding
reliablenegpov$test <- ifelse(reliablenegpov$coder1 == reliablenegpov$coder2, "agree", "disagree") #Calculate percent agreement
table(reliablenegpov$test)
prop.table(table(reliablenegpov$test))

reliablenegpovkripp <- as.matrix(t(subset(reliablenegpov, select = c("coder1", "coder2")))) #Calculate Kripendorff Alpha
reliablenegpovkripp <- kripp.alpha(reliablenegpovkripp, method=c("ordinal"))
reliablenegpovkripp


#Test reliability of civil rights rhetoric coding
reliablecivil$test <- ifelse(reliablecivil$coder1 == reliablecivil$coder2, "agree", "disagree") #Calculate percent agreement
table(reliablecivil$test)
prop.table(table(reliablecivil$test))

reliablecivilkripp <- as.matrix(t(subset(reliablecivil, select = c("coder1", "coder2")))) #Calculate Kripendorff Alpha
reliablecivilkripp <- kripp.alpha(reliablecivilkripp, method=c("ordinal"))
reliablecivilkripp


#Test reliability of negative civil rights rhetoric coding
reliablenegcivil$test <- ifelse(reliablenegcivil$coder1 == reliablenegcivil$coder2, "agree", "disagree") #Calculate percent agreement
table(reliablenegcivil$test)
prop.table(table(reliablenegcivil$test))

reliablenegcivilkripp <- as.matrix(t(subset(reliablenegcivil, select = c("coder1", "coder2")))) #Calculate Kripendorff Alpha
reliablecivilkripp <- kripp.alpha(reliablenegcivilkripp, method=c("ordinal"))
reliablecivilkripp


#Test reliability of middle class rhetoric coding
reliablemiddle$test <- ifelse(reliablemiddle$coder1 == reliablemiddle$coder2, "agree", "disagree") #Calculate percent agreement
table(reliablemiddle$test)
prop.table(table(reliablemiddle$test))

reliablemiddlekripp <- as.matrix(t(subset(reliablemiddle, select = c("coder1", "coder2")))) #Calculate Kripendorff Alpha
reliablemiddlekripp <- kripp.alpha(reliablemiddlekripp, method=c("ordinal"))
reliablemiddlekripp



# MINORITY DW-NOMINATE SCORES (OA-F, NOTE: THIS CODE WILL TAKE HOURS OR DAYS TO RUN)---------------------------------------------

# The wnominate package takes a roll-call object and scales votes along an ideological spectrum.
# There are two ways to obtain the necssary roll-call object.
# One easy option is to use the .rds file provided as part of this replication packet
# Another option is to use the RVoteview package to download roll-call votes directly from the Voteview website and use them to generate a roll-call object.
# Downloading and structuring data from Voteview requires additional, time intensive steps.
# The code to download and organize Voteview data produces an identitical roll-call object to the one already provided.
# Code for both options are provided below.

# CODE TO DOWNLOAD ROLL-CALL DATA DIRECTLY FROM THE VOTEVIEW WEBSITE (SKIP AHEAD TO JUST READ IN THE EXISTING ROLLCALL OBJECT):

# Create a loop to search the voteview website for rollcall votes.
# Only 5000 can be returned per search, so each Congressional session must be searched separately.
#Do NOT run this line of code back-to-back multiple times or you may crash the Voteview website.
votesfull <- data.frame()
i <- 1

while(i <= 113){
  dat <- voteview_search(congress = i)
  resfull <- smartbind(votesfull, dat)
  i <- i + 1
}

head(votesfull) #View the dataset

votesflat <- apply(votesfull, 2, as.character) #Flatten roll call data for export

write.csv(votesflat, "RollCallVotesFullUncoded.csv") #Export the file to .csv

# Edit the data in Excel. Create the following variables: "poverty" and "civil." 
# Set the poverty and civil variable equal to 1 if the relevant issue codes match those given in appendix F.
# Save the new data as "RollCallVotesFullCoded.csv".

votesfullcode <- read.csv("RollCallVotesFullCoded.csv") # Read the edited data back in

votesfullcode$id <- as.character(votesfullcode$id) #Ensure classes for each variable match original dataset
votesfullcode$chamber <- as.character(votesfullcode$chamber)
votesfullcode$date <- as.character(votesfullcode$date)
votesfullcode$short_description <- as.character(votesfullcode$short_description)
votesfullcode$text <- as.character(votesfullcode$text)
votesfullcode$bill_number <- as.character(votesfullcode$bill_number)
votesfullcode$key_flags <- as.list(votesfullcode$key_flags)
votesfullcode$codes.Peltzman <- as.list(votesfullcode$codes.Peltzman)
votesfullcode$codes.Clausen <- as.list(votesfullcode$codes.Clausen)
votesfullcode$codes.Issue <- as.list(votesfullcode$codes.Issue)

votessub <- subset(votesfullcode, poverty == 1 | civil == 1) #Subset roll call dataset to include only poverty and civil rights

head(votessub) #View the new dataset

rollcall <- voteview_download(votessub$id) #Create a rollcall object using the rollcall data. This will take awhile.


# CODE TO READ IN THE EXISTING ROLLCALL OBJECT (YOU MAY DO THIS IN LIEU OF THE PREVIOUS STEPS)

rollcall <- readRDS("obamarollcallobject.rds")


# RUN THE FOLLOWING MODEL AFTER EITHER CREATING A ROLLCALL OBJECT WITH VOTEVIEW OR READING IN THE EXISTING ROLLCALL OBJECT

# See the following site for information on the properties of the model specified below: https://legacy.voteview.com/dwnomin_joint_house_and_senate.htm
# Set ubeta, uweights, and dims to be identitical to the Nominate model used to calculate overall ideology
# Choose a conservative legislator in each dimension for polarity; it doesn't matter who
# Set minvotes to 15 to ensure estimates can be derived for each president of interest
# Note that the wnominate package will take a very long time to run.
# On a computer with a 2.5 GHz processor, this command took 6.25 hours.

points <- wnominate(rollcall, ubeta = 7.8334, uweights = .4113, dims = 2, polarity = list("icpsr", c(21192, 20300)), minvotes = 15) #Generate a scaling model for roll-call votes related to poverty and civil rights

# EXAMINE THE RESULTS

pointsfit <- points$fits #Shows % correct classification, APRE, and GMP for each dimension
print(pointsift)

pointspres <- subset(points$legislators, icpsr == 99903 | icpsr == 99904 | icpsr == 99905| icpsr == 99906| icpsr == 99907| icpsr == 99908| icpsr == 99909| icpsr == 99910| icpsr == 99911) #Subset the data to get only ideological estimates for presidents of interest
pointspres <- arrange(pointspres, icpsr)
print(pointspres)


#GRAPH THE RESULTS

dev.off() #Reset graphical parameters to default
par(family = "serif") #Set font
#Dimensions should be 6.5x5.4

x <- pointspres$coord1D
fig <- barplot(x, ylim = c(-1, 1), col= c("lightgrey", "grey43", "grey43","lightgrey", "grey43", "grey43", "lightgrey", "grey43","lightgrey"), cex.axis = .9, yaxt = "n", xlim = c(0, 11), main = "Presidential Ideology for Poverty and Civil Rights Issues", cex.main = .9)
abline(h = 0)
axis(2, at = c(-1, -.8, -.6, -.4, -.2, 0, .2, .4, .6, .8, 1), c("-1", "-.8", "-.6", "-.4", "-.2", "0", ".2", ".4", ".6", ".8", "1"), cex.axis = .85)
text(fig, c(lower[1] - .09, upper[2] + .10, upper[3 ]+ .10, lower[4] - .09, upper[5] + .10, upper[6] + .15, lower[7] - .09, upper[8] + .10, lower[9] - .09), c("Johnson\n-.29", "Nixon\n.07", "Ford\n.08", "Carter\n-.28", "Reagan\n.38", "H.W\nBush\n.20", "Clinton\n-.31", "W. Bush\n.29", "Obama\n-.60"), cex = .85)        
text( c(-1.7, -1.7), c(-.6, 1.01), c("Most Liberal", "Most Conservative"), pos = c(2, 2), xpd = NA, cex = .85, srt = 90)
arrows(-1.95, .36, -1.95, -.56, code = 3, xpd = NA, length =.07)


# T-TESTS FOR OBAMA’S POVERTY RHETORIC (OA-K) -------------------------------------

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE) #Reread in necessary datasets
adminspeeches <- read.csv("obamaadminrhetoric.csv", header = TRUE)

#Create subsets necessary to run t-tests
johnson <- subset(yearlyspeeches, president == "Johnson") #Create Democratic president subsets
carter <- subset(yearlyspeeches, president == "Carter")
clinton <- subset(yearlyspeeches, president == "Clinton")
obama <- subset(yearlyspeeches, president == "Obama")

all <- subset(adminspeeches, president == "Johnson"| president == "Nixon" | president == "Ford" | president == "Carter" | president == "Reagan" | president == "H.W. Bush" | president == "Clinton" | president == "W. Bush") #Create subset with all presidents except Obama
reps <- subset(adminspeeches, president == "Nixon" | president == "Ford" | president == "Reagan" | president == "H.W. Bush" | president == "W. Bush") #Create subset with all Republicans
dems <- subset(adminspeeches, president == "Johnson" |president == "Carter"|president == "Clinton") #Create subset with all Democrats except Obama


#Compare Obama to Johnson
johnsonpov <- johnson$povertyper #Create variable for Johnson's poverty rhetoric
obamapov <- obama$povertyper #Create variable for Obama's poverty rhetoric

johnsonobama <- t.test(johnsonpov, obamapov) #Two sample t-test
johnsonobama #View results


#Compare Obama to Carter
carterpov <- carter$povertyper #Create variable for Carter's poverty rhetoric

carterobama <- t.test(carterpov, obamapov) #Two sample t-test
carterobama #View results


#Compare Obama to Clinton
clintonpov <- clinton$povertyper #Create variable for Clinton's poverty rhetoric

clintonobama <- t.test(clintonpov, obamapov) #Two sample t-test
clintonobama #View results


#Compare Obama to Democrats
demspov <- dems$povertyper #Create variable for Democrat's poverty rhetoric

obamadems <- t.test(demspov, mu = mean(obamapov)) #Clustered one sample t-test
obamadems #View results


#Compare Obama to Republicans
repspov <- reps$povertyper #Create variable for Republican's povery rhetoric

obamareps <- t.test(repspov, mu = mean(obamapov)) #Clustered one sample t-test
obamareps #View results


#Compare Obama's Positive Mentions to Predcessors'
allppov <- all$ppovertyper #Create variable for Democrat's poverty rhetoric
obamappov <- obama$ppovertyper #Create variable for Obama's poverty rhetoric

obamaall <- t.test(allppov, mu = mean(obamappov)) #Clustered one sample t-test
obamaall #View results


#Compare Obama's Positive Mentions to Democrats' Positive Mentions
demsppov <- dems$ppovertyper #Create variable for Democrat's poverty rhetoric

obamadems <- t.test(demsppov, mu = mean(obamappov)) #Clustered one sample t-test
obamadems #View results


#Compare Obama to Republicans
repsppov <- reps$ppovertyper #Create variable for Republican's povery rhetoric

obamareps <- t.test(repsppov, mu = mean(obamappov)) #Clustered one sample t-test
obamareps #View results





# T-TESTS FOR OBAMA'S CIVIL RIGHTS RHETORIC (OA-K)---------------------------------

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE) #Reread in necessary datasets
adminspeeches <- read.csv("obamaadminrhetoric.csv", header = TRUE)

#Create subsets necessary to run t-tests
johnson <- subset(yearlyspeeches, president == "Johnson") #Create Democratic president subsets
carter <- subset(yearlyspeeches, president == "Carter")
clinton <- subset(yearlyspeeches, president == "Clinton")
obama <- subset(yearlyspeeches, president == "Obama")

all <- subset(adminspeeches, president == "Johnson"| president == "Nixon" | president == "Ford" | president == "Carter" | president == "Reagan" | president == "H.W. Bush" | president == "Clinton" | president == "W. Bush") #Create subset with all presidents except Obama (using per president averages)
reps <- subset(adminspeeches, president == "Nixon" | president == "Ford" | president == "Reagan" | president == "H.W. Bush" | president == "W. Bush") #Create subset with all Republicans
dems <- subset(adminspeeches, president == "Johnson" |president == "Carter"|president == "Clinton") #Create subset with all Democrats except Obama


#Compare Obama to Johnson
johnsonciv <- johnson$civilper #Create variable for Johnson's civil rights rhetoric
obamaciv <- obama$civilper #Create variable for Obama's civil rights rhetoric

johnsonobama <- t.test(johnsonciv, obamaciv) #Two sample t-test
johnsonobama #View results


#Compare Obama to Carter
carterciv <- carter$civilper #Create variable for Carter's civil rights rhetoric

carterobama <- t.test(carterciv, obamaciv) #Two sample t-test
carterobama #View results


#Compare Obama to Clinton
clintonciv <- clinton$civilper #Create variable for Clinton's civil rights rhetoric

clintonobama <- t.test(clintonciv, obamaciv) #Two sample t-test
clintonobama #View results


#Compare Obama to Democrats
demsciv <- dems$civilper #Create variable for Democrat's civil rights rhetoric

obamadems <- t.test(demsciv, mu = mean(obamaciv)) #Clustered one sample t-test
obamadems #View results


#Compare Obama to Republicans
repsciv <- reps$civilper #Create variable for Republican's civil rights rhetoric

obamareps <- t.test(repsciv, mu = mean(obamaciv)) #Clustered one sample t-test
obamareps #View results




# T-TESTS FOR OBAMA'S MIDDLE CLASS RHETORIC (OA-K)---------------------------------

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE) #Reread in necessary datasets
adminspeeches <- read.csv("obamaadminrhetoric.csv", header = TRUE)


#Create subsets necessary to run t-test
johnson <- subset(yearlyspeeches, president == "Johnson") #Create Democrat president subsets
carter <- subset(yearlyspeeches, president == "Carter")
clinton <- subset(yearlyspeeches, president == "Clinton")
obama <- subset(yearlyspeeches, president == "Obama")

all <- subset(adminspeeches, president == "Johnson"| president == "Nixon" | president == "Ford" | president == "Carter" | president == "Reagan" | president == "H.W. Bush" | president == "Clinton" | president == "W. Bush") #Create subset with all presidents except Obama (using per president averages)
reps <- subset(adminspeeches, president == "Nixon" | president == "Ford" | president == "Reagan" | president == "H.W. Bush" | president == "W. Bush") #Create subset with all Republicans
dems <- subset(adminspeeches, president == "Johnson" |president == "Carter"|president == "Clinton") #Create subset with all Democrats except Obama


#Compare Obama to Johnson
johnsonmid <- johnson$middleper #Create variable for Johnson's middle class rhetoric
obamamid <- obama$middleper #Create variable for Obama's middle class rhetoric

johnsonobama <- t.test(johnsonmid, obamamid) #Two sample t-test
johnsonobama #View results


#Compare Obama to Carter
cartermid <- carter$middleper #Create variable for Carter's middle class rhetoric

carterobama <- t.test(cartermid, obamamid) #Two sample t-test
carterobama #View results


#Compare Obama to Clinton
clintonmid <- clinton$middleper #Create variable for Clinton's middle class rhetoric

clintonobama <- t.test(clintonmid, obamamid) #Two sample t-test
clintonobama #View results


#Compare Obama to Democrats
demsmid <- dems$middleper #Create variable for Democrat's middle class rhetoric

obamadems <- t.test(demsmid, mu = mean(obamamid)) #Clustered one sample t-test
obamadems #View results


#Compare Obama to Republicans
repsmid <- reps$middleper #Create variable for Republican's middle class rhetoric

obamareps <- t.test(repsmid, mu = mean(obamamid)) #Clustered one sample t-test
obamareps #View results



# REGRESSION ANALYSES FOR YEARLY POVERTY RHETORIC (OA-L; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)---------------------------------

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE) #Reread in necessary dataset

yearlyspeeches$presidentidid <- as.factor(yearlyspeeches$presidentid) #Set clustering variable to a factor

#Create a dataframe of formulas to input into a loop
m0 <- "povertyper ~ obamadummy"
m1 <- "povertyper ~ obamadummy + gdp + gdpgrowth + deficit + percentpoor + percentunemployed + electionspeech + congressdem + presdem + congressdem*presdem"

formulas <- rbind(m0, m1) #Create a vector of formulas

#Create an empty list and iterating variable for loop
povword.regressions.yearly <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps= 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  povword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  povword.boot.out$Variable <- rownames(povword.boot.out) #Make a column out of the rownames
  povword.boot.out$se <- (povword.boot.out$ci.CI.higher-povword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  povword.boot.out <- cbind(coeffs, povword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  povword.boot.out <- subset(povword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(povword.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspeeches) #Run model using .lm to get adjusted R^2
  povword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  povword.boot.out$N <- nrow(yearlyspeeches) #Add N to dataframe
  povword.regressions.yearly[[i]] <- povword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(povword.regressions.yearly)




# REGRESSION ANALYSES FOR YEARLY CIVIL RIGHTS RHETORIC (OA-L; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)---------------------------------

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE) #Reread in necessary dataset

yearlyspeeches$presidentidid <- as.factor(yearlyspeeches$presidentid) #Set clustering variable to a factor

#Create a dataframe of formulas to input into a loop
m0 <- "civilper ~ obamadummy"
m1 <- "civilper ~ obamadummy + gdp + gdpgrowth + deficit + percentpoor + percentunemployed + electionspeech + congressdem + presdem + congressdem*presdem"

formulas <- rbind(m0, m1) #Create a vector of formulas

#Create an empty list and iterating variable for loop
civword.regressions.yearly <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civword.boot.out$Variable <- rownames(civword.boot.out) #Make a column out of the rownames
  civword.boot.out$se <- (civword.boot.out$ci.CI.higher-civword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civword.boot.out <- cbind(coeffs, civword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civword.boot.out <- subset(civword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civword.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspeeches) #Run model using .lm to get adjusted R^2
  civword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civword.boot.out$N <- nrow(yearlyspeeches) #Add N to dataframe
  civword.regressions.yearly[[i]] <- civword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(civword.regressions.yearly)





# REGRESSION ANALYSES FOR YEARLY MIDDLE CLASS RHETORIC (OA-L; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)---------------------------------

yearlyspeeches <- read.csv("obamayearlyrhetoric.csv", header = TRUE) #Reread in necessary dataset

yearlyspeeches$presidentidid <- as.factor(yearlyspeeches$presidentid) #Set clustering variable to a factor

#Create a dataframe of formulas to input into a loop
m0 <- "middleper ~ obamadummy"
m1 <- "middleper ~ obamadummy + gdp + gdpgrowth + deficit + percentpoor + percentunemployed + electionspeech + congressdem + presdem + congressdem*presdem"

formulas <- rbind(m0, m1) #Create a vector of formulas

#Create an empty list and iterating variable for loop
midword.regressions.yearly <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspeeches) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspeeches, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  midword.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  midword.boot.out$Variable <- rownames(midword.boot.out) #Make a column out of the rownames
  midword.boot.out$se <- (midword.boot.out$ci.CI.higher-midword.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  midword.boot.out <- cbind(coeffs, midword.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  midword.boot.out <- subset(midword.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(midword.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspeeches) #Run model using .lm to get adjusted R^2
  midword.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  midword.boot.out$N <- nrow(yearlyspeeches) #Add N to dataframe
  midword.regressions.yearly[[i]] <- midword.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(midword.regressions.yearly)




# FINAL REGRESSION RESULTS FOR YEARLY RHETORIC COMPILED (OA-L; MUST FIRST RUN PREVIOUS THREE SUBSECTIONS) --------------------

print(povword.regressions.yearly)

print(civword.regressions.yearly)

print(midword.regressions.yearly)





# PLOT YEARLY ANTI-POVERTY SPENDING (OA-M)------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data file

dev.off() #Reset graphical parameters to default
par(mar=c(5.1,4.1,4.1,.75)) #Establish aesthetic parameters of figure; margins
par(mfrow=c(3,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "serif") #Set text


x <- yearlyspending$budgetyear #Establish variables to graph
povtot <- yearlyspending$poverty
povdisc <- yearlyspending$povertydisc
poor <- yearlyspending$perpoor
unemploy <- yearlyspending$perunemployed
domestic <- yearlyspending$povertydom*100 #Convert from proportion to percentage



#Plot first line graph: total anti-poverty spending
plot(x, povtot, type = 'l', yaxt = 'n', lty=1, ylim = c(-7000, 240000), xlim = c(1970, 2017), ylab = "Millions of 2016 Dollars", xlab = "Year", xaxt="n", cex.axis = .95, main = "Proposed Anti-Poverty Spending by Year and President")
points(x, povtot, pch = 16) #Add points for each year

axis(2, at = c(0, 50000, 100000, 150000, 200000), labels = c("0K", "", "100K", "", "200K")) #Create y-axis label
axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Create x-axis label   

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, -6000, "Nixon", cex = .90) #Label each presidential administration
text(1976.5, -6000, "Ford", cex = .90)
text(1979.5, -6000, "Carter", cex = .90)
text(1985.5, -6000, "Reagan", cex = .90)
text(1991.5, -6000, "H.W. Bush", cex = .90)
text(1997.5, -6000, "Clinton", cex = .90)
text(2005.5, -6000, "W. Bush", cex = .90)
text(2013.5, -6000, "Obama", cex = .90)



#Plot second line graph: anti-poverty spending per poor and unemployed persons
plot(x, poor, type = 'l', lty=1, col = "black", ylim = c(-1000, 24000), xlim = c(1970, 2017), ylab = "2016 Dollars", xlab = "Year", xaxt="n", yaxt = "n", cex.axis = .95, main = "Proposed Anti-Poverty Spending Per Person by Year and President")
points(x, poor, pch = 16, col = "black") #Add points for per poor person spending for each year

lines(x, unemploy, lty = 2, col = "gray41") #Add line showing per unemployed person anti-poverty spending
points(x, unemploy, pch = 16, col = "gray41") #Add points for per unemployed person spending for each year

legend("topleft", pch = c(16, 16), lty = c(1, 1), col= c("black", "gray41"), c("Per Poor Person", "Per Unemployed Person"), bty = "n") #Create legend

axis(2, at = c(0, 5000, 10000, 15000, 20000), labels = c("0", "", "10K", "", "20K"))
axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Create x-axis label

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, -900, "Nixon", cex = .90) #Label each presidential administration
text(1976.5, -900, "Ford", cex = .90)
text(1979.5, -900, "Carter", cex = .90)
text(1985.5, -900, "Reagan", cex = .90)
text(1991.5, -900, "H.W. Bush", cex = .90)
text(1997.5, -900, "Clinton", cex = .90)
text(2005.5, -900, "W. Bush", cex = .90)
text(2013.5, -900, "Obama", cex = .90)



#Plot third line graph: anti-poverty spending as a percent of the proposed domestic budget
plot(x, domestic, type = 'l', lty=1, ylim = c(3, 35), xlim = c(1970, 2017), ylab = "Percentage", xlab = "Year", xaxt="n", cex.axis = .95, main = "Proposed Anti-Poverty Spending as a Percent of the\nProposed Discretionary Domestic Budget by Year and President")
points(x, domestic, pch = 16) #Add points for each year

axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Add x-axis label

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, 3.2, "Nixon", cex = .9) #Label each presidential administration
text(1976.5, 3.2, "Ford", cex = .9)
text(1979.5, 3.2, "Carter", cex = .9)
text(1985.5, 3.2, "Reagan", cex = .9)
text(1991.5, 3.2, "H.W. Bush", cex = .9)
text(1997.5, 3.2, "Clinton", cex = .9)
text(2005.5, 3.2, "W. Bush", cex = .9)
text(2013.5, 3.2, "Obama", cex = .9)

#Figure should be 6.5x8.8




# PLOT YEARLY CIVIL RIGHTS SPENDING (OA-M)------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data file


dev.off() #Reset graphical parameters to default
par(mar=c(5.1,4.1,4.1,.75)) #Establish aesthetic parameters of figure; margins
par(mfrow=c(3,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "serif") #Set text


x <- yearlyspending$budgetyear #Establish variables to graph
tot <- yearlyspending$civil
blacks <- yearlyspending$perblack
domestic <- yearlyspending$civildom*100 #Convert from proportion to percentage



#Plot first line graph: total civil rights spending
plot(x, tot, type = 'l', lty=1, ylim = c(0, 2100), xlim = c(1970, 2017), ylab = "Millions of 2016 Dollars", xlab = "Year", xaxt="n", cex.axis = .95, main = "Proposed Minority & Civil Rights Spending by Year and President")
points(x, tot, pch =16)

axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Create x-axis label   

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, 5, "Nixon", cex = .90) #Create labels for presidents
text(1976.5, 5, "Ford", cex = .90)
text(1979.5, 5, "Carter", cex = .90)
text(1985.5, 5, "Reagan", cex = .90)
text(1991.5, 5, "H.W. Bush", cex = .90)
text(1997.5, 5, "Clinton", cex = .90)
text(2005.5, 5, "W. Bush", cex = .90)
text(2013.5, 5, "Obama", cex = .90)



#Plot second line graph: civil rights spending per black person
plot(x, blacks, type = 'l', lty=1, ylim = c(10, 54), xlim = c(1970, 2017), ylab = "2016 Dollars", xlab = "Year", xaxt="n", cex.axis = .95, main = "Proposed Minority & Civil Rights Spending Per African American by Year and President")
points(x, blacks, pch = 16, col = "black") #Add points for per poor person spending for each year

axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Create x-axis label

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, 10.1, "Nixon", cex = .90) #Create labels for presidents
text(1976.5, 10.1, "Ford", cex = .90)
text(1979.5, 10.1, "Carter", cex = .90)
text(1985.5, 10.1, "Reagan", cex = .90)
text(1991.5, 10.1, "H.W. Bush", cex = .90)
text(1997.5, 10.1, "Clinton", cex = .90)
text(2005.5, 10.1, "W. Bush", cex = .90)
text(2013.5, 10.1, "Obama", cex = .90)



#Plot third line graph: civil rights spending as a percent of the proposed domestic budget
plot(x, domestic, type = 'l', lty=1, ylim = c(.0, .40), xlim = c(1970, 2017), ylab = "Percentage", xlab = "Year", xaxt="n", cex.axis = .95, main = "Proposed Minority & Civil Rights Spending as a Percent of the\nProposed Discretionary Domestic Budget by Year and President")
points(x, domestic, pch = 16) #Add points for each year

axis(1, at = c(1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017), labels = c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994","1995" ,"1996" , "1997","1998" , "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008","2009" , "2010","2011" , "2012","2013" , "2014","2015" ,"2016" , "2017"), tick= TRUE, cex.axis = .95, las = 2) #Add x-axis label

rect(1969.5, -100000, 1975.5, 1400000, col ='#FF003322', border = "transparent") #Create transparent rectangles to help distinguish between presidential adminsitrations
rect(1977.5, -100000, 1981.5, 1400000, col ='#FF003322', border = "transparent")
rect(1989.5, -100000, 1993.5, 1400000, col ='#FF003322', border = "transparent")
rect(2001.5, -100000, 2009.5, 1400000, col ='#FF003322', border = "transparent")

par(xpd=FALSE) #Keep text in plot region

text(1972.5, .001, "Nixon", cex = .90)
text(1976.5, .001, "Ford", cex = .90)
text(1979.5, .001, "Carter", cex = .90)
text(1985.5, .001, "Reagan", cex = .90)
text(1991.5, .001, "H.W. Bush", cex = .90)
text(1997.5, .001, "Clinton", cex = .90)
text(2005.5, .001, "W. Bush", cex = .90)
text(2013.5, .001, "Obama", cex = .90)

#Figure should be 6.5x8.8



# REGRESSIONS FOR ALTERNATIVE POVERTY MEASURES (OA-N, TABLE M1; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)---------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

yearlyspending$presidentid <- as.factor(yearlyspending$presidentid) #Set clustering variable to a factor

#Create a dataframe of formulas to input into loop
m1 <- "perpoor ~ obamadummy"
m2 <- "perpoor ~ obamadummy + gdp + gdpgrowth + deficit + percentpoor + percentunemployed + electionbudget + congressdem + presdem + congressdem*presdem"
m3 <- "perunemployed ~ obamadummy"
m4 <- "perunemployed ~ obamadummy + gdp + gdpgrowth + deficit + percentpoor + percentunemployed + electionbudget + congressdem + presdem + congressdem*presdem"
m5 <- "povertydom ~ obamadummy"
m6 <- "povertydom ~ obamadummy + gdp + gdpgrowth + deficit + percentpoor + percentunemployed + electionbudget + congressdem + presdem + congressdem*presdem"

formulas <- rbind(m1, m2, m3, m4, m5, m6)

#Define empty list and iterating variable for loop
poverty.alt <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspending) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspending, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  poverty.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  poverty.boot.out$Variable <- rownames(poverty.boot.out) #Make a column out of the rownames
  poverty.boot.out$se <- (poverty.boot.out$ci.CI.higher-poverty.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  poverty.boot.out <- cbind(coeffs, poverty.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  poverty.boot.out <- subset(poverty.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(poverty.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspending) #Run model using .lm to get adjusted R^2
  poverty.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  poverty.boot.out$N <- nrow(yearlyspending) #Add N to dataframe
  poverty.alt[[i]] <- poverty.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 


#View the results
names(poverty.alt) <- c("PerPoor1", "PerPoor2", "PerUnemployed1", "PerUnemployed2", "PercentBudget1", "PercentBudget2") #Name each element of the list
print(poverty.alt)



# REGRESSIONS FOR ALTERNATIVE CIVIL RIGHTS MEASURES (OA-N, TABLE M2; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)---------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

yearlyspending$presidentid <- as.factor(yearlyspending$presidentid) #Set clustering variable to a factor

#Create a dataframe of formulas to input into loop
m1 <- "perblack ~ obamadummy"
m2 <- "perblack ~ obamadummy + gdp + gdpgrowth + deficit + percentpoor + percentunemployed + electionbudget + congressdem + presdem + congressdem*presdem"
m3 <- "civildom ~ obamadummy"
m4 <- "civildom ~ obamadummy + gdp + gdpgrowth + deficit + percentpoor + percentunemployed + electionbudget + congressdem + presdem + congressdem*presdem"

formulas <- rbind(m1, m2, m3, m4)

#Define empty list and iterating variable for loop
civil.alt <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspending) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspending, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civil.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civil.boot.out$Variable <- rownames(civil.boot.out) #Make a column out of the rownames
  civil.boot.out$se <- (civil.boot.out$ci.CI.higher-civil.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civil.boot.out <- cbind(coeffs, civil.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civil.boot.out <- subset(civil.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civil.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspending) #Run model using .lm to get adjusted R^2
  civil.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civil.boot.out$N <- nrow(yearlyspending) #Add N to dataframe
  civil.alt[[i]] <- civil.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 


#View the results
names(civil.alt) <- c("PerBlack1", "PerBlack2", "PercentBudget1", "PercentBudget2") #Name each element of the list
print(civil.alt)


# ROBUSTNESS CHECK FOR POVERTY SPENDING (OA-O, TABLE N1; NOTE: THIS CODE WILL TAKE AWHILE TO RUN) --------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in necessary dataset

yearlyspending$presidentidid <- as.factor(yearlyspending$presidentid) #Set clustering variable to factor

#Run each model using a loop

#Create a dataframe of formulas to input into loop
m1 <- "poverty ~ obamadummy + gdp + gdpgrowth"
m2 <- "poverty ~ obamadummy + gdp + deficit"
m3 <- "poverty ~ obamadummy + gdp + percentpoor"
m4 <- "poverty ~ obamadummy + gdp + percentunemployed"
m5 <- "poverty ~ obamadummy + gdp + electionbudget"
m6 <- "poverty ~ obamadummy + gdp + congressdem"
m7 <- "poverty ~ obamadummy + gdp + presdem"
m8 <- "poverty ~ obamadummy + gdp + congressdem + presdem + congressdem*presdem"


formulas <- rbind(m1, m2, m3, m4, m5, m6, m7, m8)


#Define empty list and iterating variable for loop
poverty.robust <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspending) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspending, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  poverty.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  poverty.boot.out$Variable <- rownames(poverty.boot.out) #Make a column out of the rownames
  poverty.boot.out$se <- (poverty.boot.out$ci.CI.higher-poverty.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  poverty.boot.out <- cbind(coeffs, poverty.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  poverty.boot.out <- subset(poverty.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(poverty.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspending) #Run model using .lm to get adjusted R^2
  poverty.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  poverty.boot.out$N <- nrow(yearlyspending) #Add N to dataframe
  poverty.robust[[i]] <- poverty.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(poverty.robust) #View the list


# ROBUSTNESS CHECK FOR CIVIL RIGHTS SPENDING (OA-O, TABLE N2; NOTE: THIS CODE WILL TAKE AWHILE TO RUN) --------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in necessary dataset

yearlyspending$presidentidid <- as.factor(yearlyspending$presidentid) #Set clustering variable to factor

#Run each model using a loop

#Create a dataframe of formulas to input into loop
m1 <- "civil ~ obamadummy + gdp + gdpgrowth"
m2 <- "civil ~ obamadummy + gdp + deficit"
m3 <- "civil ~ obamadummy + gdp + percentpoor"
m4 <- "civil ~ obamadummy + gdp + percentunemployed"
m5 <- "civil ~ obamadummy + gdp + electionbudget"
m6 <- "civil ~ obamadummy + gdp + congressdem"
m7 <- "civil ~ obamadummy + gdp + presdem"
m8 <- "civil ~ obamadummy + gdp + congressdem + presdem + congressdem*presdem"


formulas <- rbind(m1, m2, m3, m4, m5, m6, m7, m8)


#Define empty list and iterating variable for loop
civil.robust <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspending) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspending, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civil.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civil.boot.out$Variable <- rownames(civil.boot.out) #Make a column out of the rownames
  civil.boot.out$se <- (civil.boot.out$ci.CI.higher-civil.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civil.boot.out <- cbind(coeffs, civil.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civil.boot.out <- subset(civil.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civil.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspending) #Run model using .lm to get adjusted R^2
  civil.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civil.boot.out$N <- nrow(yearlyspending) #Add N to dataframe
  civil.robust[[i]] <- civil.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
}


#View the results
print(civil.robust) #View the list


# COMPARE OBAMA'S SPENDING TO DEMOCRATS (OA-P, MODOLS 1A, 1B, 2A, AND 2B; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in necessary dataset

yearlyspending$presidentidid <- as.factor(yearlyspending$presidentid) #Set clustering variable to factor

dems <- subset(yearlyspending, presdem == 1) #Create Democratic subset

#Create a dataframe of formulas to input into loop
m0 <- "poverty ~ obamadummy + gdp"
m1 <- "poverty ~ obamadummy + gdp + electionbudget + percentpoor + percentunemployed + deficit"
m2 <- "civil ~ obamadummy + gdp"
m3 <- "civil ~ obamadummy + gdp + electionbudget + gdpgrowth + percentunemployed + deficit"


formulas <- rbind(m0, m1, m2, m3)

#Define empty list and iterating variable for loop
dem.regressions <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = dems) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = dems, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  dem.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  dem.boot.out$Variable <- rownames(dem.boot.out) #Make a column out of the rownames
  dem.boot.out$se <- (dem.boot.out$ci.CI.higher-dem.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  dem.boot.out <- cbind(coeffs, dem.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  dem.boot.out <- subset(dem.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(dem.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = dems) #Run model using .lm to get adjusted R^2
  dem.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  dem.boot.out$N <- nrow(dems) #Add N to dataframe
  dem.regressions[[i]] <- dem.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 


#View the results
names(dem.regressions) <- c("DemPoverty1", "DemPoverty2", "DemCivil1", "DemCivil2") #Name each element of the list
print(dem.regressions)


# COMPARE OBAMA'S SPENDING TO REPUBLICANS (OA-P, MODELS 1C, 1D, 2C, AND 2D; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in necessary dataset

yearlyspending$presidentidid <- as.factor(yearlyspending$presidentid) #Set clustering variable to factor

reps <- subset(yearlyspending, presdem == 0 | presidentid == 8) #Create Republican subset, plus Obama

#Create a dataframe of formulas to input into loop
m0 <- "poverty ~ obamadummy + gdp"
m1 <- "poverty ~ obamadummy + gdp + electionbudget + gdpgrowth + percentpoor + deficit"
m2 <- "civil ~ obamadummy + gdp"
m3 <- "civil ~ obamadummy + gdp + electionbudget + percentpoor + percentunemployed + deficit"


formulas <- rbind(m0, m1, m2, m3)

#Define empty list and iterating variable for loop
rep.regressions <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = reps) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = reps, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  rep.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  rep.boot.out$Variable <- rownames(rep.boot.out) #Make a column out of the rownames
  rep.boot.out$se <- (rep.boot.out$ci.CI.higher-rep.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  rep.boot.out <- cbind(coeffs, rep.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  rep.boot.out <- subset(rep.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(rep.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = reps) #Run model using .lm to get adjusted R^2
  rep.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  rep.boot.out$N <- nrow(reps) #Add N to dataframe
  rep.regressions[[i]] <- rep.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 



#View the results
names(rep.regressions) <- c("RepPoverty1", "RepPoverty2", "RepCivil1", "RepCivil2") #Name each element of the list
print(rep.regressions)


# FINAL REGRESSION RESULTS FOR PARTISAN COMPARISIONS COMPILED (OA-Q; MUST FIRST RUN PREVIOUS TWO SUBSECTIONS) --------------------

print(dem.regressions)

print(rep.regressions)







# COMPARE OBAMA’S SPENDING UNDER DEM VS. REP CONGRESS (OA-Q) ----------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data file

obamadems <- subset(yearlyspending, president == 'Obama' & congressdem == 1) #Create subset for Obama years under Democratic control
obamareps <- subset(yearlyspending, president == 'Obama' & congressdem == 0) #Create subset for Obama years under Republican control

t.test(obamadems$poverty, obamareps$poverty) #Compare Obama's poverty spending under Democratic vs. Republican control

t.test(obamadems$civil, obamareps$civil) #Compare Obama's civil rights spending under Democratic vs. Republican control


# PLACEBO TESTS COMPARING OBAMA AND G.W. BUSH’S SPENDING (OA-R, TABLE Q1; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)----------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

yearlyspending$presidentid <- as.factor(yearlyspending$presidentid) #Set clustering variable to a factor

yearlyspending$bushdummy <- ifelse(yearlyspending$presidentid == 7, 1, 0) #Create a dummy variable for G.W. Bush

yearlyspendingbush <- subset(yearlyspending, presidentid != 8) #Create a dataset that excludes Obama

#Run Analyses for G.W. Bush
#Create a dataframe of formulas to input into loop
m0 <- "poverty ~ bushdummy + gdp"
m1 <- "civil ~ bushdummy + gdp"

formulas <- rbind(m0, m1)

#Define empty list and iterating variable for loop
bush.regressions.comp <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspendingbush) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspendingbush, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  bush.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  bush.boot.out$Variable <- rownames(bush.boot.out) #Make a column out of the rownames
  bush.boot.out$se <- (bush.boot.out$ci.CI.higher-bush.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  bush.boot.out <- cbind(coeffs, bush.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  bush.boot.out <- subset(bush.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(bush.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspendingbush) #Run model using .lm to get adjusted R^2
  bush.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  bush.boot.out$N <- nrow(yearlyspendingbush) #Add N to dataframe
  bush.regressions.comp[[i]] <- bush.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 



#Run Analyses for Obama
#Create a dataframe of formulas to input into loop
m0 <- "poverty ~ obamadummy + gdp"
m1 <- "civil ~ obamadummy + gdp"

formulas <- rbind(m0, m1)

#Define empty list and iterating variable for loop
obama.regressions.comp <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspending) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspending, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  obama.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  obama.boot.out$Variable <- rownames(obama.boot.out) #Make a column out of the rownames
  obama.boot.out$se <- (obama.boot.out$ci.CI.higher-obama.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  obama.boot.out <- cbind(coeffs, obama.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  obama.boot.out <- subset(obama.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(obama.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspending) #Run model using .lm to get adjusted R^2
  obama.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  obama.boot.out$N <- nrow(yearlyspending) #Add N to dataframe
  obama.regressions.comp[[i]] <- obama.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 


#View the results
print(obama.regressions.comp)
print(bush.regressions.comp)



# ADDITIONAL PLACEBO TESTS USING G.W. BUSH’S POVERTY SPENDING (OA-R, TABLE Q2; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)----------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

yearlyspending$presidentid <- as.factor(yearlyspending$presidentid) #Set clustering variable to a factor

yearlyspending$bushdummy <- ifelse(yearlyspending$presidentid == 7, 1, 0) #Create a dummy variable for G.W. Bush

yearlyspendingbush <- subset(yearlyspending, presidentid != 8) #Create a dataset that excludes Obama

#Create a dataframe of formulas to input into loop
m0 <- "poverty ~ bushdummy"
m1 <- "poverty ~ bushdummy + gdp + electionbudget + congressdem + presdem + congressdem*presdem + deficit"
m2 <- "poverty ~ bushdummy + gdp + electionbudget + gdpgrowth + percentpoor + percentunemployed + congressdem + presdem + congressdem*presdem + deficit"

formulas <- rbind(m0, m1, m2)

#Define empty list and iterating variable for loop
bush.regressions.poverty <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspendingbush) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspendingbush, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  bush.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  bush.boot.out$Variable <- rownames(bush.boot.out) #Make a column out of the rownames
  bush.boot.out$se <- (bush.boot.out$ci.CI.higher-bush.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  bush.boot.out <- cbind(coeffs, bush.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  bush.boot.out <- subset(bush.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(bush.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspendingbush) #Run model using .lm to get adjusted R^2
  bush.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  bush.boot.out$N <- nrow(yearlyspendingbush) #Add N to dataframe
  bush.regressions.poverty[[i]] <- bush.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 


#View the results
print(bush.regressions.poverty)



# ADDITIONAL PLACEBO TESTS USING G.W. BUSH’S CIVIL RIGHTS SPENDING (OA-R, TABLE Q2; NOTE: THIS CODE WILL TAKE AWHILE TO RUN)----------------------------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

yearlyspending$presidentid <- as.factor(yearlyspending$presidentid) #Set clustering variable to a factor

yearlyspending$bushdummy <- ifelse(yearlyspending$presidentid == 7, 1, 0) #Create a dummy variable for G.W. Bush

yearlyspendingbush <- subset(yearlyspending, presidentid != 8) #Create a dataset that excludes Obama

#Create a dataframe of formulas to input into loop
m0 <- "civil ~ bushdummy"
m1 <- "civil ~ bushdummy + gdp"
m2 <- "civil ~ bushdummy + gdp + electionbudget + gdpgrowth + percentpoor + percentunemployed + congressdem + presdem + congressdem*presdem + deficit"

formulas <- rbind(m0, m1, m2)

#Define empty list and iterating variable for loop
bush.regressions.civil <- list()
i <- 1


#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = yearlyspendingbush) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = yearlyspendingbush, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  bush.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  bush.boot.out$Variable <- rownames(bush.boot.out) #Make a column out of the rownames
  bush.boot.out$se <- (bush.boot.out$ci.CI.higher-bush.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  bush.boot.out <- cbind(coeffs, bush.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  bush.boot.out <- subset(bush.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(bush.boot.out) <- c("Variable", "Coefficient", "PValue", "StandardError") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = yearlyspendingbush) #Run model using .lm to get adjusted R^2
  bush.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  bush.boot.out$N <- nrow(yearlyspendingbush) #Add N to dataframe
  bush.regressions.civil[[i]] <- bush.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 


#View the results
print(bush.regressions.civil)


# FINAL REGRESSION RESULTS FOR PARTISAN COMPARISIONS COMPILED (OA-R, TABLE Q2; MUST FIRST RUN PREVIOUS TWO SUBSECTIONS) --------------------

print(bush.regressions.poverty)

print(bush.regressions.civil)


# CREATE FUNCTION FOR CALCULATING PREDICTED R2 IN FORECASTING MODELS (MUST BE RUN BEFORE ANY SUBSECTIONS RELATED TO APPENDIX S) --------

pred_r_squared <- function(linear.model) {
  lm.anova <- anova(linear.model)
  tss <- sum(lm.anova$"Sum Sq")
  # predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  return(pred.r.squared)
}

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}




# REGRESSION USED TO PREDICT OBAMA'S POVERTY SPENDING (OA-S; NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

yearlyspending$congresspres <- yearlyspending$congressdem*yearlyspending$presdem #Create an interaction variable since prediction procedure disallows interactions within the model

exceptobama <- subset(yearlyspending, president != "Obama") #Remove Obama from the dataset

exceptobama$presidentid <- as.factor(exceptobama$presidentid) #Make clustering variable a factor

#Create a dataframe of formulas to input into loop
m0 <- "poverty ~  gdp + deficit + electionbudget + congressdem + presdem + congressdem*presdem"

formulas <- rbind(m0)

#Define empty list and iterating variable for loop
poverty.regressions.pred <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = exceptobama) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = exceptobama, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  poverty.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  poverty.boot.out$Variable <- rownames(poverty.boot.out) #Make a column out of the rownames
  poverty.boot.out$se <- (poverty.boot.out$ci.CI.higher-poverty.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  poverty.boot.out <- cbind(coeffs, poverty.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  poverty.boot.out <- subset(poverty.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(poverty.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = exceptobama) #Run model using .lm to get adjusted R^2
  poverty.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  poverty.boot.out$PredR2 <- pred_r_squared(m)
  poverty.boot.out$N <- nrow(exceptobama) #Add N to dataframe
  poverty.regressions.pred[[i]] <- poverty.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 

#View the results
print(poverty.regressions.pred) #These are the models presented in the OA


# REGRESSION USED TO PREDICT OBAMA'S CIVIL RIGHTS SPENDING (OA-S; NOTE: THE CODE WILL TAKE A WHILE TO RUN) --------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

exceptobama <- subset(yearlyspending, president != "Obama") #Remove Obama from the dataset

exceptobama$presidentid <- as.factor(exceptobama$presidentid) #Make clustering variable a factor

#Create a dataframe of formulas to input into loop
m0 <- "civil ~  gdp"

formulas <- rbind(m0)

#Define empty list and iterating variable for loop
civil.regressions.pred <- list()
i <- 1

#Run the loop
while (i <= nrow(formulas)){
  m <- glm(as.formula(formulas[i,1]), data = exceptobama) #Run the model
  coeffs <- cbind(coefficients(m)) #Store coefficients
  cluster.m <- cluster.wild.glm(m, dat = exceptobama, cluster = ~presidentid, ci = .95, impose.null = FALSE, boot.reps = 100000) #Calculate clustered standard CI's and p-values (actual SE must be derived)
  civil.boot.out <- data.frame(cluster.m) #Store clustered P's and CI's
  civil.boot.out$Variable <- rownames(civil.boot.out) #Make a column out of the rownames
  civil.boot.out$se <- (civil.boot.out$ci.CI.higher-civil.boot.out$ci.CI.lower)/3.92 #Calculate clustered standard errors from CI's
  civil.boot.out <- cbind(coeffs, civil.boot.out) #Put regression coefficients and clustered stanard CI's, errors, and p-values in a dataframe
  civil.boot.out <- subset(civil.boot.out, select = c("Variable", "coeffs", "wild.cluster.BS.p.value", "se")) #Subset regression coefficients, clustered standard errors, and p-values
  names(civil.boot.out) <- c("Variable", "Coefficient", "PValue", "Standard Error") #Rename columns
  m <- lm(as.formula(formulas[i,1]), data = exceptobama) #Run model using .lm to get adjusted R^2
  civil.boot.out$PredR2 <- pred_r_squared(m)
  civil.boot.out$R2 <- summary(m)$adj.r.squared #Add adjusted R^2 to data frame
  civil.boot.out$N <- nrow(exceptobama) #Add N to dataframe
  civil.regressions.pred[[i]] <- civil.boot.out #Store results
  i <- i + 1 #Reset i for the next iteration of the loop
} 


#View the results
print(civil.regressions.pred) #These are the models presented in the OA

# FINAL REGRESSIONS USED TO PREDICT OBAMA'S SPENDING COMPILED (OA-S; MUST FIRST RUN PREVIOUS TWO SUBSECTIONS) --------------------

print(poverty.regressions.pred)

print(civil.regressions.pred)




# CALCULATE BUSH'S PREDICTED VS. ACTUAL POVERTY SPENDING (OA-T, TABLE FOR ACCURACY OF ANTI-POVERTY SPENDING) --------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

yearlyspending$congresspres <- yearlyspending$congressdem*yearlyspending$presdem #Create an interaction variable since prediction procedure disallows interactions within the model

exceptbush <- subset(yearlyspending, presidentid != 7 & presidentid != 8) #Remove Obama and G.W. Bush from the dataset

exceptbush$presidentid <- as.factor(exceptbush$presidentid) #Make clustering variable a factor

#Predict Bush's poverty spending using the model specified in OA-R. Note that clustering is not necessary for the prediction because the dataset being fed into the model contains only one president.

bush <- subset(yearlyspending, presidentid == 7) #Create a dataset with only Bush

povertypredict.bush <- lm(poverty ~  gdp + deficit + electionbudget + congressdem + presdem + congresspres, data = exceptbush) #Call the regression model using a function that can generate predictions, but not clustering

mydata <- data.frame(gdp = bush$gdp, deficit = bush$deficit, electionbudget = bush$electionbudget, congressdem = bush$congressdem, presdem = bush$presdem, congresspres = bush$congresspres, presidentid = bush$presidentid) #Generate data from Bush's years to use in the model

povertyprediction.bush <- predict(povertypredict.bush, mydata, interval = "confidence") #Get predictions for Bush

povertyprediction.bush <- data.frame(povertyprediction.bush) #Store predictions in a data frame

povertyprediction.bush$actual <- bush$poverty #Add Bush's actual spending to the prediction dataframe

povertyprediction.bush$budgetyear <- bush$budgetyear #Add Bush's budget years to the prediction dataframe 

povertyprediction.bush #View Bush's predicted versus actual poverty spending

#Get the accuracy of the model, as presented in the OA-S table for poverty spending predictions
povertyforecast <- forecast(povertypredict.bush, mydata, level = .95)
povertyacc <- accuracy(povertyforecast, bush$poverty)
print(povertyacc) #Examine the MAPE

totalbush <- sum(bush$poverty) #Calculate Bush's total predicted spending
print(totalbush)

totalbushpred <- sum(povertyprediction.bush$fit) #Calculate Bush's total predicted spending
print(totalbushpred)

diffbush <- totalbush - totalbushpred #Get differences between predicted and actual poverty proposals
print(diffbush)

# CALCULATE BUSH'S PREDICTED VS. ACTUAL CIVIL RIGHTS SPENDING (OA-T, TABLE FOR ACCURACY OF CIVIL RIGHTS SPENDING) --------------

yearlyspending <- read.csv("obamayearlyspending.csv", header = TRUE) #Reread in required data files

exceptbush <- subset(yearlyspending, presidentid != 7 & presidentid != 8) #Remove Obama and G.W. Bush from the dataset

exceptbush$presidentid <- as.factor(exceptbush$presidentid) #Make clustering variable a factor

#Predict Bush's poverty spending using the model specified in OA-R. Note that clustering is not necessary for the prediction because the dataset being fed into the model contains only one president.

bush <- subset(yearlyspending, presidentid == 7) #Create a dataset with only Bush

civilpredict.bush <- lm(civil ~  gdp, data = exceptbush) #Call the regression model using a function that can generate predictions, but not clustering

mydata <- data.frame(gdp = bush$gdp) #Generate data from Bush's years to use in the model

civilprediction.bush <- predict.lm(civilpredict.bush, mydata, interval = "confidence") #Get predictions for Bush

civilprediction.bush <- data.frame(civilprediction.bush) #Store predictions in a data frame

civilprediction.bush$actual <- bush$civil #Add Bush's actual spending to the prediction dataframe

civilprediction.bush$budgetyear <- bush$budgetyear #Add Bush's budget years to the prediction dataframe 

civilprediction.bush #View Bush's actual versus predicted civil rights spending

#Get the accuracy of the model, as presented in the OA-S table for poverty spending predictions
civilforecast <- forecast(civilpredict.bush, mydata, level = .95)
civilacc <- accuracy(civilforecast, bush$civil)
print(civilacc) #Examine the MAPE

totalbush <- sum(civilprediction.bush$actual) #Calculate Bush's total predicted spending
print(totalbush)

totalbushpred <- sum(civilprediction.bush$fit) #Calculate Bush's total predicted spending
print(totalbushpred)

diffbush <- totalbush - totalbushpred #Get differences between predicted and actual civil proposals
print(diffbush)


# PLOT BUSH'S PREDICTED VS. ACTUAL SPENDING (OA-T, FIGURE FOR BUSH'S ACTUAL VERSUS PREDICTED PROPOSALS)------------------

povertyprediction.bush #View the dataframes that will be used for graphing
civilprediction.bush

dev.off() #Reset graphical parameters to default
par(mar=c(4.1,4.1,2.75,.75)) #Set margins
par(mfrow=c(2,1)) #Establish aesthetic parameters of the figure; one column and three rows of graphs
par(family = "serif") #Set text

plotCI(povertyprediction.bush$budgetyear, povertyprediction.bush$fit, ylim = c(50000, 200000), yaxt ="n", ylab = "Millions of 2016 Dollars", xlab = "Year", li = povertyprediction.bush$lwr, ui = povertyprediction.bush$upr, cex.axis = .85, cex.lab = .85) #Plot predictions with predictions invtervals
points(povertyprediction.bush$budgetyear, povertyprediction.bush$actual, pch = 16) #Add points for predicted spending
lines(povertyprediction.bush$budgetyear, povertyprediction.bush$fit, lty = 2) #Add a line for predicted spending
lines(povertyprediction.bush$budgetyear, povertyprediction.bush$actual, lty = 1) #Add line for spending spending
legend("topleft", bty = "n", lty = c(1, 1), c("G.W Bush's Predicted Spending", "G.W Bush's Actual Spending"), pch = c(21, 16), cex = .85) #Add a legend
axis(2, at = c(50000,100000, 150000, 200000, 250000), c("50K", "100K", "150K", "200K", "250K"), cex.lab = .85, cex.axis = .85) #Add the x-axis labels
title("Anti-Poverty Spending", line = .75, cex.main = .9)


plotCI(civilprediction.bush$budgetyear, civilprediction.bush$fit, ylim = c(1100,1700), ylab = "Millions of 2016 Dollars", xlab = "Year", li = civilprediction.bush$lwr, ui = civilprediction.bush$upr, cex.axis = .85, cex.lab = .85) #Plot predictions with predictions invtervals
points(civilprediction.bush$budgetyear, civilprediction.bush$actual, pch = 16) #Add points for predicted spending
lines(civilprediction.bush$budgetyear, civilprediction.bush$fit, lty = 2) #Add a line for predicted spending
lines(civilprediction.bush$budgetyear, civilprediction.bush$actual, lty = 1) #Add line for actual spending
legend("topleft", bty = "n", lty = c(1, 1), c("G.W Bush's Predicted Spending", "G.W Bush's Actual Spending"), pch = c(21, 16), cex = .85) #Add a legend
title("Civil Rights Spending", line = .75, cex.main = .9)






