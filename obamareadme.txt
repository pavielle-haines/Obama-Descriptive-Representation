##################################################################
Replication Materials for:
“I’m not the President of Black America”: Rhetorical Versus Policy Representation
By Pavielle E. Haines, Tali Mendelberg, Bennett Butler
##################################################################

This file describes the materials that can be used to replicate the analyses presented in the paper and appendices.


########################################
Data Files
########################################

Reproduction of the analyses requires six data files:

1) obamayearlyspending.csv
2) obamaadminspending.csv
3) obamayearlyrhetoric.csv
4) obamatermrhetoric.csv
4) obamaadminrhetoric.csv
5) obamaicreliability.csv
6) obamarollcallobject.rds


The data file “obamayearlyspending.csv” is an Excel file that can be read into R. It contains yearly data for the presidents’ proposed poverty and civil rights spending from 1970 through 2017. Each row entry corresponds to one budget year. For each budget year, there are seven column entries for presidential budget proposals:(1 & 2) total proposed poverty and civil rights spending; (3, 4, & 5) proposed spending per relevant person (per poor person and per unemployed for poverty and per African American for civil rights); and (6 & 7) proposed poverty and civil rights spending as a percent of the discretionary domestic budget. The file also contains column entries for relevant social and political factors in each year. This file can be used to reproduce Table 2 and Figure 5 in the paper, as well as many of the analyses in the online appendix.

The data file “obamaadminspending.csv” is an Excel file that can be read into R. It contains data on each presidential administration’s average proposed poverty and civil rights spending. Each row entry corresponds to one presidential administration. For each administration, there are seven column entries for that president’s average budget proposals: (1 & 2) total proposed poverty and civil rights spending; (3, 4, & 5) proposed spending per relevant person (per poor person and per unemployed for poverty and per African American for civil rights); and (6 & 7) proposed poverty and civil rights spending as a percent of the discretionary domestic budget. This file can be used to reproduce Figures 3 and 4 in the paper.

The data file “obamayearlyrhetoric.csv” is an Excel file that can be read into R. It contains yearly speech data from the president’s State of the Union address from 1965 through 2016. Each row entry corresponds to a State of the Union address. For each address, there are five column entries detailing its content: (1) the percentage of words devoted to poverty; (2) the percentage of words devoted to a negative poverty references; (3) the percentage of words devoted to race; (4) the percentage of words devoted to negative racial references; and (5) the percentage of words devoted to the middle class.  The file also contains column entries for relevant social and political factors in the year leading up to the speech. This file can be used to reproduce the many of the t-tests presented in the paper and as well as analyses in the online appendix.

The data file “obamatermrhetoric.csv” is an Excel file that can be read into R. It contains term averages for speech data from the president’s State of the Union address from 1965 through 2016. Each row entry corresponds to one presidential term. For each term, there are five column entries detailing the average content of the president’s State of the Union addresses: (1) the average percentage of words devoted to poverty; (2) the average percentage of words devoted to negative poverty references; (3) the average percentage of words devoted to race; (4) the average percentage of words devoted to a negative racial references; (5) and the average percentage of words devoted to the middle class. The file also contains column entries for average, relevant social and political factors in each term. This file can be used to reproduce Figures 1 and 2, Table 1, and many of the analyses in the online appendix.

The data file “obamaadminrhetoric.csv” is an Excel file that can be read into R. It contains administration averages for speech data from the president’s State of the Union address from 1965 through 2016. Each row entry corresponds to one presidential administration. For each administration, there are five column entries detailing the average content of the president’s State of the Union addresses: (1) the average percentage of words devoted to poverty; (2) the average percentage of words devoted to negative poverty references; (3) the average percentage of words devoted to race; (4) the average percentage of words devoted to a negative racial references; (5) and the average percentage of words devoted to the middle class. The file also contains column entries for average, relevant social and political factors in each term. This file can be used to reproduce descriptive statistics in the paper and t-tests in the online appendix.

The data file “obamaicreliability.csv” is an Excel file that can be read into R. It contains content analysis data from two coders for 10% of the State of the Union speeches given between 1965 and 2016. Each row entry corresponds to a speech, with ten column entries for presidential rhetoric: (1 & 2) the average percentage of words devoted to poverty as recorded by coder one and two; (3 & 4) the average percentage of words devoted to negative poverty references as recorded by coder one and two; (5 & 6) the average percentage of words devoted to race as recorded by coder one and two; (7 & 8) the average percentage of words devoted to a negative racial references as recorded by coder one and two; and (9 & 10) the average percentage of words devoted to the middle class as recorded by coder one and two. This file can be used to reproduce the interceder reliability analyses in the online appendix.

The data file “obamarollcallobject.rds” is an R object. It was created in R and can be read back in. It contains a roll-call object that can be used to generate ideological scalings using the wnominate package. The data includes a subset of roll-call votes and the corresponding vote (or position taken) for Senator, Representatives, and Presidents through the 113th Congress. The roll-call votes included in the object are those related to poverty or civil rights. These roll-call votes were identified using the Nominate issue codes described in online appendix F. This file can be used to reproduce the DW-Nominate scores for poverty and civil rights in the online appendix.

Although each file is provided in full and contains all the necessary variables for replication, you can recreate derived datasets and variables using the provided R script. For instance, the code used to derive “obamaadminspending.csv” from “obamayearlyspending.csv” is included in the R script. Similarly, the percentage of words devoted to various issue areas can be recalculated using the provided R script. Finally, code is provided to reproduce “obamarollcallobject.rds” should the user wish to undertake this time intensive process.


########################################
R Script
########################################

The file “obamareplication.R” contains all the necessary code for reproducing the tables and figures in the main paper and online appendix. Before attempting to run the code, ensure that the correct version of R is installed, that you have downloaded all required files, and that the R code and data files are saved to the same location.

The analyses were conducted using R version 3.3.3. You may be required to install additional packages, as indicated in the code, to run the script.

The script is divided into multiple sections and subsections corresponding to each task and analysis. Detailed markup describes the purpose of each chunk of code:

-The first section includes the code required to install all necessary packages. This section of code only needs to be run the first time the script is opened and may subsequently be skipped.

-The second section includes the code required to call the required packages and read in the datasets. It must be run each time the script is opened before any subsequent code can be run. Be sure to set the working directory to the location of your data files before attempting to read them in.

-The third section includes the code for calculating and generating derived variables and datasets. It is *not* necessary to run this section of script prior to running subsequent code.

-The fourth section includes the code necessary for replicating every analysis presented in the paper. Each subsection is clearly labeled to correspond to the results presented in a table, graph, or as part of the text. The code in each subsection functions independently, so (unless otherwise noted) subsections can be in any desired order. Note that some subsections of code will take upwards of five minutes to run.

-The fifth section includes the code necessary for replicating every analysis presented in the online appendix. Each subsection is clearly labeled to correspond to the results presented in a table, graph, or as part of the text. The code in each subsection functions independently, so (unless otherwise noted) subsections can be in any desired order. Note that some subsections of code will take upwards of five minutes to run. The code to reproduce DW-Nominate scores may take hours or days to run.


########################################
User Guides for Data and Sources
########################################

The file “Obama Codebook for Data Files.pdf” contains detailed information about all the variables included in the datasets and used the analyses.

The file “Obama Data Sources and Collection Procedures.pdf” contains detailed information about the sources and processes used to generate the datasets. It provides detailed instructions on how to recreate the datasets provided here.
