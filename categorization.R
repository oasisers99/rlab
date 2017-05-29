# Install and load necessary libraries
# install.packages("stringr")
# install.packages("data.table")
install.packages("file.split")
library("data.table")
library("stringr")
library("file.split")

#Set working directori
setwd("C:/lab/finance_transactions")

################ sample file (10%) ###################
#Read and quick view of the 10 percent-sized file 1.2 million rows
ft <- fread(file="transaction_records_vol_1.csv", sep = ",", fill = TRUE)

str(ft)
ncol(ft)
nrow(ft)
head(ft,5)

#Remove columns not useful - id, bank
ft$id <- NULL
ft$bank <- NULL
ncol(ft)

#Add categorization flag at the right most
#Default 0 meaning 'not categorized'
ft$cat <- 0

ft_splited <- split(ft, 1:5)
ft_splited$`1`


#Regular expression to categorize
result_grep <- grep('((.*(golf|night|dance).*(club).*)|(.*\\b(oval|circus|zoo)\\b.*)|.*(stadium|museum|cinema|theatre|multiplex|convention|entertainment|bowling|aquarium).*|(.*(sea|underwater|splash|adventure|aqua|water).*(world|park).*)|.*\\b(hoyts|kingpin|dreamworld|waterworld)\\b.*)', ft$text, value = TRUE)
write.csv(result_grep, "entertainment_result.csv")
################ sample file (10%) ###################


splited <- split(1:10, 1:5)
splited$`1`




################ full file (100%) ###################
## Currently it shows hanging symptom. I can consider to develop some application ##
ftall <- fread(file="transaction_records_all.csv", sep = ",", fill = TRUE)

#nrow(ftall)
#head(ftall[,4],5)

result_grep_all <- grep('charity|donation|red\\scros|cancer\\sCouncil|(The\\s)?Salvation(-|\\sar)|Ronald\\sMa?cdonald\\sHous|Fred\\s*Hollows|The\\sSmith\\sFamily|World\\s*Vision|St\\sVincents?\\sDe\\sPaul', ftall[,4], value = TRUE)
write.csv(result_grep_all, "charity_result.csv")
################ full file (100%) ###################


################ Testing panel ###################

file.split


################ Testing panel ###################
