#this is reading the file from excel
setwd("C:/Users/Caroline/Desktop/Field Museum Files/")
library(readxl)
library(MANOVA.RM)
multiplesheets <- function(fname){
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  names(data_frame) <- sheets
  print(data_frame)
}

#this is re-arranging the sheet so I can use it
morphsheetsloc <- "measurements (summer 2022).xlsb.xlsx"
morphsheetsraw <- multiplesheets(morphsheetsloc)

View(morphsheetsraw[[5]])
females <- morphsheetsraw[[5]][1:9,1:26]
View(females)

males <- morphsheetsraw[[5]][13:21,1:26]
View(males)
XenMinraw <- cbind(males, females)
XenMin <- as.data.frame(t(XenMinraw))
View(XenMin) 

#first variable (wing cord) for t testing
wingcF <- as.numeric(XenMin[XenMin[,2] == "F", 9])
wingcM <- as.numeric(XenMin[XenMin[,2] == "M", 9])
View(wingcF)

wing.cord.tt <- t.test(wingcF, wingcM,alternative = "two.sided", var.equal = FALSE)
summary(XenMintt) 

#second variable (bill length)
bill.lengthF <- as.numeric(XenMin[XenMin[,2] == "F", 3])
bill.lengthM <- as.numeric(XenMin[XenMin[,2] == "M", 3])

bill.length.tt <- t.test(bill.lengthF, bill.lengthM, alternative = "two.sided", var.equal = FALSE)

#third variable (bill width)
bill.widthF <- as.numeric(XenMin[XenMin[,2] == "F", 4])
bill.widthM <- as.numeric(XenMin[XenMin[,2] == "M", 4])

bill.width.tt <- t.test(bill.widthF, bill.widthM, alternative = "two.sided", var.equal = FALSE) 

#fourth variable (bill depth) 
bill.depthF <- as.numeric(XenMin[XenMin[,2] == "F", 5])
bill.depthM <- as.numeric(XenMin[XenMin[,2] == "M", 5])

bill.depth.tt <- t.test(bill.depthF, bill.depthM, alternative = "two.sided", var.equal = FALSE) 

#fifth variable (tarsus length)  
tar.lengthF <- as.numeric(XenMin[XenMin[,2] == "F", 6])
tar.lengthM <- as.numeric(XenMin[XenMin[,2] == "M", 6])

tar.length.tt <- t.test(tar.lengthF, tar.lengthM, alternative = "two.sided", var.equal = FALSE) 

#sixth variable (tarsus depth)
tar.depthF <- as.numeric(XenMin[XenMin[,2] == "F", 7])
tar.depthM <- as.numeric(XenMin[XenMin[,2] == "M", 7])

tar.depth.tt <- t.test(tar.depthF, tar.depthM, alternative = "two.sided", var.equal = FALSE)

#seventh variable (tail length) 
tail.lengthF <- as.numeric(XenMin[XenMin[,2] == "F", 6])
tail.lengthM <- as.numeric(XenMin[XenMin[,2] == "M", 6])

tail.length.tt <- t.test(tail.lengthF, tail.lengthM, alternative = "two.sided", var.equal = FALSE) 

#extra stuff from before, dont want to delete just in case
#tailF <- XenMin[]
#xenminpvals <- list()
#xenminpvals[["wingc"]] <- XenMintt$p.value

#View(morphsheetsraw[[1]])

#DigCya <- as.data.frame(morphsheetsraw[[1]])
#DigCyaM <- DigCya[DigCya$sex == "M",]
#DigCyaF <- DigCya[DigCya$sex == "F",]

#t.test(x = DigCyaF$'bill1(length)', y = DigCyaM$'bill1(length)')

#temp <- lm(DigCya$`bill1(length)` ~ DigCya$sex)
#summary(temp)
#temp2 <- summary(manova(bill1(length) + bill2(width) + bill3(depth) ~ sex, data = DigCya))




