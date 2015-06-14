
# Create sepsis data ------------------------------------------------------

# Load some libraries
library(VirtualTwins)
library(randomForest) 

# Sepsis is a csv file available in SIDES example to this address: 
# http://biopharmnet.com/wiki/Software_for_subgroup_identification_and_analysis
# type ?sepsis to see details
# I downloaded zip file and extract the sepsis.csv in data-raw folder. 
sepsis.csv <- read.csv(file = "data-raw/sepsis.csv", na.strings = ".")

# Check data
str(sepsis.csv)

# Count NA's
sum(is.na(sepsis.csv))
# No NA's in outcome
sum(is.na(sepsis.csv$survival))

# For futures computation i need to impute missing values
# I use random forest imputation with randomForest package with simple parameters
# I need to make survival field as factor
sepsis.csv$survival <- factor(sepsis.csv$survival, levels = 0:1) 
sepsis.imp <- with(sepsis.csv, rfImpute(y = survival, x = sepsis.csv[, -1], iter = 5, ntree = 500))
str(sepsis.imp)

# Change THERAPY levels in 0/1 and type will be numeric
sepsis.imp$THERAPY <- with(sepsis.csv, ifelse(THERAPY == "control", 0, 1))

# Change survival into numeric values
sepsis.imp$survival <- as.numeric(as.character(sepsis.imp$survival))

# Check data
sepsis <- formatRCTDataset(sepsis.imp, "survival", "THERAPY", T)

# Save file into data folder
save(sepsis, file = "data/sepsis.rdata")

# End.

