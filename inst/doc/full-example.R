## ---- eval = F-----------------------------------------------------------
#  vt.data <- function(dataset, outcome.field, treatment.field, interactions = TRUE, ...){
#    data <- formatRCTDataset(dataset, outcome.field, treatment.field, interactions = TRUE)
#    VT.object(data = data, ...)
#  }

## ------------------------------------------------------------------------
# load library VT
library(VirtualTwins)
# load data sepsis
data(sepsis)
# initialize VT.object
vt.o <- vt.data(sepsis, "survival", "THERAPY", TRUE)

## ------------------------------------------------------------------------
# Creation of categorical variable
cat.x <- rep(1:5, (nrow(sepsis))/5)
cat.x <- as.factor(cat.x)
sepsis.tmp <- cbind(sepsis, cat.x)
vt.o.tmp <- vt.data(sepsis.tmp, "survival", "THERAPY", TRUE)

## ---- echo = FALSE-------------------------------------------------------
rm(vt.o.tmp, cat.x, sepsis.tmp)

## ------------------------------------------------------------------------
# use randomForest::randomForest()
library(randomForest, verbose = F)
# Reproducibility
set.seed(123)
# Fit rf model 
# default params
# set interactions to TRUE if using interaction between T and X
model.rf <- randomForest(x = vt.o$getX(interactions = T),
                         y = vt.o$getY())
# initialize VT.forest.one
vt.f.rf <- VT.forest.one(vt.o, model.rf)

## ------------------------------------------------------------------------
# # use randomForest::randomForest()
# library(party, verbose = F)
# # Reproducibility
# set.seed(123)
# # Fit cforest model 
# # default params
# # set interactions to TRUE if using interaction between T and X
# model.cf <- cforest(formula = vt.o$getFormula(), data = vt.o$getData(interactions = T))
# # initialize VT.forest.one
# vt.f.cf <- VT.forest.one(vt.o, model.cf)

## ------------------------------------------------------------------------
# Copy new object
vt.o.tr <- vt.o$copy()
# Change levels
tmp <- ifelse(vt.o.tr$data$survival == 1, "y", "n")
vt.o.tr$data$survival <- as.factor(tmp)
rm(tmp)
# Check new data to be sure
formatRCTDataset(vt.o.tr$data, "survival", "THERAPY")
# use caret::train()
library(caret, verbose = F)
# Reproducibility
set.seed(123)
# fit train model
fitControl <- trainControl(classProbs = T, method = "none")
model.tr <- train(x = vt.o.tr$getX(interactions = T),
                  y = vt.o.tr$getY(),
                  method = "rf",
                  tuneGrid = data.frame(mtry = 5),
                  trControl = fitControl)
# initialize VT.forest.one
vt.f.tr <- VT.forest.one(vt.o.tr, model.tr)

