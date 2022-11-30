# code to recreate analysis from Henry et al. 2022 Mark‑recapture study and habitat assessment for the northern metalmark butterfly, Calephelis borealis (Lepidoptera: Riodinidae)

library(data.table)
library(anytime)
library(RMark)

##### PREPARE THE DATA FOR ANALYSIS ######

## declare which year this analysis is for (derived from multi-year analysis but updated for annual)
focalYr <- 2022

## load the prepared inp file from prepareInp.R

mm2022 <- readRDS("./2022 survey data//inpFile2022.RDS")
## sex as factor
dat7[,sex := factor(sex, levels = c("F", "M"))] # set factor levels for butterfly sex
dat7[, .N, sex] # n.b. = some marked butterflies did not have sex determined

## also load observer file
# Note that because all but one date was collected by same observer we will be unable to use observer as covariate
observer <- setDT(fread("./2022 survey data/Observer2022.csv"))
observer$DATE<- anydate(observer$DATE) # need to add since not reading in DATE col correctly.
observer[,DATE := as.IDate(DATE)]
(obsY <- observer[year(DATE) == focalYr])
obs2 <- data.frame(time = as.numeric(obsY[,DATE] - obsY[1, DATE]) + 1,
                   obs = obsY[,OBSERVER])

##### PROCESS THE DATA FOR ANALYSIS USING RMARK ######

## process data and make design
source("./2022 survey data/MARKformulas2022.r") # load in MARK model structures

## CJS without sex as a covariate
NoMe.CJS1 <- process.data(mm2022) #Process encounter history dataframe for MARK analysis
NoMe.CJS1.ddl <- make.design.data(NoMe.CJS1) #Create design dataframes for MARK model specification

##### FIT MODELS TO ESIMATE SURVIVAL (PHI) AND RESIGHTING PROB. (P) IN RMARK ######

# phi: probability of surviving from one occasion to another 
# p = probability of resighting

# Survival and resighting is similar between sexes and over time 
NoMe.phidot.pdot <- mark(NoMe.CJS1,
                         NoMe.CJS1.ddl,
                        model.parameters = list(Phi = Phidot, p = pdot))

# Survival is similar between sexes but resighting varies over time 
NoMe.phidot.ptime <- mark(NoMe.CJS1,
                          NoMe.CJS1.ddl,
                          model.parameters = list(Phi = Phidot, p = ptime))

## build model selection table
if(exists("msCJS1")) rm(msCJS1)
(NoMe.list <- ls()[sapply(ls(), function(f) class(get(f))[1] == "mark")])
for(i in NoMe.list){
    if(exists("markModelInt")) rm(markModelInt)
    markModelInt <- get(i)
    msInt <- data.frame(object = i,
                        model = markModelInt$model.name,
                        npar = markModelInt$results$npar,
                        deviance = markModelInt$results$deviance,
                        AICc = markModelInt$results$AICc)
    if(!exists("msCJS1")){
        msCJS1 <- msInt
    }else{
        msCJS1 <- rbind(msCJS1, msInt)
    }
}
msCJS1$deltaAICc <- msCJS1$AICc - min(msCJS1$AICc)
(msCJS1 <- msCJS1[order(msCJS1$AICc),])

## Plot p(detection) over time
plot(NoMe.phidot.ptime$results$real$estimate[2:(nrow(obsY)-1)],
     type = "o", pch = 16,
     xaxt = "n", xlab = "",
     ylab = "p(detection)", ylim = c(0, 1), yaxs = "i")
axis(1, at = 1:(nrow(obsY)-1), las = 2, labels = obsY[2:.N,DATE])
points(NoMe.phidot.ptime$results$real$ucl[2:(nrow(obsY)-1)], pch = 2, type = "o")# upper CL
points(NoMe.phidot.ptime$results$real$lcl[2:(nrow(obsY)-1)], pch = 6, type = "o")# lower CL

## Fit CJS model _with_ sex as a covariate -- 

NoMe.CJS2 <- process.data(mm2022, groups = "sex")
NoMe.CJS2.ddl <- make.design.data(NoMe.CJS2)

## Fit models adding sex as factor

# Survival and resighting is similar between sexes and over time 
NoMe.phidot.pdot2 <- mark(NoMe.CJS2,
                          NoMe.CJS2.ddl,
                          model.parameters = list(Phi = Phidot, p = pdot))

# Survival is similar between sexes but resighting varies over time
NoMe.phidot.ptim2 <- mark(NoMe.CJS2,
                          NoMe.CJS2.ddl,
                          model.parameters = list(Phi = Phidot, p = ptime))

# Survival varies between sexes but resighting is same over time
NoMe.phisex.pdot <- mark(NoMe.CJS2,
                         NoMe.CJS2.ddl,
                         model.parameters = list(Phi = Phisex, p = pdot))

# Survival is similar between sexes but resighting varies by sex
NoMe.phidot.psex <- mark(NoMe.CJS2,
                         NoMe.CJS2.ddl,
                         model.parameters = list(Phi = Phidot, p = psex))

## build model selection table
if(exists("msCJS2")) rm(msCJS2)
if(exists("markModelInt")) rm(markModelInt)
(NoMe.l2st <- setdiff(ls()[sapply(ls(), function(f) class(get(f))[1] == "mark")],
                      NoMe.list))
for(i in NoMe.l2st){
    if(exists("markModelInt")) rm(markModelInt)
    markModelInt <- get(i)
    msInt <- data.frame(object = i,
                        model = markModelInt$model.name,
                        npar = markModelInt$results$npar,
                        deviance = markModelInt$results$deviance,
                        AICc = markModelInt$results$AICc)
    if(!exists("msCJS2")){
        msCJS2 <- msInt
    }else{
        msCJS2 <- rbind(msCJS2, msInt)
    }
}
msCJS2$deltaAICc <- msCJS2$AICc - min(msCJS2$AICc)
(msCJS2 <- msCJS2[order(msCJS2$AICc),])

## Plot p(detection) over time from top model (NoMe.phidot.ptim2)
plot(NoMe.phidot.ptim2$results$real$estimate[2:(nrow(obsY)-1)],
     type = "o", pch = 16,
     xaxt = "n", xlab = "",
     ylab = "p(detection)", ylim = c(0, 1), yaxs = "i")
axis(1, at = 1:(nrow(obsY)-1), las = 2, labels = obsY[2:.N,DATE])
points(NoMe.phidot.ptim2$results$real$ucl[2:(nrow(obsY)-1)], pch = 2, type = "o")
points(NoMe.phidot.ptim2$results$real$lcl[2:(nrow(obsY)-1)], pch = 6, type = "o")

## Get coefficient estimates by sex
NoMe.phidot.psex$results$real

##### FIT MODELS TO ESIMATE ABUNDANCE (N) IN MARK ######

## set up data for POPAN models
NoMe.POPAN <- process.data(mm2022, model = "POPAN")
NoMe.POPAN.ddl <- make.design.data(NoMe.POPAN)
#NoMe.POPAN.ddl$p <- merge_design.covariates(NoMe.POPAN.ddl$p, obs2)

# estimate N assuming model parms are constant
NoMe.fmP <- mark(NoMe.POPAN,
                 NoMe.POPAN.ddl,
                 model.parameters = list(Phi = Phidot, p = pdot,
                                         pent = pentdot, N = Ndot))
NoMe.fmP$results$real
NoMe.fmP$results$derived$`Gross N* Population Size`

## estimate abundance with sex as group
NoMe.POPANsx <- process.data(mm2022, model = "POPAN", groups = "sex")
NoMe.POPANsx.ddl <- make.design.data(NoMe.POPANsx)

# estimate N assuming model parms are constant but grouped by sex
NoMe.fmPalt <- mark(NoMe.POPANsx,
                    NoMe.POPANsx.ddl,
                    model.parameters = list(Phi = Phidot, p = pdot,
                                            pent = pentdot, N = Ndot))
# estimate N assuming resighting varies by sex but other parms are constant

NoMe.fmPsx <- mark(NoMe.POPANsx,
                   NoMe.POPANsx.ddl,
                   model.parameters = list(Phi = Phidot, p = psex,
                                           pent = pentdot, N = Ndot))
NoMe.fmPsx$results$real
NoMe.fmPsx$results$derived$`Gross N* Population Size`
sapply(NoMe.fmPsx$results$derived$`Gross N* Population Size`, sum)

## model selection between those two
for(i in c("NoMe.fmPalt", "NoMe.fmPsx")){
    if(exists("markModelInt")) rm(markModelInt)
    markModelInt <- get(i)
    msInt <- data.frame(object = i,
                        model = markModelInt$model.name,
                        npar = markModelInt$results$npar,
                        deviance = markModelInt$results$deviance,
                        AICc = markModelInt$results$AICc)
    if(!exists("msPPN")){
        msPPN <- msInt
    }else{
        msPPN <- rbind(msPPN, msInt)
    }
}
msPPN$deltaAICc <- msPPN$AICc - min(msPPN$AICc)
(msPPN <- msPPN[order(msPPN$AICc),])



## population estimates from top model (NoMe.fmPsx)
NoMe.fmPsx$results$real[1:5,]
NoMe.fmPsx$results$derived$`Gross N* Population Size`
sapply(NoMe.fmPsx$results$derived$`Gross N* Population Size`, sum)

## population estimates from other model (NoMe.fmPalt)
NoMe.fmPalt$results$real[1:4,]
NoMe.fmPalt$results$derived$`Gross N* Population Size`
sapply(NoMe.fmPalt$results$derived$`Gross N* Population Size`$estimate, sum)

