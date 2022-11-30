#produces MARK input files as .RDS file in a /For MARK directory


library(data.table)
library(anytime)

## load the effort table (date, observer(s))
effort <- setDT(fread("./2022 survey data/Observer2022.csv"))
effort$DATE<- anydate(effort$DATE) # need to add since not reading in DATE col correctly.
effort[,fDate := as.IDate(DATE)]

## load the raw data in field form format (record ID, date, patch, bug number, sex, status)
## status in {Marked, Resighted, Unmarked}
dat4 <- setDT(fread("./2022 survey data/resight2022.csv"))
dat4$DATE<- anydate(dat4$DATE) # need to add since not reading in DATE col correclty.
dat4[,fDate := as.IDate(DATE)]

## some simple data descriptions
datDesc <- merge(dat4[,.N,year(fDate)],
                 dat4[STATUS == "Unmarked",.(unm = .N),year(fDate)])
datDesc[,pUnm := unm/N]
datDesc

dat4[STATUS == "Marked", .N, .(year(fDate), PATCH)][order(PATCH, year)]

## develop encounter history file for each year

    y <- 2022
    
    ## effort for the year
    effortY <- effort[year(fDate) == y, fDate]
    ## ensure it is continuous
    if(!all(diff(effortY) == 1)) effortY <- seq(min(effortY), max(effortY), by = "day")
    
    ## mark-resight for the year
    dat5 <- dat4[year(fDate) == y & STATUS %in% c("Marked", "Resighted")]
    
    ## set up array with rows for individuals and columns for days, initially populated with zeros
    dat6 <- array(data = 0,
                  dim = c(length(unique(dat5[,BUGNO])),
                          length(effortY)),
                  dimnames = list(unique(dat5[,BUGNO]),
                                  sub(paste( y, "-", sep = ""), "", effortY)))
    ## fill in 1 for individuals on days they were marked or resighted
    for(j in 1:nrow(dat6)){
        ## records for each individual
        dat5Int <- dat5[BUGNO == dimnames(dat6)[[1]][j]]
        dat6[j, match(dat5Int[,fDate], effortY)] <- 1
    }
    
    # ## daily counts, two ways (can verify processing to array)
    # dat5[, .N, fDate]
    # apply(dat6, 2, sum)
    ## format as encounter history as expected in Program MARK
    dat7 <- data.table(BUGNO = as.integer(dimnames(dat6)[[1]]),
                       ch = apply(dat6, 1, function(f){
                           paste(f, collapse = "")}))
    ## attach sex
    coVariates <- unique(dat5[, c("BUGNO", "SEX")])
    dat7 <- merge(dat7, coVariates)
    names(dat7) <- tolower(names(dat7))
    saveRDS(dat7, paste0("./2022 survey data/inpFile", y, ".RDS"))
