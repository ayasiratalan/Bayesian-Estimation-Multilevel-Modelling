#install.packages("formatR")
library(formatR)
# REPLICATION CODES FOR BAYESIAN STATISTICS FINAL PROJECT
# ABDULLAH YASIR ATALAN
# w

# 1- DATA CLEANING

# 2- DATA MERGE

# 3- SUMMARY STATISTICS of the DATA

# 4- ARE THERE ANY MISSING DATA, and are they correlated?

# 5- If there are, use Mice for multiple imputation

# 6- USE STANDARD REGRESSION THAT YOU WILL USE FOR CONDUCT 3: Possibly negative binomial model 

# 7- For the same model use MCMC JAGS,
# FOR THIS: list all covariates
# Decide on random intercept, slope?
# Write model
# Priors
# Parameters

# After estimations, do some model checking with superdiag?
# After that compare the results with the base model

# Some information

####################################################################################################
####################################################################################################
####################################################################################################

# REPLICATION CODES FOR CONDUCT 3 FINAL PROJECT
# ~ ABDULLAH YASIR ATALAN
# Title: Does coups lead to military purges in border countries?

rm(list=ls())

# Necessary Packages
library(readr)
library(tidymodels)
library(countrycode)
library(MASS)
library(plm)

# Import purge dataset
purge <-read_csv("~/Desktop/ACADEMIA/AMERICAN UNIVERSITY/Conduct III/Final Paper/Jun Sudduth/analysis_tscs_v1.csv")


###Bayes icin gerekli variablelar
# purge %>% colnames()
# purge$ln_milex
# purge$democracy
# purge$rgdpe
# purge$tenure
# purge$ln_milper
# purge$ln_solpay






# CODING
purgeSize <- NA

#purge$size_purge_year <- purge$size_purge_year %>% as.factor()
#purge$size_purge_year %>% levels()

purgeSize <- ifelse(purge$size_purge_year=="One officer",1,
                    ifelse(purge$size_purge_year=="less than 10",2,
                           ifelse(purge$size_purge_year=="less than 100",3,
                                  ifelse(purge$size_purge_year=="more than 100",4,purge$size_purge_year))))


purgeSize <- ifelse(is.na(purgeSize),0,purgeSize)
purge$purgeSize1 <- purgeSize
purge$purgeSize1 <- purge$purgeSize1 %>% as.numeric()
#purge$purgeSize <-  purge$purgeSize %>% as.factor()
#purge$purgeSize <- purge$purgeSize %>% factor(ordered =T )


purge$rank_officer_year <- ifelse(is.na(purge$rank_officer_year),"no purge",purge$rank_officer_year)
purge$purgeRank <- purge$rank_officer_year %>% as.factor()

purge$top_high_purge <- ifelse(purge$purgeRank%in%c("top-rank officers purged","high-rank officers purged"),1,0)


# Import country borders dataset
country_borders <- read_csv("~/Desktop/ACADEMIA/AMERICAN UNIVERSITY/Conduct III/Final Paper/country-borders-master/GEODATASOURCE-COUNTRY-BORDERS.CSV")

# turn iso2c country code to cown in borders data

country_borders$ccode <-  countrycode(sourcevar = country_borders$country_code,origin = "iso2c",destination = "cown")

country_borders$border_ccode <- countrycode(sourcevar = country_borders$country_border_code,origin = "iso2c",destination = "cown")

# remove rows with NAs in  ccode and store the dataframe as borders2
borders2 <- country_borders[!is.na(country_borders$ccode),]

# First turn purge data to country-year format save as purge1
purge1 <- purge %>% group_by(ccode,year) %>%
    summarise(purge=first(purge_v1),
              violent_purge=first(violent_purge_v1),
              purgeSize=first(purgeSize1),
              purge_top=first(purge_top),
              top_high_purge=first(top_high_purge),
              transleader=first(transleader),
              cgv_regime1=max(cgv_regime),
              cgv_regime2=min(cgv_regime),
              tenure1=max(tenure),
              tenure2=min(tenure),
              ln_milex=first(ln_milex),
              democracy=first(democracy),
              rgdpe=first(rgdpe),
              ln_milper=first(ln_milper),
              ln_solpay=first(ln_solpay))









# Import coups dataset
coup <- read.delim("http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt")

# Rename column
coup <- coup %>%
    rename(border_ccode = ccode)

# put 0 for NA values in borders dataset
borders2$border_ccode[is.na(borders2$border_ccode)] = 0


#################################
#################################
#################################
########## LIST ALL BORDER COUNTRIES IN ONE ROW ( BUT I GAVE UP FROM THIS METHOD)
##########borders2$border_list_name <- NA
########## for (ii in unique(borders2$ccode)) {
########## borders2$country_border_code[borders2$ccode==ii] %>% list() -> borders2$border_list_name[borders2$ccode==ii] 
########## }
########## 
########## borders2$border_list_ccode <- NA
########## for (ii in unique(borders2$ccode)) {
##########   borders2$border_ccode[borders2$ccode==ii] %>% list() -> borders2$border_list_ccode[borders2$ccode==ii] }
########## purge2$border_list <- c()
########## purge2$border_list<- list(purge2$border_list)
########## 
########## for (k in unique(purge2$ccode)) { #HATALI
########## purge2$border_list[purge2$ccode==40] <- unique(borders2$border_list_ccode[borders2$ccode==40])
########## }
########## View(purge2)
########## View(borders2)
#################################
#################################
#################################


# Merge border countries with purge data and store as df
df <- merge(purge1,borders2,by="ccode",all.x = TRUE)

# Sort the data by ccode and year respectively
df <- df[
    with(df, order(ccode, year)),
]

# Create new column called coups
df$coups <- NA

# if there is any coups in border countries in a given year put coup variable into that column or 0 otherwise
for (ii in unique(df$year)) {
    df$coups[df$year==ii] <- ifelse(df$border_ccode[df$year==ii]%in%coup$border_ccode[coup$year==ii],coup$coup[coup$year==ii],0)
}

# Rename coups column with Successful,Unsuccessfuls or No coups
df$coups <- ifelse(df$coups==2,"S",df$coups)
df$coups <- ifelse(df$coups==1,"U",df$coups)
df$coups <- ifelse(df$coups==0,"No",df$coups)
df$coups <- as.factor(df$coups)

# finally create country year format data set with purge and coups (coups in border countries) data 
test <- df %>%
    dplyr::group_by(country_name, 
                    year,
                    ccode,
                    country_code)%>% 
    summarise(purge=first(purge),
              violent_purge=first(violent_purge),
              purgeSize=first(purgeSize),
              purge_top=first(purge_top),
              top_high_purge=first(top_high_purge),
              transleader=first(transleader),
              cgv_regime1=first(cgv_regime1),#if regime type change I want to keep both
              cgv_regime2=first(cgv_regime2),
              tenure1=first(tenure1),# same for leader tenures
              tenure2=first(tenure2),
              ln_milex=first(ln_milex),
              democracy=first(democracy),
              rgdpe=first(rgdpe),
              ln_milper=first(ln_milper),
              ln_solpay=first(ln_solpay),
              coupsU = sum(coups=="U"), coupsS=sum(coups=="S"), coupsN=sum(coups=="No"))




# The coup variable is coded as with the following criteria: 
#if at least one coup is successful, it is coded as successful.
#If there is at least 1 unsuccessful AND no successful coups, it is coded as unsuccessful,
#if there is no successful or unsuccessful coups, it is coded as No.

test$realcoup <- NA
test$realcoup <- ifelse(test$coupsU==0&test$coupsS==0,"No",test$realcoup)
test$realcoup <- ifelse(test$coupsU>0&test$coupsS==0,"U",test$realcoup)
test$realcoup <- ifelse(test$coupsU==0&test$coupsS>0,"S",test$realcoup)
test$realcoup <- ifelse(test$coupsU>0&test$coupsS>0,"S",test$realcoup)

# Turn the new column into factor variables
test$realcoup <- as.factor(test$realcoup)

# Drop unnecessary columns
test <- dplyr::select(test,-c(coupsU,coupsS,coupsN))




## Below I want to create a leader switch variable if any leaderchange occurs in that specific year (BUT I REALIZED THAT THIS WAS ALREADY IN THE DATA)
# counts <- test %>% count(country_name,year,ccode,realcoup)
# 
# counts1 <- na.omit(counts)
# 
# c2 <- counts1[counts1$n==2,] 
# 
# test$leader_change <- NA
# 
# for (ii in unique(test$year)) {
#   test$leader_change[test$year==ii] <-  ifelse(test$country_name[test$year==ii]%in%c2$country_name[c2$year==ii],"switch","noswitch")
# }
# 
# 
# View(test)



# Create another column with lagged coup variable
test2 <- test %>%
    group_by(ccode) %>% 
    mutate(lagged_realcoup = dplyr::lag(realcoup, n = 1, default = NA)) %>% as.data.frame()


test5 <- test2
# # remove rows with NA in coups since one row in each country will be NA after lagged one
# test3 <- na.omit(test2)
# 
# 
# # Purge olup da lider degisimi olanlar
# test3 %>% filter(test3$purge==1&transleader==1) 
# #Purge olup da regime tipi degisenler
# test3 %>% filter(test3$purge==1&cgv_regime1!=cgv_regime2) 
# 
# 
# 
# test4 <- test3 %>% dplyr::select(-cgv_regime1)
# test5 <- test4 %>% rename(cgv_regime=cgv_regime2)
# 


# test_Reverse1$realcoup_dummy <- ifelse(test_Reverse1$realcoup=="S"|test_Reverse1$realcoup=="U",1,0)
# test_Reverse1$realcoup_dummy <- as.factor(test_Reverse1$realcoup_dummy)
test5$lagged_realcoup_dummy <- ifelse(test5$lagged_realcoup=="S"|test5$lagged_realcoup=="U",1,0)
test5$lagged_realcoup_dummy <- as.factor(test5$lagged_realcoup_dummy)
test5$realcoup_dummy <- ifelse(test5$realcoup=="S"|test5$realcoup=="U",1,0)
test5$realcoup_dummy <- as.factor(test5$realcoup_dummy)

test_imputation <- test5
test_imputation <- test_imputation[!is.na(test_imputation$lagged_realcoup_dummy),]

library(mice)
library(miceadds)

test_imputation[sapply(test_imputation, is.infinite)] <- NA
mis <- subset(test_imputation,select=c("ln_milex","ln_milper","ln_solpay","rgdpe","purge","tenure1","transleader","lagged_realcoup","lagged_realcoup_dummy","democracy","ccode"))

colSums(is.na(mis)/nrow(mis))

sum(complete.cases(mis))/nrow(mis)


m <- 5
test_imputed<- mice( mis,m)

imputed1 <- complete(test_imputed)
a <- md.pattern(mis, plot = T,rotate.names =T )

knitr::kable(a)



# BAYESIAN ANALYSIS Variables JAGS
test5 <- na.omit(test5)

lapply(c("rjags","arm","coda","superdiag","R2WinBUGS","R2jags","lme4","MCMCpack"),
       library, character.only=TRUE)


y <- imputed1$purge
ln_milex <- imputed1$ln_milex
rgdpe <- imputed1$rgdpe
ln_milper <- imputed1$ln_milper
ln_solpay <- imputed1$ln_solpay
transleader <- imputed1$transleader
lagged_coup <- imputed1$lagged_realcoup
lagged_coup_dummy <- imputed1$lagged_realcoup_dummy
tenure <- imputed1$tenure1
democracy <- imputed1$democracy
n <- nrow(imputed1)
y_alt_vio <- imputed1$violent_purge
y_alt_top <- imputed1$top_high_purge

unique.country <- unique(imputed1$ccode)
J <- length(unique.country)

country <- rep(NA, J)
for (i in 1:J) country[imputed1$ccode == unique.country[i]] <- i




### MCMC LOGIT
jags.model  <- function()  {
    #first block: individual loop
    for (i in 1:n) {
        y[i] ~ dbin (mu.bound[i], 1)
        mu.bound[i] <- max(0, min(1, mu[i]))
        logit(mu[i]) <- alpha[country[i]]+
            beta[1]*rgdpe[i]+
            beta[2]*ln_milper[i]+
            beta[3]*ln_milex[i]+
            beta[4]*ln_solpay[i]+
            beta[5]*transleader[i]+
            beta[6]*tenure[i]+
            beta[7]*lagged_coup_dummy[i]+
            beta[8]*democracy[i]
    }
    #second block
    
    for (j in 1:J) {
        alpha[j] ~ dnorm(mu.alpha, tau.alpha)
    }
    #assign noninformative prior distributions.
    beta[1] ~ dnorm(0,0.0001)
    beta[2] ~ dnorm(0,0.0001)
    beta[3] ~ dnorm(0,0.0001)
    beta[4] ~ dnorm(0,0.0001)
    beta[5] ~ dnorm(0,0.0001)
    beta[6] ~ dnorm(0,0.0001)
    beta[7] ~ dnorm(0,0.0001)
    beta[8] ~ dnorm(0,0.0001)
    tau.alpha ~ dgamma(1.0,1)
    mu.alpha ~ dnorm(0,0.0001);
}
#save model
write.model(jags.model, "~/Desktop/ACADEMIA/AMERICAN UNIVERSITY/Bayesian Statistics-Gill/jags.model")

jags.param <- c("beta","tau.alpha", "mu.alpha")

#pass the data to JAGS
jags.list<- list(n, y, J, country,rgdpe,ln_milper,
                 ln_milex,ln_solpay,transleader,tenure,lagged_coup_dummy,democracy)

names(jags.list)<-c("n","y","J","country","rgdpe","ln_milper",
                    "ln_milex","ln_solpay","transleader","tenure",
                    "lagged_coup_dummy","democracy")

jags.inits <- function(){list( "tau.alpha" = 1, "mu.alpha" = rnorm(1))}

purge.out <- jags(data=jags.list, parameters.to.save=jags.param, inits = jags.inits,
                  n.iter=500, n.chains=3,
                  model="~/Desktop/ACADEMIA/AMERICAN UNIVERSITY/Bayesian Statistics-Gill/jags.model", DIC=TRUE)



autojags1 <- autojags(purge.out,n.chains=3)
results <- autojags1$BUGSoutput$summary
rownames(results) <- c("rgdpe","ln_milper","ln_milex","ln_solpay","transleader",
                       "tenure","lagged_coup_dummy","democracy",
                       "Deviance","Mu.alpha","Tau.alpha")

knitr::kable(results,format="latex", booktabs=TRUE )%>%   kable_styling(latex_options="scale_down")


# gln

summary(pool(glm.mids (purge ~ ln_milex + ln_milper + ln_solpay + 
                           rgdpe + transleader + democracy + tenure1+lagged_realcoup_dummy, data = test_imputed,family = binomial))
        
        
        ######
        # 1- test 2 na omit i degistir, sadece lag yillarla alakli
        # 2- NA li versiyonu ele al ve NA patternlerini cikar
        # 3- NA imputation yap vs mean?
        # 4- Analizi tekrarla
        
        ########################################
        ########## MISSING VALUES IMPUTATION
        #############################################
        # test2$purge %>% is.na() %>% sum()
        # test2$ln_milper %>% is.na() %>% sum()
        # test2$rgdpe %>% is.na() %>% sum()
        # test2$ln_solpay %>% is.na() %>% sum()
        # test2$ln_milex %>% is.na() %>% sum()
        
        # a <- ifelse(test2$ln_milex%in%NA,1,0)
        # b <- ifelse(test2$ln_milper%in%NA,1,0)
        # c <- ifelse(test2$ln_solpay%in%NA,1,0)
        # d <- ifelse(test2$rgdpe%in%NA,1,0)
        # df <- data.frame("milex.miss"=a,"milper.miss"=b,"solpay.miss"=c,"rgdpe.miss"=d)
        # matrix <- subset(test2,select=c("purge","violent_purge","purgeSize","tenure1","democracy","transleader"))
        # cor(matrix,df,use="pairwise.complete.obs")
        
        
        
        
        
        
        
        
        
        
        library(mice)
        library(miceadds)
        
        test_imputation[sapply(test_imputation, is.infinite)] <- NA
        mis <- subset(test_imputation,select=c("ln_milex","ln_milper","ln_solpay","rgdpe","purge","tenure1","transleader","lagged_realcoup","lagged_realcoup_dummy","democracy","ccode"))
        
        colSums(is.na(mis)/nrow(mis))
        
        sum(complete.cases(mis))/nrow(mis)
        
        
        m <- 5
        test_imputed<- mice( mis,m)
        
        imputed1 <- complete(test_imputed)
        
        
        
        
        pool(glm.mids (purge ~ ln_milex + ln_milper + ln_solpay + 
                           rgdpe + transleader + democracy + tenure1+lagged_realcoup_dummy, data = test_imputed,family = binomial))
        
        
        md.pattern(mis)
        #install.packages("VIM")
        library(VIM)
        aggr_plot <- aggr(mis, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
        
        densityplot(test_imputed)
        stripplot(test_imputed, pch = 20, cex = 1.2)
        scattmatrixMiss(mis)
        library(lattice)
        
        
        x <- as.data.frame(abs(is.na(mis)))
        ## Select columns with some (but not all) missing values
        y <- x[,sapply(x, sd) > 0]
        
        ## Create a correlation matrix: Variables missing together have high correlation
        cor(y)
        
        
        
        