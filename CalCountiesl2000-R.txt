cty <- scan("http://www.medepi.net/data/calpop/calcounty.txt", what="")
calpop <- read.csv("http://www.medepi.net/data/calpop/CalCounties2000.txt",header=T)
for(i in 1:length(cty)){
  calpop$County[calpop$County==i] <- cty[i]
}

calpop$Agecat <- cut(calpop$Age, c(0,20,45,65,100),
                     include.lowest = TRUE, right = FALSE) 
calpop$AsianPI <- calpop$Asian + calpop$Pacific.Islander
calpop$AmerInd <- calpop$American.Indian
calpop$Latino <- calpop$Hispanic
calpop$AfrAmer <- calpop$Black

baindex <- calpop$County=="Alameda" |
           calpop$County=="San Francisco" 

bapop <- calpop[baindex,]

agelabs <- names(table(bapop$Agecat))
sexlabs <- c("Female", "Male")
racen <- c("White", "AfrAmer", "AsianPI", "Latino", "Multirace", "AmerInd")
ctylabs <- names(table(bapop$County))
bapop2 <- aggregate(bapop[,racen],
                    list(Agecat = bapop$Agecat,
                         Sex = bapop$Sex,
                         County = bapop$County), sum)
tmp <- as.matrix(cbind(bapop2[1:4,racen], bapop2[5:8,racen],
             bapop2[9:12,racen], bapop2[13:16,racen]))
bapop3 <- array(tmp, c(4, 6, 2, 2))
dimnames(bapop3) <- list(agelabs, racen, sexlabs, ctylabs)
