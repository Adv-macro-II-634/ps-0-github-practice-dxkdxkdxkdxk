

##just reverted to here and oh it works.

########## ole ole ole ole

for (i in 1:20){
  year = data.frame((list[i]))
  names(year)[1:6] = c("County","year","State","resid","Rwin","Rmov")
  
  tb = data.frame(table(year$State))
  tb = tb[tb$Freq>2,]   
  year = year[year$State %in% tb$Var1,]
  
  y = lmList(resid ~ Rmov | State, data = year)
  
  coef = data.frame(summary(y)$coef[,1,2])
  coef$year = paste0(i+1990)
  coef$State = rownames(coef)
  coef$mov = longdata$mov[match(interaction(coef$State,coef$year),interaction(longdata$state,longdata$year))]
  assign(paste0("cb",i+1990),coef)
}