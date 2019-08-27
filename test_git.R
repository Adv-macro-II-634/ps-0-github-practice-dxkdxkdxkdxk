<<<<<<< HEAD
for (i in 1:27){
  ################## outcome data
  ### onwership: total
  q = readxl::read_xlsx(paste("an",i+1989,".xlsx",sep = ''))
  q = q[q$Own==0 & q[1] !="US000" & q$`Area Type` !="State",]  # own = 0,total  own = 2, state gov 
  names(q)[9]="State"
  names(q)[10]="County"
  names(q)[6] = "year"
  q$`Annual Average Employment` = log(q$`Annual Average Employment`)
  q$`Annual Average Pay` = log(q$`Annual Average Pay`)
  q = q[!(is.infinite(q$`Annual Average Employment`)) &
          !(is.infinite(q$`Annual Average Pay`)),]
  q$t = i
  q = as.matrix(q)
  assign(paste0("q",i+1989),q)
}
qbind = rbind(q1990,q1991,q1992,q1993,q1994,q1995,q1996,q1997,q1998,
              q1999,q2000,q2001,q2002,q2003,q2004,q2005,q2006,q2007,
              q2008,q2009,q2010,q2011,q2012,q2013,q2014,q2015,q2016)
qbind = data.frame(qbind)

####### regress outcome of t+k on t-1 
oc = qbind[,c(10,6,9,18,15,21)]
names(oc)[4:5] = c("avginc", "avgemp")
#oc = oc[!(is.na(oc$State)),]

for (i in 4:6) {
  oc[i] = as.numeric(as.character(unlist(oc[i])))
}
oc[1] = as.character(unlist(oc[1]))
oc[3] = as.character(unlist(oc[3]))
oc[2] = as.numeric(as.character(unlist(oc[2])))

aux = oc
aux$year = aux$year - 6
aux1 = oc
aux1$year = aux1$year + 1

##just reverted to here and oh it works.
=======


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
>>>>>>> XD-test

#### submission
