library(lmtest)
library(sandwich)
library(car)
library(gplots)
library(calibrate)
library(msm)
library(effects)

op <- par(no.readonly=TRUE)

#set the los for various things
bp.test.significance<-0.05
CI.plot.significance<-0.05


#note for ramsey test: H0: model has no omitted variables
#note for the BPW test: H0: model is homoskedastic


#
#	Read in and edit the data
#

mydata <- read.csv("overdata.csv",header=TRUE)
mydata$innings2<-mydata$Innings-1
mydata$first15<-ifelse(mydata$Over<=15, 1, 0)
mydata$at.bat.lost.toss=abs(mydata$at.bat.won.toss-1)
names(mydata)

region.europe<-c("England","Ireland","Netherlands","Scotland","Wales")
region.subcontinent<-c("Bangladesh","India","Pakistan","Sri Lanka")
region.ssafrica<-c("Kenya","South Africa","Zimbabwe")
region.windies<-c("West Indies")
region.oceania<-c("Australia","New Zealand")
region.other<-c("Canada","Malaysia","Morocco","United Arab Emirates")


mydata$region[mydata$Country %in% region.europe]<-"Europe"
mydata$region[mydata$Country %in% region.subcontinent]<-"Subcontinent"
mydata$region[mydata$Country %in% region.ssafrica]<-"SS Africa"
mydata$region[mydata$Country %in% region.windies]<-"West Indies"
mydata$region[mydata$Country %in% region.oceania]<-"Oceania"
mydata$region[mydata$Country %in% region.other]<-"Other"



team.list<-unique(unlist(mydata$At.Bat))
team.list
temp.common.support<-data.frame(teams=team.list,include=0)

for(i in 1:nrow(temp.common.support)){
	temp.subset1<-subset(mydata,mydata$At.Bat==temp.common.support$teams[i] & mydata$Innings==1 & mydata$Day.night==0)
	temp.subset2<-subset(mydata,mydata$At.Bat==temp.common.support$teams[i] & mydata$Innings==1 & mydata$Day.night==1)
	temp.subset3<-subset(mydata,mydata$At.Bat==temp.common.support$teams[i] & mydata$Innings==2 & mydata$Day.night==0)
	temp.subset4<-subset(mydata,mydata$At.Bat==temp.common.support$teams[i] & mydata$Innings==2 & mydata$Day.night==1)
	if(nrow(temp.subset1)>=1 & nrow(temp.subset2)>=1 & nrow(temp.subset3)>=1 & nrow(temp.subset4)>=1){
		temp.common.support[i,2]<-1
	}
}
cs<-subset(temp.common.support,temp.common.support$include==1)
mydata$cs<-ifelse(mydata$At.Bat %in% cs$teams & mydata$Fielding %in% cs$teams,1,0)


fulldata<-subset(mydata,mydata$Error.In.Data==0)
mydata<-subset(mydata,mydata$Error.In.Data==0 & mydata$cs==1)
#write.table(mydata,file="C:\\test.csv",sep=",")
cs.team.list<-unique(unlist(mydata$At.Bat))
cs.team.list


#
# Add in the average ranking data and generate chart
#
quality <- read.csv("rankingdata.csv",header=TRUE)
names(quality)
mydata$ranking<-quality$Rank[match(mydata$At.Bat,quality$Country)]

ranking<-quality$Rank
dn.perc<-quality$Ppn.Day.Night*100
model<-lm(dn.perc~ranking)
summary(model)
se<-sqrt(diag(vcov(model)))
se

#Bubble Plot
radius<-sqrt(quality$Total.Matches/pi)
symbols(ranking,dn.perc,circles=radius,inches=0.25,fg="black",bg="#BDBDBD",xlab="Average annual ICC Points (May 1999 – December 2011)",ylab="Day Night Matches (%)",ylim=c(0,60),xlim=c(0,150))
grid()
abline(model$coefficients[1],model$coefficients[2],col="red")
textxy(ranking,dn.perc,quality$Country,cx=0.7)
text(1.1,28,paste("Day Night (%) =",round(model$coefficients[1],1),"+",round(model$coefficients[2],1),"* Points"),col="Red",pos=4,cex=0.8)
text(3.7,23,paste("(",round(se[1],1),") (",round(se[2],1),")",sep=""),col="Red",pos=4, cex=0.7)

#dot plot
plot(ranking,dn.perc,pch=16,xlab="Average annual ICC Points (May 1999 – December 2011)",ylab="Day Night Matches (%)",ylim=c(0,60),xlim=c(0,150))
grid()
abline(model$coefficients[1],model$coefficients[2],col="red",lty=2)
textxy(ranking,dn.perc,quality$Country,cx=0.7,)
text(55,27,paste("Day Night (%) =",round(model$coefficients[1],1),"+",round(model$coefficients[2],1),"* Points"),col="Red",pos=4,cex=0.8)
text(85,25,paste("(",round(se[1],1),")   (",round(se[2],1),")",sep=""),col="Red",pos=4, cex=0.7)




#
#
# Compute the table in the paper that shows teams preferences for batting second after winning the toss
#
#
temp.mydata<-subset(mydata,mydata$new.game==1)
temp.mydata$win.toss.field<-abs(temp.mydata$at.bat.won.toss-1)
simplified.data<-data.frame(win.toss.field=temp.mydata$win.toss.field,Toss.Winner=temp.mydata$Toss.Winner,Day.night=temp.mydata$Day.night,Home.Team=temp.mydata$Home.Team,Away.Team=temp.mydata$Away.Team)

coin.toss.decision<-data.frame(countries=cs.team.list,day.number=0,dn.number=0,day.win.toss=0,dn.win.toss=0,day.field=0,dn.field=0,day.ppn=0,dn.ppn=0,diff=0,pvalue=0,lower=0,upper=0,mid=0)
for(i in 1:length(cs.team.list))
{
	#
	#	Here you first take matches where the team won the toss, then split into day and dn
	#	Then calculate ppn of time they chose to bat second
	#	Then test for a statistical significant difference in the proportions
	#

	temp.team<-toString(cs.team.list[i])

	temp.table.data.day<-subset(simplified.data,simplified.data$Toss.Winner==temp.team & simplified.data$Day.night==0)
	temp.table.data.dn<-subset(simplified.data,simplified.data$Toss.Winner==temp.team & simplified.data$Day.night==1)
	
	temp.day.average<-mean(temp.table.data.day$win.toss.field)
	temp.dn.average<-mean(temp.table.data.dn$win.toss.field)
	
	temp.diff<-temp.dn.average-temp.day.average

	test.result<-prop.test(x=c(sum(temp.table.data.dn$win.toss.field),sum(temp.table.data.day$win.toss.field)),n=c(length(temp.table.data.dn$win.toss.field),length(temp.table.data.day$win.toss.field)))


	coin.toss.decision[i,2]<-nrow(subset(simplified.data,(simplified.data$Home.Team==temp.team | simplified.data$Away.Team==temp.team) &  simplified.data$Day.night==0))
	coin.toss.decision[i,3]<-nrow(subset(simplified.data,(simplified.data$Home.Team==temp.team | simplified.data$Away.Team==temp.team) &  simplified.data$Day.night==1))


	coin.toss.decision[i,4]<-nrow(temp.table.data.day)
	coin.toss.decision[i,5]<-nrow(temp.table.data.dn)

	coin.toss.decision[i,6]<-sum(temp.table.data.day$win.toss.field)
	coin.toss.decision[i,7]<-sum(temp.table.data.dn$win.toss.field)
	

	coin.toss.decision[i,8]<-temp.day.average
	coin.toss.decision[i,9]<-temp.dn.average
	coin.toss.decision[i,10]<-temp.diff
	coin.toss.decision[i,11]<-test.result$p.value	
	coin.toss.decision[i,12]<-test.result$conf.int[1]
	coin.toss.decision[i,13]<-test.result$conf.int[2]
	coin.toss.decision[i,14]<-mean(c(test.result$conf.int[1],test.result$conf.int[2]))

}
coin.toss.decision

temp.uiw=coin.toss.decision$upper-coin.toss.decision$mid
temp.liw=coin.toss.decision$mid-coin.toss.decision$lower
plotCI(x=1:length(cs.team.list),y=coin.toss.decision$mid,uiw=temp.uiw,liw=temp.liw, lty = 2, ylim=c(min(coin.toss.decision$lower),max(coin.toss.decision$upper)))



#
#Subset to teams that lost the toss to eliminate preferences
#
backupdata<-mydata
mydata<-subset(mydata,mydata$at.bat.won.toss==0)

#
#Graph the DID to motivate it
#
data.day.first<-subset(mydata$Runs,mydata$Innings==1 & mydata$Day.night==0)
data.day.second<-subset(mydata$Runs,mydata$Innings==2 & mydata$Day.night==0)
data.dn.first<-subset(mydata$Runs,mydata$Innings==1 & mydata$Day.night==1)
data.dn.second<-subset(mydata$Runs,mydata$Innings==2 & mydata$Day.night==1)

print("DID treatment effect")
(mean(data.day.first)-mean(data.day.second))-(mean(data.dn.first)-mean(data.dn.second))

density.day.first<-density(data.day.first, bw=1)
density.day.second<-density(data.day.second, bw=1)
density.dn.first<-density(data.dn.first, bw=1)
density.dn.second<-density(data.dn.second, bw=1)

op <- par(no.readonly=TRUE)
par(mfrow=c(3,2),mar=c(3.9,4.5,2,3)+.1)
plot(density.day.first,xlim=c(0,36), main="", xlab="", ylab="density",cex.axis=1.25,cex.lab=1.25, bty="n")
lines(density.day.second, lty=2, col=2)
legend("topright", inset=0.02, legend=c("Innings 1","Innings 2"),lty=c(1,2),col=c("black","red"), bty="n")
title(main="a. Distribution of runs, Day", font.main=4, family="A")

plot(density.dn.first,xlim=c(0,36), main="", xlab="", ylab="density",cex.axis=1.25,cex.lab=1.25, bty="n")
lines(density.dn.second, lty=2, col=2)
legend("topright", inset=0.02, legend=c("Innings 1","Innings 2"),lty=c(1,2),col=c("black","red"), bty="n")
title(main="b. Distribution of runs, Day-night", font.main=4, family="A")

#estimate differences in density functions

f.density.day.first<-approxfun(density.day.first$x,density.day.first$y,yleft=0,yright=0)
f.density.day.second<-approxfun(density.day.second$x,density.day.second$y,yleft=0,yright=0)
f.density.dn.first<-approxfun(density.dn.first$x,density.dn.first$y,yleft=0,yright=0)
f.density.dn.second<-approxfun(density.dn.second$x,density.dn.second$y,yleft=0,yright=0)


panel.a<-array(0,dim=c(3,37))
panel.b<-array(0,dim=c(3,37))

for(i in 1:37)
{
	panel.a[1,i]<-integrate(f.density.day.first,i-1,i)$value
	panel.a[2,i]<-integrate(f.density.day.second,i-1,i)$value
	panel.a[3,i]<-panel.a[2,i]-panel.a[1,i]

	panel.b[1,i]<-integrate(f.density.dn.first,i-1,i)$value
	panel.b[2,i]<-integrate(f.density.dn.second,i-1,i)$value
	panel.b[3,i]<-panel.b[2,i]-panel.b[1,i]
}

DID.graph<-panel.b[3,]-panel.a[3,]

ymax<-max(panel.a[3,],panel.b[3,],DID.graph)
ymin<-min(panel.a[3,],panel.b[3,],DID.graph)

windowsFonts(
  A=windowsFont("Georgia"),
)
barplot(panel.a[3,],space=0.25, col="grey",ylim=c(ymin,ymax),border=NA, xlim=c(0,36), xlab="", ylab="Difference in probability",cex.axis=1.25,cex.lab=1.25)
axis(1,lty=1,at=6*(0:6)+1,labels=c(0,6,12,18,24,30,36),cex.axis=1.25,cex.lab=1.25)
title(main="c. 2nd innings - 1st innings, Day", font.main=4, family="A")

barplot(panel.b[3,],space=0.25, col="grey",ylim=c(ymin,ymax),border=NA, xlim=c(0,36), xlab="", ylab="Difference in probability",cex.axis=1.25,cex.lab=1.25)
axis(1,lty=1,at=6*(0:6)+1,labels=c(0,6,12,18,24,30,36),cex.axis=1.25,cex.lab=1.25)
title(main="d. 2nd innings - 1st innings, Day-night", font.main=4, family="A")

barplot(DID.graph,space=0.25, col="grey",ylim=c(ymin,ymax),border=NA, xlim=c(0,36), xlab="runs scored per over", ylab="Difference in probability",cex.axis=1.25,cex.lab=1.25)
axis(1,lty=1,at=6*(0:6)+1,labels=c(0,6,12,18,24,30,36),cex.axis=1.25,cex.lab=1.25)
title(main="e. Difference in differences", font.main=4, family="A")

plot(sum(DID.graph*(0:36)),0, xlim=c(-1,1), ylim=c(0,0), xlab="runs scored per over",cex.axis=1.25,cex.lab=1.25,col="grey",yaxt="n",ylab="",bty="n",cex=3,pch=16)
#axis(1,lty=1,at=6*(0:6)+1,labels=c(0,6,12,18,24,30,36),cex.axis=1.25,cex.lab=1.25)
title(main="f. Weighted average", font.main=4, family="A")

par(op)



#
# Basic DID estimates
#

print("Non-parametric treatment effect")
sum(DID.graph*(0:36))

print("DID treatment effect")
(mean(data.day.first)-mean(data.day.second))-(mean(data.dn.first)-mean(data.dn.second))



#Run DID models

model1<-lm(Runs~Day.night+innings2+I(Day.night*innings2),data=mydata)
summary(model1)
reset(model1,power=3)
bp.result1<-bptest(model1)
bp.result1
if(bp.result1$p.value<bp.test.significance){
	model1.robustSE<-sqrt(diag(hccm(model1)))
	model1.robustT<-coef(model1)/model1.robustSE
	model1.robustp<-2*(1-pnorm(abs(model1.robustT)))
	as.matrix(model1.robustp)
}


model2<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+Over+Total.Out,data=mydata)
summary(model2)
reset(model2,power=3)
bp.result2<-bptest(model2)
bp.result2
if(bp.result2$p.value<bp.test.significance){
	model2.robustSE<-sqrt(diag(hccm(model2)))
	model2.robustT<-coef(model2)/model2.robustSE
	model2.robustp<-2*(1-pnorm(abs(model2.robustT)))
	as.matrix(model2.robustp)
}


model3<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+Over+at.bat.won.toss+region+at.bat.at.home+Total.Out,data=mydata)
summary(model3)
reset(model3,power=3)
bp.result3<-bptest(model3)
bp.result3
if(bp.result3$p.value<bp.test.significance){
	model3.robustSE<-sqrt(diag(hccm(model3)))
	model3.robustT<-coef(model3)/model3.robustSE
	model3.robustp<-2*(1-pnorm(abs(model3.robustT)))
	as.matrix(model3.robustp)
}


model4<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+Over+at.bat.won.toss+region+at.bat.at.home+factor(Total.Out),data=mydata)
summary(model4)
reset(model4,power=3)
bp.result4<-bptest(model4)
bp.result4
if(bp.result4$p.value<bp.test.significance){
	model4.robustSE<-sqrt(diag(hccm(model4)))
	model4.robustT<-coef(model4)/model4.robustSE
	model4.robustp<-2*(1-pnorm(abs(model4.robustT)))
	as.matrix(model4.robustp)
}

model5<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+Over+at.bat.won.toss+region+at.bat.at.home+factor(Total.Out)+I(Over^2)+I(Over^3)+I(Over*Total.Out)+I((Over*Total.Out)^2)+I((Over*Total.Out)^3),data=mydata)
summary(model5)
reset(model5,power=3)
bp.result5<-bptest(model5)
bp.result5
if(bp.result5$p.value<bp.test.significance){
	model5.robustSE<-sqrt(diag(hccm(model5)))
	model5.robustT<-coef(model5)/model5.robustSE
	model5.robustp<-2*(1-pnorm(abs(model5.robustT)))
	as.matrix(model5.robustp)
}

model6<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+Over+at.bat.won.toss+region+at.bat.at.home+factor(Total.Out)+I(Over^2)+I(Over^3)+I(Over*Total.Out)+I((Over*Total.Out)^2)+I((Over*Total.Out)^3)+factor(At.Bat)+factor(Fielding),data=mydata)
summary(model6)
reset(model6,power=3)
bptest(model6)
bp.result6<-bptest(model6)
bp.result6
if(bp.result6$p.value<bp.test.significance){
	model6.robustSE<-sqrt(diag(hccm(model6)))
	model6.robustT<-coef(model6)/model6.robustSE
	model6.robustp<-2*(1-pnorm(abs(model6.robustT)))
	as.matrix(model6.robustp)
}


#
#	Test for multicollinearity
#
model4a<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+Over+region+at.bat.at.home+factor(Total.Out),data=mydata)
vif(model4a)

model6a<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+Over+region+at.bat.at.home+factor(Total.Out)+I(Over^2)+I(Over^3)+I(Over*Total.Out)+I((Over*Total.Out)^2)+I((Over*Total.Out)^3)+factor(At.Bat)+factor(Fielding),data=mydata)
vif(model6a)

#
#
#	Create the error bar plots
#
#

estimated.treatment.effects<-c(coef(model1)[4],coef(model2)[4],coef(model3)[4],coef(model4)[4],coef(model5)[4],coef(model6)[4])
stderr.treatment.effects<-qnorm(1-0.5*CI.plot.significance)*c(model1.robustSE[4],model2.robustSE[4],model3.robustSE[4],model4.robustSE[4],model5.robustSE[4],model6.robustSE[4])
plotCI(estimated.treatment.effects,uiw=stderr.treatment.effects, lty = 2, ylim = c(-0.6,0),xlab="Model number", ylab="Treatment effect (runs per over)",main="95% Confidence interval of estimated treatment effects")


#
#
#	Find matches affected by the analysis and measure the extent of effect
#
#
preferred.result<-summary(model6)
estimated.effect<-preferred.result$coefficients[4]

lower<-estimated.effect+qnorm(1-0.5*CI.plot.significance)*model6.robustSE[4]
upper<-estimated.effect-qnorm(1-0.5*CI.plot.significance)*model6.robustSE[4]

#find affected matches
temp.mydata<-backupdata
temp.mydata$last.over<-c(temp.mydata$new.game[2:length(temp.mydata$new.game)],1)
temp.mydata<-subset(temp.mydata,temp.mydata$last.over==1 & temp.mydata$Day.night==1 & temp.mydata$at.bat.wins==0 & temp.mydata$at.bat.won.toss==0)

#Number of matches where team batting second in a dn match lost hte toss and hte match
total.matches<-nrow(temp.mydata)
total.matches

#Number of matches where estiamted effect is greater than margin
affected.matches<-sum(abs(temp.mydata$Over*estimated.effect)>=temp.mydata$Target.Score-temp.mydata$Total.Runs)
affected.matches

affected.matches/total.matches*100

#total margin explained
total.explaned<-sum(abs(temp.mydata$Over*estimated.effect))/sum(temp.mydata$Target.Score-temp.mydata$Total.Runs)
total.explaned

lower.total.explaned<-sum(abs(temp.mydata$Over*lower))/sum(temp.mydata$Target.Score-temp.mydata$Total.Runs)
lower.total.explaned

upper.total.explaned<-sum(abs(temp.mydata$Over*upper))/sum(temp.mydata$Target.Score-temp.mydata$Total.Runs)
upper.total.explaned



#
#	Sensitivity Tests
#

#model 7 includes ranking as a variable
model7<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+ranking+Over+at.bat.won.toss+region+at.bat.at.home+factor(Total.Out)+I(Over^2)+I(Over^3)+I(Over*Total.Out)+I((Over*Total.Out)^2)+I((Over*Total.Out)^3)+factor(At.Bat)+factor(Fielding),data=mydata)
summary(model7)
reset(model7,power=3)
bptest(model7)
bp.result7<-bptest(model7)
bp.result7
if(bp.result7$p.value<bp.test.significance){
	model7.robustSE<-sqrt(diag(hccm(model7)))
	model7.robustT<-coef(model7)/model7.robustSE
	model7.robustp<-2*(1-pnorm(abs(model7.robustT)))
	as.matrix(model7.robustp)
}


#model 8 is based on the full data set (not just the common support)
nrow(mydata)
sens.data<-subset(fulldata,fulldata$at.bat.won.toss==0)
nrow(sens.data)

model8<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+Over+at.bat.won.toss+region+at.bat.at.home+factor(Total.Out)+I(Over^2)+I(Over^3)+I(Over*Total.Out)+I((Over*Total.Out)^2)+I((Over*Total.Out)^3)+factor(At.Bat)+factor(Fielding),data=sens.data)
summary(model8)
reset(model8,power=3)
bptest(model8)
bp.result8<-bptest(model8)
bp.result8
if(bp.result8$p.value<bp.test.significance){
	model8.robustSE<-sqrt(diag(hccm(model8)))
	model8.robustT<-coef(model8)/model8.robustSE
	model8.robustp<-2*(1-pnorm(abs(model8.robustT)))
	as.matrix(model8.robustp)
}


#model 9 is based on the test teams only
nrow(mydata)
test.teams<-c("Australia","Bangladesh","England","India","New Zealand","Pakistan","South Africa","Sri Lanka","West Indies","Zimbabwe")
sens.data$test<-ifelse(sens.data$At.Bat %in% test.teams & sens.data$Fielding %in% test.teams,1,0)
testdata<-subset(sens.data,sens.data$test==1)
nrow(testdata)

model9<-lm(Runs~Day.night+innings2+I(Day.night*innings2)+Over+at.bat.won.toss+region+at.bat.at.home+factor(Total.Out)+I(Over^2)+I(Over^3)+I(Over*Total.Out)+I((Over*Total.Out)^2)+I((Over*Total.Out)^3)+factor(At.Bat)+factor(Fielding),data=fulldata)
summary(model9)
reset(model9,power=3)
bptest(model9)
bp.result9<-bptest(model9)
bp.result9
if(bp.result9$p.value<bp.test.significance){
	model9.robustSE<-sqrt(diag(hccm(model9)))
	model9.robustT<-coef(model9)/model9.robustSE
	model9.robustp<-2*(1-pnorm(abs(model9.robustT)))
	as.matrix(model9.robustp)
}







#
#	Wicket Analysis
#

#	Plot number of out in each over for day and dn

temp.result<-array(0,dim=c(50,2))
for(i in 1:50)
{
	temp.result[i,1]<-mean(mydata$Total.Out[mydata$Over==i & mydata$Day.night==0])
	temp.result[i,2]<-mean(mydata$Total.Out[mydata$Over==i & mydata$Day.night==1])
}
temp.result
plot(1:50,temp.result[,1], type="l", col="red")
lines(temp.result[,2], col="black")
legend("topleft", inset=0.01, legend=c("Day only","Day Night"),lty=1,col=c("red","black"))




#
#	Wickets as dependent variable
#
plot(0:10,as.matrix(table(mydata$Total.Out))/nrow(mydata))

#poisson regression
modelW1 <- glm(Total.Out ~ Over+Day.night+innings2+I(Day.night*innings2)+at.bat.won.toss+region+at.bat.at.home+I(Over^2)+I(Over^3)+factor(At.Bat)+factor(Fielding),data=mydata, family = "poisson")
summary(modelW1)
cov.m1 <- vcovHC(modelW1, type = "HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate = coef(modelW1), `Robust SE` = std.err, `Pr(>|z|)` = 2 * 
    pnorm(abs(coef(modelW1)/std.err), lower.tail = FALSE), LL = coef(modelW1) - 1.96 * 
    std.err, UL = coef(modelW1) + 1.96 * std.err)

r.est


# quasipoisson to control for some overdispersion
modelW2 <- glm(Total.Out ~ Over+Day.night+innings2+I(Day.night*innings2)+region+at.bat.at.home+I(Over^2)+I(Over^3)+factor(At.Bat)+factor(Fielding),data=mydata, family="quasipoisson")
summary(modelW2)
cov.m1 <- vcovHC(modelW2, type = "HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate = coef(modelW2), `Robust SE` = std.err, `Pr(>|z|)` = 2 * 
    pnorm(abs(coef(modelW2)/std.err), lower.tail = FALSE), LL = coef(modelW2) - 1.96 * 
    std.err, UL = coef(modelW2) + 1.96 * std.err)

r.est


day.data<-mydata
dn.data<-mydata
day.data$Day.night<-0
day.data$innings2<-1
dn.data$Day.night<-1
dn.data$innings2<-1
day.fit<-exp(predict(modelW2,newdata=day.data))
dn.fit<-exp(predict(modelW2,newdata=dn.data))

input<-cbind(mydata$Over,day.fit,dn.fit)
holder<-array(0,dim=c(50,4))
for(i in 1:50)
{
	temp.input<-subset(input,input[,1]==i)
	holder[i,1]<-mean(temp.input[,1])
	holder[i,2]<-mean(temp.input[,2])
	holder[i,3]<-mean(temp.input[,3])
	holder[i,4]<-holder[i,3]-holder[i,2]
}
holder
plot(holder[,1],holder[,2],type="l",xlab="Over Number",ylab="Expected number of wickets lost")
lines(holder[,1],holder[,3],col="red",lty=2)
legend("topleft", inset=0.01, legend=c("Day only","Day-night"),lty=c(1,2),col=c("black","red"),bty="n")

rm(list=ls(all=TRUE))




