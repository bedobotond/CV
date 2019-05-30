#évenkénti felosztás
pie(table(guns$year/97218),radius = 1, labels= NA,col = c("#003300","#006600","#33CC33"),
    main= "Fegyveres halálesetek évenkénti bontása")
legend(x=1.3,y=1,legend = c(2012,2013,2014),fill =c("#003300","#006600","#33CC33"))
box()

#hónaponkénti felosztás
barplot(table(guns$year*guns$month/97218),ylim=c(0,3500),names.arg = NA,las=1,
        main="Fegyveres halálesetek száma havi bontásban", col = c("#003300","#33CC33"))
abline(h=mean(table(guns$year*guns$month/97218)), col=2, lwd=2)
mtext(text ="2012.01",side=1,cex = 0.6,adj = 0)
mtext(text ="2014.12",side=1,cex = 0.6,adj = 1)
text(x=0,y=2850,adj=0,label="Átlag",col=2,cex = 0.7)
box()

#nemenkénti felosztás
barplot(table(guns$sex)/97218,main = "Fegyveres halálesetek nemek közt felosztva",
        xlab = "Nem",ylab = "%",ylim= c(0,1),las=1,names.arg = c("Nõk","Férfiak"),col=c("red","blue"))
text(x=0.75,y=0.2,label = 0.1441297)
text(x=1.9,y=0.9,label = 0.8558703)
box()

#cél alapján felosztás
barplot(table(guns$intent)/97218,main = "Fegyverhasználat célja",xlab = "Az ok",ylab = "%",
        ylim = c(0,1), names.arg = c("Baleset","Gyilkosság","Öngyilkosság"),col = c("#003300","#006600","#33CC33"))
text(x=0.7,y=0.1,label = 0.01643729)
text(x=1.9,y=0.4,label = 0.34282746)
text(x=3.1,y=0.7,label = 0.64073525)
box()

#mûveltség
barplot(table(guns$education)/97218,las = 1,
        main = "Az áldozatok mûveltségi szintjeinek aránya",ylab = "%",ylim = c(0,1),col = c("#003300","#006600","#33CC33","#00FF00"))
legend(x=0.5,y=1,legend = c("Érettségi nélküli","Érettségivel rendelkezõ","Egyetemi képzésben részesült","Egyetemi végzettséggel rendelkezik"),fill =c("#003300","#006600","#33CC33","#00FF00"))
box()

#rassz
barplot((table(guns$race)/97218)[order((table(guns$race)/97218),decreasing = T)],las = 1, main = "Az adatok rassz alapján vizsgálva",ylab = "%",ylim = c(0,1),
        col = c("#003300","#006600","#33CC33"),xaxt = "n")
text(x=1.9,y=0.28,label = 0.231983789,cex = 0.7)
text(x=0.7,y=0.7,label = 0.231983789,cex = 0.7)
text(x=3.1,y=0.13,label = 0.231983789,cex = 0.7)
axis(1, at = c(0.75,2,3.1,4.25,5.5), labels = c("Fehér","Fekete", "Hispan","Ázsiai","Õslakos") )
mtext(text ="Fekete, fehér és hispan áldozatok összesen: 98%",side=3,cex = 0.75)
box()

#helyszín
barplot(table(guns$place)/97218,horiz = T,las=1,xlim = c(0,1),xlab = "%",cex.axis = 0.7,main = "Halálesetek helyszínének aránya",
        col = c("#003300","#33CC33"),cex.names = 0.3)
mtext(text ="Otthon történt halálesetek: 60%",side=3,cex = 0.75)
box()

#egy histogram arról, hogy a kooral hogyan változik a fegyveres gyilkosságok gyakorisága
hist(guns$age,freq = T,ylim = c(0,15000),breaks = 19,main = "Fegyveres halálesetek kor alapján",
     xlab = "Kor",ylab="Elõfordulás",las=1,cex.axis=0.8)
box()

#Most ugyanez, csak szándék szerint két részre bontva
#Öngyilkosság
hist(guns$age[guns$intent=="Suicide"],freq = T,ylim = c(0,15000),breaks = 19,main = "Öngyilkosság",xlab = "Kor",ylab="Elõfordulás",las=1,
     cex.axis = 0.8, cex.lab=0.8)
box()

#a másik ábra
hist(guns$age[guns$intent=="Homicide"],freq = T,ylim = c(0,15000),breaks = 19,main = "Gyilkosság",xlab = "Kor",ylab="Elõfordulás",las=1,
     cex.axis = 0.8, cex.lab=0.8)
box()

#boxploton ábrázolva
boxplot(guns$age,main="Halálesetek kor alapján",xlab="Kor",las=1,ylim=c(0,110),horizontal = T)
mtext(text ="Q1 = 27; Q2 = 42; Q3 = 58",side=3,cex = 0.75)
quantile(guns$age,probs = c(0, 0.25, 0.5, 0.75, 1))

#boxplot nemek közt törve, tanultság alapján plotolva, kor alapján számolva
boxplot(guns$age ~ guns$sex * guns$education,main="Kor vs Mûveltség nemek szerint bontva",col= c(2,4),las =1,
        axes = F,ylab="Kor",xlab="Mûveltség")
axis(2,at=seq(0,120,10),seq(0,120,10),las=1)
axis(1,at=c(1.5,3.5,5.5,7.5),labels=c(1,2,3,4),las = 1,cex.axis = 0.6)
legend(x=5.5,y=15.5,legend=c("Nõ","Férfi"),fill = c(2,4),cex=0.6,horiz=T)
box()

#Q-Q plot kor
hist(guns$age,freq = F,ylim = c(0,0.03),breaks = 19,main = "Kor sûrûség",
     xlab = "Kor",ylab="Sûrûség",las=1,cex.axis=0.8)
lines(density(guns$age), col = 2)
box()

qqnorm(guns$age,xlab="Elméleti kvantilisek",ylab = "Minta kvantilisek",main="Kor Q-Q Plot",las = 1)
qqline(guns$age,col=2)


#adatok felosztva
hist(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Homicide"],freq = F,main = "Fehér férfiak gyilkosságban haltak meg",xlab = "Kor",las=1,ylim = c(0,0.03),breaks = 19,cex.axis=0.8)
lines(density(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Homicide"]),col=2)
box()
qqnorm(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Homicide"],xlab="Elméleti kvantilisek",ylab = "Minta kvantilisek")
qqline(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Homicide"],col=2)

hist(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Homicide"],freq = F,main = "Fehér nõk gyilkosságban haltak meg",xlab = "Kor",las=1,ylim = c(0,0.03),breaks = 19,cex.axis=0.8)
lines(density(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Homicide"]),col=2)
box()
qqnorm(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Homicide"],xlab="Elméleti kvantilisek",ylab = "Minta kvantilisek")
qqline(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Homicide"],col=2)

hist(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Homicide"],freq = F,main = "Fekete férfiak gyilkosságban haltak meg",xlab = "Kor",las=1,ylim = c(0,0.06),breaks = 19,cex.axis=0.8)
lines(density(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Homicide"]),col=2)
box()
qqnorm(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Homicide"],xlab="Elméleti kvantilisek",ylab = "Minta kvantilisek")
qqline(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Homicide"],col=2)

hist(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Homicide"],freq = F,main = "Fekete nõk gyilkosságban haltak meg",xlab = "Kor",las=1,ylim = c(0,0.06),breaks = 19,cex.axis=0.8)
lines(density(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Homicide"]),col=2)
box()
qqnorm(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Homicide"],xlab="Elméleti kvantilisek",ylab = "Minta kvantilisek")
qqline(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Homicide"],col=2)


hist(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Suicide"],freq = F,main = "Fehér férfiak öngyilkosságban haltak meg",xlab = "Kor",las=1,ylim = c(0,0.03),breaks = 19,cex.axis=0.8)
lines(density(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Suicide"]),col=2)
box()
qqnorm(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Suicide"],xlab="Elméleti kvantilisek",ylab = "Minta kvantilisek")
qqline(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Suicide"],col=2)

hist(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Suicide"],freq = F,main = "Fehér nõk öngyilkosságban haltak meg",xlab = "Kor",las=1,ylim = c(0,0.03),breaks = 19,cex.axis=0.8)
lines(density(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Suicide"]),col=2)
box()
qqnorm(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Suicide"],xlab="Elméleti kvantilisek",ylab = "Minta kvantilisek")
qqline(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Suicide"],col=2)

hist(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Suicide"],freq = F,main = "Fekete férfiak öngyilkosságban haltak meg",xlab = "Kor",las=1,ylim = c(0,0.06),breaks = 19,cex.axis=0.8)
lines(density(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Suicide"]),col=2)
box()
qqnorm(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Suicide"],xlab="Elméleti kvantilisek",ylab = "Minta kvantilisek")
qqline(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Suicide"],col=2)

hist(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Suicide"],freq = F,main = "Fekete nõk öngyilkosságban haltak meg",xlab = "Kor",las=1,ylim = c(0,0.03),breaks = 19,cex.axis=0.8)
lines(density(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Suicide"]),col=2)
box()
qqnorm(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Suicide"],xlab="Elméleti kvantilisek",ylab = "Minta kvantilisek")
qqline(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Suicide"],col=2)


#Hipotézisvizsgálat
#egymintás t-próba
t.test(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Homicide"],mu=32.4,alternative = "greater")

t.test(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Homicide"],mu=39.3,alternative = "greater")

t.test(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Homicide"],mu=38.2,alternative = "greater")

t.test(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Homicide"],mu=31.9,alternative = "greater")

t.test(guns$age[guns$sex == "M" & guns$race== "White" & guns$intent == "Suicide"],mu=49,alternative = "greater")

t.test(guns$age[guns$sex == "F" & guns$race== "White" & guns$intent == "Suicide"],mu=46,alternative = "greater")

t.test(guns$age[guns$sex == "M" & guns$race== "Black" & guns$intent == "Suicide"],mu=36.9,alternative = "greater")

t.test(guns$age[guns$sex == "F" & guns$race== "Black" & guns$intent == "Suicide"],mu=38.4,alternative = "greater")

#kétmintás t-próba
t.test(guns$age[guns$sex== "F"],guns$age[guns$sex== "M"],alternative="greater")

#khí-négyzet próba
barplot(table(guns$intent,guns$sex),beside = T,ylim=c(0,60000),ylab="Gyilkosságok száma",las=2, main= "Nem és szándék", names.arg = c("Nõ","Férfi"),las=1,col = c("#003300","#006600","#33CC33"),cex.axis = 0.8) 
legend(x=1,y=50000,legend = c("Baleset","Gyilkosság","Öngyilkosság"),fill= c("#003300","#006600","#33CC33"))
box()
chisq.test(table(guns$intent,guns$sex), correct = T)

barplot(table(guns$year,guns$sex),beside = T,ylim=c(0,60000),ylab="Gyilkosságok száma",las=2, main= "Nem és év", names.arg = c("Nõ","Férfi"),las=1,col = c("#003300","#006600","#33CC33"),cex.axis = 0.8) 
legend(x=1,y=50000,legend = c(2012,2013,2014),fill= c("#003300","#006600","#33CC33"))
box()
chisq.test(table(guns$year,guns$sex), correct = T)
