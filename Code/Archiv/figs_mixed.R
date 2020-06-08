dblue <- rgb(5, 75, 195, maxColorValue = 255)
dred <- rgb(200, 5, 23, maxColorValue = 255)
dgreen <- rgb(15, 100, 5, maxColorValue = 255)
dyell <- rgb(250, 220, 15, maxColorValue = 255)
dpurp <- rgb(160, 0, 210, maxColorValue = 255)
dpink <- rgb(250, 30, 220, maxColorValue = 255)
dturq <- rgb(0, 240, 250, maxColorValue = 255)
dorange <- rgb(250, 160, 0, maxColorValue = 255)

dgreen <- rgb(15, 100, 5, maxColorValue = 255)
shgreen2 <- rgb(90, 150, 10, maxColorValue = 255)
shgreen3 <- rgb(180, 195, 20, maxColorValue = 255)
shgreen4 <- rgb(200, 220, 60, maxColorValue = 255)
shgreen5 <- rgb(70, 150, 90, maxColorValue = 255)
dblue <- rgb(5, 75, 195, maxColorValue = 255)

cols <- c(dgreen,shgreen2,shgreen3,shgreen4,dyell)

library("lme4")
library("plotrix")

# Random Intercept --------------------------------------------------------

set.seed(42)

n<- 160
nKlassen <- 6

Uebungen <- round(runif(n,1,29), digits=0)
Klasse <- sample(1:nKlassen, size=n, replace=TRUE)
effects <- round(rnorm(nKlassen,0,10), digits=1)
random_intercept <- numeric(n)
for(i in 1:n){
  random_intercept[i] <- effects[Klasse[i]]
}
error <- round(rnorm(n,0,5), digits=1)
Leistung <- round(12 + 2.2 * Uebungen + random_intercept + error, digits=0)

Klasse <- as.factor(Klasse)
levels(Klasse) <- c("7a","7b","7c","7d","7e","7f")

dat <- data.frame(Uebungen, Klasse, Leistung)
summary(dat)

mm_ri <- lmer(Leistung ~ 1 + Uebungen + (1|Klasse), data = dat)
summary(mm_ri)
 

# Plot Random Intercept erstellen ----------------------------------------------------------

Klassencols <- integer(n)
for(i in 1:n){
  Klassencols[i] <- cols[rank(-coef(mm_ri)$Klasse[,1])][which(levels(Klasse)==Klasse[i])]
}

# pdf("mm_ri.pdf")
par(mar=c(5,7,3,3))
plot(dat$Uebungen, dat$Leistung,  xlab="Anzahl Übungseinheiten", 
     ylab="Anzahl Punkte", 
     xlim=c(0,30), ylim=c(0,100), col = Klassencols, pch=20, 
     xaxs="i", yaxs="i", cex.lab=1.3, cex.axis=1.2)

# Linien pro Klasse
for(j in 1:nKlassen){
  abline(coef(mm_ri)$Klasse[j,1],coef(mm_ri)$Klasse[j,2],
         col = cols[rank(-coef(mm_ri)$Klasse[,1])][j])
}
# fixed Linie
abline(fixef(mm_ri), col = dred, lwd = 2)

# Intercept fixed 
arrows(x0=-2.8,x1=0,y0=fixef(mm_ri)[1],y1=fixef(mm_ri)[1], col=dred, lwd=2, 
       lend=2, xpd=T, length=0.1)
text(x=-4.2,y=fixef(mm_ri)[1]-1,expression(hat(gamma)['00']), cex=1.5, xpd=T, col=dred)

# intercept Klasse
arrows(x0=-2.8,x1=0,y0=sort(coef(mm_ri)$Klasse[,1])[6],
       y1=sort(coef(mm_ri)$Klasse[,1])[6], col=cols[1], lwd=2, lend=2, xpd=T, 
       length=0.1)
text(x=-6.9,y=sort(coef(mm_ri)$Klasse[,1])[6]-1,
     expression(hat(gamma)['00']), 
     cex=1.5, xpd=T, col=dred)
text(x=-5.5,y=sort(coef(mm_ri)$Klasse[,1])[6]-1,
     expression("+"), 
     cex=1.5, xpd=T)
text(x=-4.2,y=sort(coef(mm_ri)$Klasse[,1])[6]-1,
     expression(hat(delta)['0j']), 
     cex=1.5, xpd=T, col=dgreen)

# Steigung an einer Klasse
niedr_Kl<-levels(Klasse)[order(coef(mm_ri)$Klasse[,1])][1]
arrows(x0=14,x1=22,y0=predict(mm_ri,newdata=data.frame(Uebungen=14,Klasse=niedr_Kl)),
       y1=predict(mm_ri,newdata=data.frame(Uebungen=14,Klasse=niedr_Kl)), 
       col=1, lwd=2, lend=2, xpd=T, length=0.1)
arrows(x0=22,x1=22,y0=predict(mm_ri,newdata=data.frame(Uebungen=14,Klasse=niedr_Kl)),
       y1=predict(mm_ri,newdata=data.frame(Uebungen=22,Klasse=niedr_Kl)), 
       col=1, lwd=2, lend=2, xpd=T, length=0.1)
text(x=18,y=27,"1", cex=1.5)
text(x=23.7,y=38,expression(hat(gamma)['10']), cex=1.5, col=dred)

# Abweichung Klasse
hoech_Kl<-levels(Klasse)[order(-coef(mm_ri)$Klasse[,1])][1]
arrows(x0=4,x1=4,y0=predict(mm_ri,newdata=data.frame(Uebungen=4,Klasse=hoech_Kl),re.form=NA),
       y1=predict(mm_ri,newdata=data.frame(Uebungen=4,Klasse=hoech_Kl)), 
       col=cols[1], lwd=2, lend=2, xpd=T, length=0.1, code=3)
boxed.labels(x=5.4,y=32.5,expression(hat(delta)['0j']), cex=1.5, xpd=T, 
             bg="white", border=NA, col=dgreen)

# Abweichung Schueler
points(x=7.5,y=53,pch=20,col=dgreen)
arrows(x0=7.5,x1=7.5,y0=predict(mm_ri,newdata=data.frame(Uebungen=7.5,Klasse=hoech_Kl)),
       y1=53, 
       col=cols[1], lwd=2, lend=2, xpd=T, length=0.1, code=3)
boxed.labels(x=6.3,y=48,expression(hat(epsilon)['ij']), cex=1.5, xpd=T, 
             bg="white", border=NA, col=dgreen)
#dev.off()

summary(mm_ri)

# random intercept + slope, uncorrelated ----------------------------------

set.seed(77) #42 #55 sieht aus wie corr

n<- 160
nKlassen <- 6

Uebungen <- round(runif(n,1,29), digits=0)
Klasse <- sample(1:nKlassen, size=n, replace=TRUE)
effects_i <- round(rnorm(nKlassen,0,8), digits=1)
# uncorr
effects_s <- round(rnorm(nKlassen,0,1), digits=1)
random_intercept <- numeric(n)
for(i in 1:n){
  random_intercept[i] <- effects_i[Klasse[i]]
}
random_slope <- numeric(n)
for(i in 1:n){
  random_slope[i] <- effects_s[Klasse[i]]
}
error <- round(rnorm(n,0,5), digits=1)
Leistung <- round(17 + 1.7 * Uebungen + random_intercept + random_slope * Uebungen + error, digits=0)

Klasse <- as.factor(Klasse)
levels(Klasse) <- c("7a","7b","7c","7d","7e","7f")

dat <- data.frame(Uebungen,Klasse,Leistung)
summary(dat)

mm_ri <- lmer(Leistung ~ 1 + Uebungen + (1|Klasse), data = dat)
summary(mm_ri)

mm_ris <- lmer(Leistung ~ 1 + Uebungen + (1+Uebungen|Klasse), data = dat)
summary(mm_ris)

Klassencols <- integer(n)
for(i in 1:n){
  Klassencols[i] <- cols[rank(-coef(mm_ris)$Klasse[,2])][which(levels(Klasse)==Klasse[i])]
}

#pdf("mm_ris.pdf")
par(mar=c(5,7,3,3))
plot(dat$Uebungen, dat$Leistung,  xlab="Anzahl Übungseinheiten", ylab="Anzahl Punkte", 
     xlim=c(0,30), ylim=c(0,100), col = Klassencols, pch=20, xaxs="i", yaxs="i", 
     cex.lab=1.3, cex.axis=1.2)
lwd_Kl<-rep(1,6)
#lowest slope
lwd_Kl[which(levels(Klasse)==levels(Klasse)[order(coef(mm_ris)$Klasse[,2])][1])]<-1
#highest slope
lwd_Kl[which(levels(Klasse)==levels(Klasse)[order(-coef(mm_ris)$Klasse[,2])][1])]<-1
# Linien pro Klasse
for(j in 1:nKlassen){
  abline(coef(mm_ris)$Klasse[j,1],coef(mm_ris)$Klasse[j,2],
         col = cols[rank(-coef(mm_ris)$Klasse[,2])][j], lwd=lwd_Kl[j])
}
# Linie fixed
abline(fixef(mm_ris), col = dred, lwd = 2)
# Intercept fixed
arrows(x0=-2.8,x1=0,y0=fixef(mm_ris)[1],y1=fixef(mm_ris)[1], 
       col=dred, lwd=2, lend=2, xpd=T, length=0.1)
text(x=-4.2,y=fixef(mm_ris)[1]-2,expression(hat(gamma)['00']), cex=1.5, xpd=T, col=dred)
# Intercept eine Klasse
arrows(x0=-2.8,x1=0,
       y0=sort(coef(mm_ris)$Klasse[,1])[which(levels(Klasse)==levels(Klasse)[order(coef(mm_ris)$Klasse[,2])][1])],
       y1=sort(coef(mm_ris)$Klasse[,1])[which(levels(Klasse)==levels(Klasse)[order(coef(mm_ris)$Klasse[,2])][1])], 
       col=cols[1], lwd=2, lend=2, xpd=T, length=0.1)
text(x=-6.9,
     y=sort(coef(mm_ris)$Klasse[,1])[which(levels(Klasse)==levels(Klasse)
                                           [order(coef(mm_ris)$Klasse[,2])][1])]+4.3,
     expression(hat(gamma)['00']), cex=1.5, xpd=T,col=dred)
text(x=-5.5,
     y=sort(coef(mm_ris)$Klasse[,1])[which(levels(Klasse)==levels(Klasse)
                                           [order(coef(mm_ris)$Klasse[,2])][1])]+4.3,
     expression("+"), cex=1.5, xpd=T)
text(x=-4.2,
     y=sort(coef(mm_ris)$Klasse[,1])[which(levels(Klasse)==levels(Klasse)
                                           [order(coef(mm_ris)$Klasse[,2])][1])]+4.3,
     expression(hat(delta)['0j']), cex=1.5, xpd=T, col=dgreen)
#niedr_Kl<-levels(Klasse)[order(coef(mm_ris)$Klasse[,1])][1]
#hoechst_Kl<-levels(Klasse)[order(-coef(mm_ris)$Klasse[,1])][1]
# hoechste Steigung
Nr_Klasse<-which(levels(Klasse)==levels(Klasse)[order(-coef(mm_ris)$Klasse[,2])][1])
# random effects, bereits aufaddiert auf fixed effects
my_predict <- function(Nr_Uebungen,Nr_Klasse){
  coef(mm_ris)$Klasse[,1][Nr_Klasse] + coef(mm_ris)$Klasse[,2][Nr_Klasse] * Nr_Uebungen
}
my_predict(14,Nr_Klasse)
# Steigung eine Klasse
arrows(x0=14,x1=19,y0=my_predict(14,Nr_Klasse),y1=my_predict(14,Nr_Klasse), 
       col=cols[1], lwd=2, lend=2, xpd=T, length=0.1)
arrows(x0=19,x1=19,y0=my_predict(14,Nr_Klasse),y1=my_predict(19,Nr_Klasse), 
       col=cols[1], lwd=2, lend=2, xpd=T, length=0.1)
boxed.labels(x=16.5,y=47,"1", cex=1.5, bg="white", border=NA)
boxed.labels(x=20.8,y=58,expression(hat(gamma)['10']), cex=1.5, xpd=T, 
     bg="white", border=NA, col=dred)
boxed.labels(x=22.2,y=58,expression("+"), cex=1.5, xpd=T, 
             bg="white", border=NA)
boxed.labels(x=23.45,y=58,expression(hat(delta)['1j']), cex=1.5, xpd=T, 
             bg="white", border=NA,col=dgreen)
# re.form=NA ist nur fixed effects
# Steigung fixed
arrows(x0=12,x1=17,y0=predict(mm_ri,newdata=data.frame(Uebungen=12,Klasse=niedr_Kl),
                              re.form=NA),
       y1=predict(mm_ri,newdata=data.frame(Uebungen=12,Klasse=niedr_Kl),re.form=NA), 
       col=dred, lwd=2, lend=2, xpd=T, length=0.1)
arrows(x0=17,x1=17,y0=predict(mm_ri,newdata=data.frame(Uebungen=12,Klasse=niedr_Kl),
                              re.form=NA),
       y1=predict(mm_ri,newdata=data.frame(Uebungen=17,Klasse=niedr_Kl),re.form=NA), 
       col=dred, lwd=2, lend=2, xpd=T, length=0.1)
boxed.labels(x=14.8,y=24,"1", cex=1.5, bg="white", border=NA)
boxed.labels(x=19,y=31,expression(hat(gamma)['10']), cex=1.5, bg="white", border=NA, 
             col=dred)
#dev.off()

summary(mm_ris)


####################################################
# random intercept + slope, uncorrelated - EXTREMER
####################################################

set.seed(2) 

n<- 800
nKlassen <- 40

Uebungen <- round(runif(n,0,50), digits=0)
Klasse <- sample(1:nKlassen, size=n, replace=TRUE)
effects_i <- round(rnorm(nKlassen,0,80), digits=1)
# uncorr
effects_s <- round(rnorm(nKlassen,0,5), digits=1)
random_intercept <- numeric(n)
for(i in 1:n){
  random_intercept[i] <- effects_i[Klasse[i]]
}
random_slope <- numeric(n)
for(i in 1:n){
  random_slope[i] <- effects_s[Klasse[i]]
}
error <- round(rnorm(n,0,5), digits=1)
Leistung <- round(30 + 4 * Uebungen + random_intercept + random_slope * Uebungen + error, digits=0)

Klasse <- as.factor(Klasse)

dat <- data.frame(Uebungen,Klasse,Leistung)
summary(dat)

mm_ris <- lmer(Leistung ~ 1 + Uebungen + (1+Uebungen|Klasse), data = dat)
summary(mm_ris)

pdf("mm_ris_uncorr.pdf")
par(mar=c(5,7,3,3))
plot(dat$Uebungen, dat$Leistung,  xlab="x", ylab="y", 
     xlim=c(0,50), ylim=c(-300,700), col = "gray", pch=20, xaxs="i", yaxs="i", 
     cex.lab=1.3, cex.axis=1.2)
# Linien pro Klasse
for(j in 1:nKlassen){
  abline(coef(mm_ris)$Klasse[j,1],coef(mm_ris)$Klasse[j,2],col = "gray", lwd=1)
}
# Linie fixed
abline(fixef(mm_ris), col = 1, lwd = 2)
dev.off()

####################################################
# random intercept + slope, correlated - EXTREMER
####################################################

set.seed(2) 

n<- 800
nKlassen <- 40

Uebungen <- round(runif(n,0,50), digits=0)
Klasse <- sample(1:nKlassen, size=n, replace=TRUE)
effects_i <- round(rnorm(nKlassen,0,80), digits=1)
effects_i <- sort(effects_i)
effects_s <- round(rnorm(nKlassen,0,5), digits=1)
effects_s <- sort(effects_s)
cor(effects_i,effects_s)
random_intercept <- numeric(n)
for(i in 1:n){
  random_intercept[i] <- effects_i[Klasse[i]]
}
random_slope <- numeric(n)
for(i in 1:n){
  random_slope[i] <- effects_s[Klasse[i]]
}
error <- round(rnorm(n,0,5), digits=1)
Leistung <- round(30 + 4 * Uebungen + random_intercept + random_slope * Uebungen + error, digits=0)

Klasse <- as.factor(Klasse)

dat <- data.frame(Uebungen,Klasse,Leistung)
summary(dat)

mm_ris <- lmer(Leistung ~ 1 + Uebungen + (1+Uebungen|Klasse), data = dat)
summary(mm_ris)

pdf("mm_ris_corr.pdf")
par(mar=c(5,7,3,3))
plot(dat$Uebungen, dat$Leistung,  xlab="x", ylab="y", 
     xlim=c(0,50), ylim=c(-300,700), col = "gray", pch=20, xaxs="i", yaxs="i", 
     cex.lab=1.3, cex.axis=1.2)
# Linien pro Klasse
for(j in 1:nKlassen){
  abline(coef(mm_ris)$Klasse[j,1],coef(mm_ris)$Klasse[j,2],col = "gray", lwd=1)
}
# Linie fixed
abline(fixef(mm_ris), col = 1, lwd = 2)
dev.off()

####################################################
# random intercept + slope, neg correlated - EXTREMER
####################################################

set.seed(2) 

n<- 800
nKlassen <- 40

Uebungen <- round(runif(n,0,50), digits=0)
Klasse <- sample(1:nKlassen, size=n, replace=TRUE)
effects_i <- round(rnorm(nKlassen,0,80), digits=1)
effects_i <- sort(effects_i)
effects_s <- round(rnorm(nKlassen,0,5), digits=1)
effects_s <- sort(effects_s,decreasing=TRUE)
cor(effects_i,effects_s)
random_intercept <- numeric(n)
for(i in 1:n){
  random_intercept[i] <- effects_i[Klasse[i]]
}
random_slope <- numeric(n)
for(i in 1:n){
  random_slope[i] <- effects_s[Klasse[i]]
}
error <- round(rnorm(n,0,5), digits=1)
Leistung <- round(30 + 4 * Uebungen + random_intercept + random_slope * Uebungen + error, digits=0)

Klasse <- as.factor(Klasse)

dat <- data.frame(Uebungen,Klasse,Leistung)
summary(dat)

mm_ris <- lmer(Leistung ~ 1 + Uebungen + (1+Uebungen|Klasse), data = dat)
summary(mm_ris)

pdf("mm_ris_neg_corr.pdf")
par(mar=c(5,7,3,3))
plot(dat$Uebungen, dat$Leistung,  xlab="x", ylab="y", 
     xlim=c(0,50), ylim=c(-300,700), col = "gray", pch=20, xaxs="i", yaxs="i", 
     cex.lab=1.3, cex.axis=1.2)
# Linien pro Klasse
for(j in 1:nKlassen){
  abline(coef(mm_ris)$Klasse[j,1],coef(mm_ris)$Klasse[j,2],col = "gray", lwd=1)
}
# Linie fixed
abline(fixef(mm_ris), col = 1, lwd = 2)
dev.off()



####################################################
# Verbrechen Polizisten
####################################################

set.seed(2) 

n_citites<-5

n <- round(rnorm(n_citites,15,5), digits=0)
m_p <- seq(30,200,length.out = n_citites)
v_p <- seq(5,90,length.out = n_citites)
i_v <- seq(100,700,length.out = n_citites)

Typ <- numeric(0)
Polizisten <- numeric(0)
Verbrechen <- numeric(0)

for(i in 1:length(n)){
Typ_i <- rep(i,n[i])
Polizisten_i <- round(rnorm(n[i],m_p[i],v_p[i]), digits=0)
error <- round(rnorm(n[i],0,30), digits=0)
Verbrechen_i <- i_v[i] - 0.01 * Polizisten_i + error
Typ <- c(Typ,Typ_i)
Polizisten <- c(Polizisten,Polizisten_i)
Verbrechen <- c(Verbrechen,Verbrechen_i)
}

dat <- data.frame(Typ, Polizisten, Verbrechen)

#pdf("verbrechen.pdf")
par(mar=c(5,7,3,3))
plot(dat$Verbrechen, dat$Polizisten,  xlab="Polizisten", ylab="Verbrechen", 
     col = cols[dat$Typ], 
     xlim=c(0,800),ylim=c(0,300),
     pch=20, xaxs="i", yaxs="i", axes=FALSE,
     cex.lab=1.3, cex.axis=1.2)
#dev.off()

