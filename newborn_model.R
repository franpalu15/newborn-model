#QUESITO 1
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(gghalves)
library(readr)
dati <- read.csv("neonati.csv", header = T, sep = ",")
#quesito 2

str(dati)
summary(dati)


#quesito 3
par(mfrow=c(1,2))
###effettuiamo un po' di analisi descrittiva
boxplot(dati$Gestazione, col="lightblue")
hist(dati$Gestazione) 
classi_gestazione<-cut(dati$Gestazione, breaks=c(25, 30, 35,40))
N <- dim(dati)[1]
ni<-table(classi_gestazione)
fi<-ni/N
Ni<-cumsum(ni)
Fi<-Ni/N
tabella_gestazione <- as.data.frame(cbind(ni,fi,Ni,Fi))
tabella_gestazione$fi 
tabella_gestazione$perc <- round(tabella_gestazione$ni/max(tabella_gestazione$Ni)*100,1)
median(dati$Gestazione)
mean(dati$Gestazione)
sd(dati$Gestazione)
moments::kurtosis(dati$Gestazione)-3
moments::skewness(dati$Gestazione)
###la maggior parte delle mamme (95%) hanno 
###completato una gestazione compresa tra 35 e 40 settimane
##media e mediana sono vicine, ma gli indici di forma ci suggeriscono che siamo di fronte
###ad una variabile asimmetrica spostata a destra, come si vede dall'istogramma
###molto appuntita come si può vedere anche dalla curtosi


###gravidanze
par(mfrow=c(1,2))
boxplot(dati$N.gravidanze)
hist(dati$N.gravidanze)
classi_gravidanze<-cut(dati$N.gravidanze, breaks=c(0, 1, 2, 3, 4))
N <- dim(dati)[1]
ni<-table(classi_gravidanze)
fi<-ni/N
Ni<-cumsum(ni)
Fi<-Ni/N

tabella_gravidanze <- as.data.frame(cbind(ni,fi,Ni,Fi))
tabella_gravidanze$perc <- round(tabella_gravidanze$ni/max(tabella_gravidanze$Ni)*100,1)
###come si vede dalla tabella, circa il 60% delle neo-mamme era al primo figlio
###circa il 25% era alla seconda gravidanza
###dal boxplot si possono vedere molti outliers, con alcune mamme che hanno addirittura
###raggiunto 12 figli (solo il 3.5% delle mamme hanno più di 4 figli)
##media e mediana sono molto vicine e la standard deviation non è troppo elevata
median(dati$N.gravidanze)
mean(dati$N.gravidanze)
sd(dati$N.gravidanze)
moments::kurtosis(dati$N.gravidanze)-3 ###come si notava già dall'istogramma (ma questo valore ce lo conferma alla grande), 
###il numero delle gravidanze è una variabile leptocurtica (infatti l'istogramma è molto appuntito)
moments::skewness(dati$N.gravidanze) ###per niente simmetrica perché lontana dallo zero
###il numero delle gravidanze e le settimane di gestazione è quasi come se fossero capovolte
##una è asimmetrica a sx e l'altra a destra, una ha la mediana vicina al minimo, l'altra vicina al massimo, come ci si aspettava
###a priori, senza sapere troppo sui dati. Infatti è poco probabile che una mamma abbia 10 figli, mentre è assai probabile che
##si partorisca attorno alle 35-40 settimane.

###fumatrici
attach(dati)
table(dati$Fumatrici)
(104/(104+2396))*100
##il 4.2 % circa delle mamme fuma



###peso
par(mfrow=c(1,2))
boxplot(dati$Peso)
hist(dati$Peso)
##esistono diversi outliers specie sotto il limite inferiore del box (probabilmente bimbi nati prematuri?)
classi_Peso<-cut(dati$Peso, breaks=c(830, 1930, 2930, 3930, 4930))
N <- dim(dati)[1]
ni<-table(classi_Peso)
fi<-ni/N
Ni<-cumsum(ni)
Fi<-Ni/N

tabella_Peso <- as.data.frame(cbind(ni,fi,Ni,Fi))
tabella_Peso$perc <- round(tabella_Peso$ni/max(tabella_Peso$Ni)*100,1)

median(dati$Peso)
mean(dati$Peso)
sd(dati$Peso)
moments::kurtosis(dati$Peso)-3 
moments::skewness(dati$Peso) 
###gli indici di forma ci suggeriscono che il peso come ci si aspetta è una variabile più simmetrica
###e "meno appuntita" rispetto alle altre viste finora
##media e mediana sono vicine e in questo caso invece la sd è enorme, data dal fatto che
###ovviamente, i bambini nascono con peso diverso, probabilmente in modo relativo al sesso e soprattutto alle settimane di gestazione
###un bambino nato prematuro non potrà mai pesare quanto un bambino nato al termine dei 9 mesi di gestazione.
###la divisione in classi di peso ci dice però che il 70% dei bambini nasce con un peso tra 3 e i 4 kg
##mentre sono di più i bambini che nascono con peso superiore a 4 kg sono circa il 9 %
##quelli nati con peso inferiore a 3 kg sono circa il 20%. Quelli nati con peso inferiore a 2 kg sono solo il 2% circa.

###lunghezza ---> mi aspetto che sia strettamente correlata al peso
par(mfrow=c(1,2))
boxplot(dati$Lunghezza)
hist(dati$Lunghezza)
##esistono diversi outliers specie sotto il limite inferiore del box (probabilmente bimbi nati prematuri?)
classi_Lung<-cut(dati$Lunghezza, breaks=c(320, 370, 420, 470, 520))
N <- dim(dati)[1]
ni<-table(classi_Lung)
fi<-ni/N
Ni<-cumsum(ni)
Fi<-Ni/N

tabella_Lung <- as.data.frame(cbind(ni,fi,Ni,Fi))
tabella_Lung$perc <- round(tabella_Lung$ni/max(tabella_Lung$Ni)*100,1)
###il 97% dei bambini ha una lunghezza maggiore di 420 cm, l'83% maggiore di 470 cm.
median(dati$Lunghezza)
mean(dati$Lunghezza)
sd(dati$Lunghezza)
moments::kurtosis(dati$Lunghezza)-3 
moments::skewness(dati$Lunghezza) 
####gli indici di forma ci suggeriscono una distribuzione simile a quella del peso
###come possiamo aspettarci si tratta di una distribuzione asimmetrica, spostata verso destra e 
###poco appuntita, la media è leggermente diversa dalla mediana e la sd è molto più bassa
###di quella del peso, anche se qui ovviamente parliamo appunto di lunghezze in cm e di peso in Kg


###cranio ---> mi aspetto che sia strettamente correlata al peso
par(mfrow=c(1,2))
boxplot(dati$Cranio)
hist(dati$Cranio)
##esistono diversi outliers specie sotto il limite inferiore del box (probabilmente bimbi nati prematuri?)
classi_cranio<-cut(dati$Cranio, breaks=c(260, 285, 310, 335, 360))
N <- dim(dati)[1]
ni<-table(classi_cranio)
fi<-ni/N
Ni<-cumsum(ni)
Fi<-Ni/N

tabella_cranio <- as.data.frame(cbind(ni,fi,Ni,Fi))
tabella_cranio$perc <- round(tabella_cranio$ni/max(tabella_cranio$Ni)*100,1)
###il 96% dei bambini ha una lunghezza del cranio superiore a 310 cm
median(dati$Cranio)
mean(dati$Cranio)
sd(dati$Cranio)
#la media è molto vicina alla mediana e standard deviation non elevatissima
moments::kurtosis(dati$Cranio)-3 
moments::skewness(dati$Cranio) 
###gli indici di forma suggeriscono una leggera asimmetria verso dx ed una distribuzione più schiacciata

###tipo parto
table(dati$Tipo.parto)
(728/(728+1772))*100
##il 29 % circa delle mamme ha avuto un parto cesareo

table(dati$Ospedale)
###i parti sembrano abbastanza distribuiti in tutti gli ospedali

table(dati$Sesso)
###sono nate leggermente + femmine rispetto ai maschi

###fine quesito 3




##quesito 4
medie_peso <- c(NA)
medie_lun <- c(NA)
dev_std_lun <- c(NA)
dev_std_lun <- c(NA)
num_campioni <- 1000  
n <- 1000 ##se aumento questo, la media la sd si avvicineranno a quella del campione
###la precisione dello stimatore quindi aumenterà

set.seed(2)

for(i in 1:num_campioni){
  sample_lun <- sample(dati$Lunghezza, n)
  sample_peso <- sample(dati$Peso, n)
  medie_lun[i] <- mean(sample_lun)
  medie_peso[i] <- mean(sample_peso)  
  dev_std_lun[i] <- sd(sample_lun)
  dev_std_peso[i] <- sd(sample_peso)
  
}

plot(density(medie_peso))
plot(density(medie_lun))
media_vera_lun = mean(dati$Lunghezza)
media_vera = mean(dati$Peso)
mean(medie_lun) ; media_vera_lun
mean(medie_peso) ; media_vera

t.test(dati$Peso, y = medie_peso)
t.test(dati$Lunghezza,y = medie_lun)
###da ciò che è scaturito dall'analisi, non possiamo ritenere questo campione
###rappresentativo della popolazione

#utilizzando come mu la media dei campioni creati ad hoc, il p-value è leggermente più basso
##probabilmente servirebbero più dati


###QUESITO 5 - VERIFICA DIFFERENZE SIGNIFICATIVE TRA I DUE SESSI
dati_maschi <- dati[dati$Sesso == 'M',]
dati_femmine <- dati[dati$Sesso == 'F',]

###MASCHI
medie_peso_m <- c(NA)
medie_lun_m <- c(NA)
dev_std_lun_m <- c(NA)
dev_std_peso_m <- c(NA)
num_campioni <- 1000  
n <- 1000 ##se aumento questo, la media la sd si avvicineranno a quella del campione
###la precisione dello stimatore quindi aumenterà

set.seed(2)

for(i in 1:num_campioni){
  sample_lun_m <- sample(dati_maschi$Lunghezza, n)
  sample_peso_m <- sample(dati_maschi$Peso, n)
  medie_lun_m[i] <- mean(sample_lun_m)
  medie_peso_m[i] <- mean(sample_peso_m)  
  dev_std_lun_m[i] <- sd(sample_lun_m)
  dev_std_peso_m[i] <- sd(sample_peso_m)
  
}

plot(density(medie_peso_m))
plot(density(medie_lun_m))
media_vera_lun_m = mean(dati_maschi$Lunghezza)
media_vera_m = mean(dati_maschi$Peso)
mean(medie_lun_m) ; media_vera_lun_m
mean(medie_peso_m) ; media_vera_m

t.test(dati_maschi$Peso, mu= mean(medie_peso_m))
t.test(dati_maschi$Lunghezza,mu= mean(medie_lun_m))

       
######FEMMINE
medie_peso_f <- c(NA)
medie_lun_f <- c(NA)
dev_std_lun_f <- c(NA)
dev_std_peso_f <- c(NA)
num_campioni <- 1000  
n <- 1000 ##se aumento questo, la media la sd si avvicineranno a quella del campione
###la precisione dello stimatore quindi aumenterà

set.seed(2)

for(i in 1:num_campioni){
  sample_lun_f <- sample(dati_femmine$Lunghezza, n)
  sample_peso_f <- sample(dati_femmine$Peso, n)
  medie_lun_f[i] <- mean(sample_lun_f)
  medie_peso_f[i] <- mean(sample_peso_f)  
  dev_std_lun_f[i] <- sd(sample_lun_f)
  dev_std_peso_f[i] <- sd(sample_peso_f)
  
}

plot(density(medie_peso_f))
plot(density(medie_lun_f))
media_vera_lun_f = mean(dati_femmine$Lunghezza)
media_vera_f = mean(dati_femmine$Peso)
mean(medie_lun_f) ; media_vera_lun_f
mean(medie_peso_f) ; media_vera_f

t.test(x = dati_femmine$Peso, y= sample_peso_f)
t.test(x = dati_femmine$Lunghezza, y= sample_peso_f)
###L'unica variabile che è rappresentativa della popolazione è la lunghezza
###delle femmine, il resto hanno restituito p-value superiore a 0.05;

###proviamo a verificare cosa accade per la circonferenza della testa (variabile cranio)

medie_cranio_f <- c(NA)
medie_cranio_m <- c(NA)
dev_std_cranio_f <- c(NA)
dev_std_cranio_m <- c(NA)
num_campioni <- 1000  
n <- 1000 ##se aumento questo, la media la sd si avvicineranno a quella del campione
###la precisione dello stimatore quindi aumenterà

set.seed(2)

for(i in 1:num_campioni){
  sample_cranio_m <- sample(dati_maschi$Cranio, n)
  sample_cranio_f <- sample(dati_femmine$Cranio, n)
  
  medie_cranio_f[i] <- mean(sample_cranio_f)
  medie_cranio_m[i] <- mean(sample_cranio_m) 
  dev_std_cranio_f[i] <- sd(sample_cranio_f)
  dev_std_cranio_m[i] <- sd(sample_cranio_m)
  
}

plot(density(medie_cranio_f))
plot(density(medie_cranio_m))

t.test(x = dati_femmine$Cranio, y= sample_cranio_f)
t.test(x = dati_maschi$Cranio, y= sample_cranio_m)
###le differenze nel diametro del cranio tra maschi e femmine 
### non sembrano essere statisticamente significative


###QUESITO 6

##proposta 1 --> calcolo diretto
dati_osp1 <- dati[dati$Ospedale == 'osp1',]
dati_osp1 <- dati[dati_osp1$Tipo.parto == 'Ces',]
table(dati_osp1$Tipo.parto) ### n = 221
dati_osp2 <- dati[dati$Ospedale == 'osp2',]
dati_osp2 <- dati[dati_osp2$Tipo.parto == 'Ces',]
table(dati_osp2$Tipo.parto) ###n = 206
dati_osp3 <- dati[dati$Ospedale == 'osp3',]
dati_osp3 <- dati[dati_osp3$Tipo.parto == 'Ces',]
table(dati_osp3$Tipo.parto) ###n = 205
###nell'ospedale 1 si fanno più parti cesarei
attach(dati)


###proposta 2, t.test
dati$Ospedale[dati$Ospedale=="osp1"] <- 1
dati$Ospedale[dati$Ospedale=="osp2"] <- 2
dati$Ospedale[dati$Ospedale=="osp3"] <- 3

dati$Tipo.parto[dati$Tipo.parto=="Nat"] <- 0
dati$Tipo.parto[dati$Tipo.parto=="Ces"] <- 1

dati$Tipo.parto <- as.numeric(dati$Tipo.parto)
dati$Ospedale <- as.numeric(dati$Ospedale)
boxplot(Tipo.parto~Ospedale)
#dal boxplot non si evince nulla
###verifichiamo col pairwise

pairwise.t.test(Tipo.parto, Ospedale, paired = F, pool.sd = T,
                p.adjust.method = "none")

###tutti i p-value sono superiori al valore consentito, quindi possiamo
###dire che anche se dai calcoli uno dei tre ospedali presenta un numero 
###leggermente più alto rispetto agli altri due di parti cesarei, non possiamo
####accettare l'ipotesi come vera traslando le considerazioni alla popolazione

### proposta 3
chisq.test(dati$Tipo.parto, dati$Ospedale)

###ovviamente la prima proposta è legata ad un calcolo numerico su dati di soli
###tre ospedali e questo non può mai essere generalizzato

###sia la proposta 2, sia la proposta 3 ci portano nella stessa direzione:
##quindi è improbabile che in un ospedale ci sia un numero di parti cesarei significativamente più alti




###SECONDA PARTE

###QUESITO 1 - bis
####ANALISI MULTIDIMENSIONALE
###andiamo a verificare quanto le variabili sono legate al peso del nascituro

cor(x = dati$Peso, y = dati$Lunghezza)
library(openair)
scatterPlot(dati, x = "Peso", y = "Lunghezza", linear = T, pch = 10)
###la correlazione con la lunghezza, come ci aspettavamo, è elevata, come si evince anche
###dallo scatterplot, che ci dice che si ha un aumento di un cm di lunghezza circa ogni
### 40 g di aumento di peso

par(mfrow=c(2,2))
cor(x = dati$Peso, y = dati$Gestazione)
scatterPlot(dati, x = "Peso", y = "Gestazione", linear = T, pch = 10, col = "blue2")
###debole correlazione, ma è una variabile che può interessare
cor(x = dati$Peso, y = dati$Cranio)
scatterPlot(dati, x = "Peso", y = "Cranio", linear = T, pch = 10, col = "red2")
###discreta correlazione, è una variabile importante
cor(x = dati$Peso, y = dati$Anni.madre)
scatterPlot(dati, x = "Peso", y = "Anni.madre", linear = T, pch = 10, col = "green2")
##nuvola dispersa, sicuramente non è una variabile che può interessare

cor(x = dati$Lunghezza, y = dati$Gestazione)
cor(x = dati$Cranio, y = dati$Lunghezza)
cor(x = dati$Cranio, y = dati$Gestazione)
#come possiamo vedere, le variabili numeriche sono ben correlate, positivamente,
###tra di loro, quindi possono tutte essere usate per la costruzione del modello

###ho cercato di dividere in base al sesso, ma i coefficienti di correlazione non hanno
###mostrato una variazione significativa

attach(dati)
mod_peso1<-lm(Peso~Lunghezza+Gestazione+Cranio)
summary(mod_peso1)
###il modello è già ottimo perché abbiamo un R^2 superiore a 0.7 e tutte le variabili
##che abbiamo inserito nel modello sono significative
mod_peso2<-lm(Peso~Lunghezza+Gestazione+Cranio+Anni.madre+Fumatrici+Ospedale+Sesso)
summary(mod_peso2)

mod_peso3<-lm(Peso~Lunghezza+Cranio)
summary(mod_peso3)


#QUESITO - 2 - bis
mod_peso2<-lm(Peso~Lunghezza+Gestazione+Cranio+Anni.madre+Fumatrici+Ospedale+Sesso+Tipo.parto)
summary(mod_peso2)

mod_peso4<-lm(Peso~Lunghezza+Sesso)
summary(mod_peso4)

BIC(mod_peso1,mod_peso2,mod_peso3, mod_peso4)
##possiamo tranquillamente prendere il modello 1 o 3 perché sacrificando moltissime variabili
###abbiamo comunque un ottimo r quadro e il BIC non sale di molto.


###QUESITO 3 - bis
library(broom)
rbind("mod1" = glance(mod_peso1), "mod2" = glance(mod_peso2), "mod3" = glance(mod_peso3), "mod4" = glance(mod_peso4), "mod5" = glance(mod_peso))
###il modello migliore potrebbe essere il mod_peso2, che è quello con praticamente
##tutte le variabili, questo è subito seguito dal mod_peso3, perché solo con lunghezza e cranio
###riusciamo ad avere un'ottima stima del peso, adesso andiamo a vedere il suo MSE.
stepwise <- MASS::stepAIC(
  mod_peso2, direction = "both", k = log(n))
summary(stepwise)
BIC(mod_peso3, stepwise) ###la procedura stepwise ha scelto lo stesso che avrei scelto io, a mano.

##QUESITO 4 - bis
###dagli scatterplot visti precedentemente ho notato una lieve curvatura nelle due
###variabili fondamentali, quindi le ho poste all'interno del modello migliore, migliorandolo
mod_update <- update(mod_peso3,~.+I(Gestazione^2)+I(Lunghezza^2)+(Cranio*Gestazione)-Cranio)
summary(mod_update)
summary(mod_peso3) ###i summary ci confermano il miglioramento del modello.

###QUESITO 5 - bis
###lev
lev<-hatvalues(mod_update)
plot(lev)
p<-sum(lev)
n<-length(lev)
soglia=2*p/n
abline(h=soglia,col=2)
lev[lev>soglia]

##outliers
library(car)
plot(rstudent(mod_update))
abline(h=c(-2,2), col = 3)
outlierTest(mod_update)
###non ci sono valori troppo lontani, essendo l'indice compreso tra 0 e 1 possiamo notare
###che probabilmente gli outliers non hanno molta influenza sulla nuvola in basso
###ci sono solo 3 outliers e gli applichiamo la corr di Bonferroni
##distanza di cook
cook<-cooks.distance(mod_update)
plot(cook,ylim = c(0,1)) 
max(cook) ###il massimo è di 2.14, supera il valore soglia di 0.5 e quello di allarme di 1
###ovviamente anche la distanza di cook ci restituisce le stesse considerazioni

par(mfrow=c(2,2))
plot(mod_update)
###solo un punto supera la soglia dello 0.5 del leverage e per quanto
###riguarda la distribuzione, la coda in alto sembra essere un po spostata più verso l'alto

library(lmtest)
bptest(mod_update)
dwtest(mod_update)
shapiro.test(residuals(mod_update))
plot(density(residuals(mod_update)))
###a parte il dw il resto dei test dà ottimi risultati



MSE<-function(y_oss,y_prev){
  return(sum((y_oss-y_prev)^2)/length(y_prev))
}

##oppure

summ <- summary(mod_update)
mse <- mean(summ$residuals^2) ##l'mse è molto alto quindi considerando
###un r^2 non superiore all'80% e un mse altino, penso che il modello
###non sia il massimo a livello predittivo

###################
####QUESITI 7-8####
###################
mod_prova <- lm(Peso~Gestazione+N.gravidanze+Sesso)
gest <- 39
grav <- 3
sex <- "F"
tabella_pred <- as.data.frame(cbind(gest, grav, sex))
mean(predict(mod_prova, tabella_pred))
tab_prev <- predict(mod_prova, tabella_pred)
mse_test <- MSE(dati$Peso, tab_prev)
mse_train<-MSE(dati$Peso, fitted(mod_prova))


lmtest::bptest(mod_prova)
lmtest::dwtest(mod_prova)
shapiro.test(mod_prova$residuals)
plot(density(residuals(mod_prova)))

library(ggplot2)
ggplot(data=dati)+
  geom_point(aes(x=Gestazione,
                 y=Peso,col=Sesso),position = "jitter")+
  geom_smooth(aes(x=Gestazione,
                  y=Peso,col=Sesso),se=F,method = "lm")

###test 2
mod_prova2 <- lm(Peso~Gestazione)

tabella_pred2 <- as.data.frame(cbind(gest))
mean(predict(mod_prova, tabella_pred2))
tab_prev2 <- predict(mod_prova2, tabella_pred2)
mse_test <- MSE(dati$Peso, tab_prev2)
mse_train<-MSE(dati$Peso, fitted(mod_prova2))
library(ggplot2)
ggplot(data=dati)+
  geom_point(aes(x=Gestazione,
                 y=Peso),position = "jitter")+
  geom_smooth(aes(x=Gestazione,
                  y=Peso),se=F,method = "lm")

###anche con il cambio di variabili, utilizzando solo la gestazione
###per prevedere il peso, la media delle previsioni è la stessa
##con due variabili il modello diventa più facilmente visualizzabile
##l'MSE è abbastanza alto, ma i residui sono normalmente distribuiti


