dane<-Dane_FBI[1:11]
attach(dane)

#rozklad a prori
#m0 - srednia: 1 020 003
#v0 - odchylenie standardowe: 1 613 702

rozklad_apriori<-rnorm(20, mean=1020003, sd= 1613702)
gestosc_apriori<-density(rozklad_apriori)

#rozklad a posteriori
#m1 - srednia: 1 423 196,582
#v0 - odchylenie standardowe: 313 437,4105

rozklad_aposteriori<-rnorm(20, mean=1423196.582, sd= 313437.4105)
gestosc_aposteriori<-density(rozklad_aposteriori)

#rozklad_f.wiarygodnosci

rozklad_wiarygodnosci<-rnorm(20, mean=1439004.35, sd=  319522.714)
gestosc_wiarygodnosci<-density(rozklad_wiarygodnosci)
#dwa na jednym
plot(density(rozklad_apriori), ylim=c(0, 10*(10^(-7))))
lines(gestosc_aposteriori, col="green")
lines(gestosc_wiarygodnosci, col="blue")
#wartosci oczekiwane
#a priori m0: 1 020 003
#a posteriori m1: 1 423 196,582

#modalna
#a priori
x<-rozklad_apriori
estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}
estimate_mode(x)
#a posteriori
x<-rozklad_aposteriori
estimate_mode(x)

#mediany
qnorm(0.5, mean=1020003, sd= 1613702)
qnorm(0.5, mean=1423196.582, sd= 313437.4105)

#odchylenie standardowe
#a priori v0: 1 613 702
#a posteriori v1: 313 437,4105

#kwantyle rzedu 0,25 i 0,75
qnorm(0.25, mean=1020003, sd= 1613702)
qnorm(0.75, mean=1020003, sd= 1613702)

qnorm(0.25, mean=1423196.582, sd= 313437.4105)
qnorm(0.75, mean=1423196.582, sd= 313437.4105)

#90%-wy kwantylowy przedzial wiarygodnosci
lewo<-qnorm(0.025, mean=1020003, sd= 1613702)
prawo<-qnorm(0.975, mean=1020003, sd= 1613702)
przedzial_apriori<-prawo-lewo
przedzial_apriori

lewo<-qnorm(0.025, mean=1423196.582, sd= 313437.4105)
prawo<-qnorm(0.975, mean=1423196.582, sd= 313437.4105)
przedzial_aposteriori<-prawo-lewo
przedzial_aposteriori
