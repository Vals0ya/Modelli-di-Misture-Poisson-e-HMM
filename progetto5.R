library(mhsmm)
library(dplyr)
library(ggplot2)
names(soap) <- c("vendite") #rinomino la variabile

#grafico vendite settimanali del sapone in circa 5  anni
ggplot(soap, aes(x = seq_along(vendite), y = vendite)) +
  geom_line() +
  labs(x = "Settimana", y = "Vendite del Sapone", title = "Vendite Settimanali del Sapone") +
  scale_x_continuous(breaks = seq(1, nrow(soap), by = 10))

# Aggiungi una colonna che rappresenta il gruppo di 52 settimane
soap$gruppo <- rep(1:ceiling(nrow(soap) / 52), each = 52, length.out = nrow(soap))

# Crea un grafico con facet_wrap
ggplot(soap, aes(x = seq_along(vendite), y = vendite)) +
  geom_line() +
  labs(x = "Settimana", y = "Vendite del Sapone", title = "Vendite Settimanali del Sapone") +
  facet_wrap(~gruppo, scales = "free")  # La funzione facet_wrap divide il grafico in sottogruppi


# Calcola la media delle vendite per ogni gruppo
media_vendite_per_gruppo <- soap %>%
  group_by(gruppo) %>%
  summarize(media_vendite = mean(vendite))

max(soap$vendite) 


# -- ----------------------------------------------------------------------


#Fit modelli di misture Poisson con uno, due, tre e quattro cluster.
#Quanti cluster ritieni necessari?

mean(soap$vendite) #vediamo la media e osserviamo che la maggior parte dei valori rientra tra il 5 e il 10


#1.

K <- 1
val_iniziale1 <- hmmspec(init = rep(1/K, K),
                     trans = matrix(1/K, nrow = K, ncol = K),
                     parms.emis = list(lambda = c(5)),
                     dens.emis = dpois.hsmm)
fit1 <- hmmfit(soap[,1], val_iniziale1, mstep = mstep.pois)
#Per ottenere un modello significativo, dovresti impostare K su un valore maggiore di uno.


#2
K <- 2
val_iniziale2 <- hmmspec(init = rep(1/K, K),
                     trans = matrix(1/K, nrow = K, ncol = K),
                     parms.emis = list(lambda = c(5,10)),
                     dens.emis = dpois.hsmm)
fit2 <- hmmfit(soap[,1], val_iniziale2, mstep = mstep.pois)
fit2$model
#Plot del logaritmo di verosomiglianza durante l'Adattamento(come evolve la verosomiglianza):
plot(fit2$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")

#Predizione degli Stati Nascosti:
stati_nascosti2<- fit2$yhat #classificazione
plot(soap[,1],col=stati_nascosti2,main = "numero di vendite")

#Linee Orizzontali per i Parametri di Emissione(divise per stato):
abline(h=fit2$model$parms.emission$lambda[1])
abline(h=fit2$model$parms.emission$lambda[2],col=2)


round(fit2$model$transition,2)
fit2$model #guardo lamba per vedere quanto sono separate 

#3

K <- 3
val_iniziale3 <- hmmspec(init = rep(1/K, K),
                     trans = matrix(1/K, nrow = K, ncol = K),
                     parms.emis = list(lambda = c(5, 10, 15)),
                     dens.emis = dpois.hsmm)
fit3 <- hmmfit(soap[,1], val_iniziale3, mstep = mstep.pois)
fit3$model
#Plot del Log-Likelihood durante l'Adattamento:
plot(fit3$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iterazione")

#Predizione degli Stati Nascosti:
stati_nascosti3 <- fit3$yhat
plot(soap[,1],col=stati_nascosti3,main = "numero di vendite")

#Linee Orizzontali per i Parametri di Emissione:
abline(h=fit3$model$parms.emission$lambda[1])
abline(h=fit3$model$parms.emission$lambda[2],col=2)
abline(h=fit3$model$parms.emission$lambda[3],col=3)

round(fit3$model$transition,3)
fit3$model #guardo lamba
#4

K <- 4
val_iniziale4 <- hmmspec(init = rep(1/K, K),
                     trans = matrix(1/K, nrow = K, ncol = K),
                     parms.emis = list(lambda = c(5,10,15,20)),
                     dens.emis = dpois.hsmm)
fit4 <- hmmfit(soap[,1], val_iniziale4, mstep = mstep.pois)
fit4$model
#Plot del Log-Likelihood durante l'Adattamento:
plot(fit4$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")

#Predizione degli Stati Nascosti:
stati_nascosti4 <- fit4$yhat
plot(soap[,1],col=stati_nascosti4,main = "numero di vendite")


#Linee Orizzontali per i Parametri di Emissione:
abline(h=fit4$model$parms.emission$lambda[1])
abline(h=fit4$model$parms.emission$lambda[2],col=2)
abline(h=fit4$model$parms.emission$lambda[3],col=3)
abline(h=fit4$model$parms.emission$lambda[4],col=4)


round(fit4$model$transition,4) #overfitting nel verde, perche nella diag princ è 0, infatti il verde è incluso tra il rosso e l'azzurro. Perciò capisco che servono 3 cluster
fit4$model #guaro lamba

#analizzo le verosomiglianza che devono esssere massimizzate
fit2$loglik #-618
fit3$loglik# -610
fit4$loglik #-607
#tra 3 e 4 cambia poco

# calcolo il numero di parametri.
num_parametri2<-c((2-1)+2*(2-1)+2)
num_parametri3<- c((3-1)+3*(3-1)+3)
num_parametri4<- c((4-1)+4*(4-1)+4)


#vediamo l'aic. formula: AIC=−2×log-likelihood+2×k
AIC2<- -2*fit2$loglik[length(fit2$loglik)]+2*num_parametri2
AIC3<- -2*fit3$loglik[length(fit3$loglik)]+2*num_parametri3
AIC4<- -2*fit4$loglik[length(fit4$loglik)]+2*num_parametri4
AIC_tot<- c(AIC2,AIC3,AIC4) #migliore infatti è quello con 3 cluster 


#vediamo il BIC. formula:BIC=−2×log-likelihood+k×log(n)
BIC2 <- -2 * fit2$loglik[length(fit2$loglik)] + num_parametri2 * log(length(soap[,1]))
BIC3 <- -2 * fit3$loglik[length(fit3$loglik)] + num_parametri3 * log(length(soap[,1]))
BIC4 <- -2 * fit4$loglik[length(fit4$loglik)] + num_parametri4 * log(length(soap[,1]))
BIC_tot <- c(BIC2, BIC3, BIC4)#secondo bic è quello con 2 cluster 

