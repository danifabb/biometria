### IMPORT E PULIZIA DEI DATASET BCI

# provo a caricare il dataframe "BCI_env.txt", tuttavia si verifica un errore: ci sono alcuni elementi mancanti NA 
# error: line 9 did not have 9 elements; nella riga 9 manca la modalit? per la variabile "Precipitation"

# devo quindi specificare alcuni argomenti, affinch? R sappia come trattare i valori mancanti
# sep = " " serve per indicare che il separatore tra i valori ? uno spazio vuoto
# in questo modo, la casella rimane vuota e non c'? slittamento a sinistra delle righe che presentano i dati mancanti
# l'argomento header = T serve per indicare che la prima riga = intestazione delle colonne
BCI_env <- read.table("data/BCI_env (2).txt", sep = " ", header = T) 

# carico la matrice di comunita' (comma-separated values)
BCI <- read.csv("data/BCI (1).csv")
View(BCI)

# conto il numero di osservazioni, specie e variabili ambientali
nrow(BCI) #50 osservazioni = 50 righe
ncol(BCI) #225 specie = 225 colonne di BCI
ncol(BCI_env) #9 variabili ambientali = 9 colonne di BCI_env

# carico il pacchetto vegan per avere le informazioni riguardo al dataset
install.packages("vegan")
library(vegan)
?BCI
# BCI = Barro Colorado Island. Dati raccolti da 50 plot da 1 ettaro ciascuno. Conteggio delle specie di alberi (matrice)
# BCI_env: 9 variabili associate agli stessi 50 plot

str(BCI_env) #50 osservazioni di 9 variabili
# le coordinate UTM.EW e UTM.NS e la misura di eterogeneit? ambientale sono vettori numerici
# i valori di precipitazione (mm/anno) e altitudine (m) sono vettori integer

unique(BCI_env$Geology) # il vettore character relativo alla formazione geologica ? formato da un unico valore "Tb" per tutti i 50 plot

unique(BCI_env$Age.cat) # il vettore character relativo all'et? della foresta ? formato da 2 valori, c2 e c3

# converto i vettori di tipo character in fattori con pi? livelli 

BCI_env$Age.cat <- factor(BCI_env$Age.cat, 
                          ordered = T, 
                          levels = c("c2", "c3")) # fattore ordinato

as.factor(BCI_env$Age.cat) # controllo che la struttura e i livelli siano corretti

BCI_env$Habitat <- factor(BCI_env$Habitat, 
                          levels = c("Young", "OldSlope", "OldLow", "OldHigh", "Swamp"))

as.factor(BCI_env$Habitat)

BCI_env$Stream <- factor(BCI_env$Stream, 
                         levels = c("Yes", "No"))

str(BCI) # vettori integer per abbondanza di specie

summary(BCI_env) 
# UTM.EW e UTM.NS: media e mediana coincidono, il primo e il terzo quartile hanno la stessa distanza dalla mediana
# distribuzione tende ad essere simmetrica 

# per precipitation e elevation si ha un unico valore
# EnvHet: min 0.0000; max 0.7264 
# EnvHet: 1st Qu 0.08; mean 0.31 - distribuzione spostata verso sinistra
# ci sono 3 NA, che voglio eliminare assieme alle righe (osservazioni) in cui sono presenti

summary(BCI) 

# rimozione degli NA
index_na <- which(is.na(BCI_env), arr.ind = T) # ottengo un indice delle posizioni dei valori NA nel dataframe
index_na[ , 1] # seleziono solo la prima colonna dell'indice, ovvero quella che elenca le righe che presentano NA
BCI_env <- na.omit(BCI_env) # elimino le osservazioni con NA nel dataframe
View(BCI_env)
BCI <- BCI[-(index_na[ , 1]), ] # elimino le stesse righe nella matrice di comunit?

#controllo il numero di osservazioni
nrow(BCI_env) #47
nrow(BCI)

#controllo che non ci siano pi? NA
anyNA(BCI_env) 


### DISTRIBUZIONE UNIVARIATA DELLE VARIABILI AMBIENTALI

# rappresentazione dei caratteri quantitativi 

hist(BCI_env$UTM.EW,
     xlab = "UTM coordinates (zone 17N) East-West",
     main = "",
     breaks = 10)

hist(BCI_env$UTM.NS,
     xlab = "UTM coordinates (zone 17N) North-South",
     main = "",
     breaks = 5) # distribuzione quasi simmetrica

hist(BCI_env$Precipitation,
     xlab = "Precipitation (mm/year)",
     xlim = c(2200, 2800),
     main = "") 

hist(BCI_env$Elevation,
     xlab = "Elevation (m)",
     xlim = c(80, 160),
     main = "")

hist(BCI_env$EnvHet,
     xlab = "Eterogeneita' ambientale (Simpson)",
     main = "",
     breaks = 8) #distribuzione asimmetrica positiva
# molti valori piccoli estremi: la classe che ha frequenza maggiore ? quella tra 0 e 0.1
# altre classi con frequenza alta: 0.4-0.5 e 0.6-0.7


# esporto quest'ultimo istogramma in formato .png
png("outputs/environmental_heterogeneity_hist.png", width = 2000, height = 1200, res = 300)
hist(BCI_env$EnvHet,
     xlab = "Eterogeneita' ambientale (Simpson)",
     main = "",
     breaks = 8)
dev.off()


# rappresentazione di caratteri qualitativi

agecat <- table(BCI_env$Age.cat)
barplot(agecat,
        xlab = "Forest age category",
        ylab = "NÂ° of plots") # il 98% dei plot sono di categoria c3

habitat <- table(BCI_env$Habitat)

barplot(habitat,
        xlab = "Habitat",
        ylab = "NÂ° di plot") # circa meta' dei plot sono foresta OldLow

stream <- table(BCI_env$Stream)
barplot(stream,
        xlab = "River",
        ylab = "NÂ° of plots") # circa 10% dei plot ha un corso d'acqua

jpeg("outputs/habitat_barplot.jpeg", width = 2000, height = 1200, res = 300)
habitat <- table(BCI_env$Habitat)
barplot(habitat,
        xlab = "Habitat",
        ylab = "NÂ° di plot",
        main = "Tipi di habitat sull'isola Barro Colorado")
dev.off()

# trasformo la matrice di abbondanza in matrice di assenza/presenza
BCI_pa <- decostand(BCI, method = "pa") 
View(BCI_pa)

# calcolo la ricchezza di specie per ogni plot
sr <- specnumber(BCI) 
View(sr) # es. nella prima osservazione sono state trovate 93 specie

# aggiungo una colonna a BCI_env per la ricchezza di specie
BCI_env$sr <- sr 
str(BCI_env) # sr e' vettore integer; abbiamo 10 variabili

hist(BCI_env$sr,
     xlab = "Ricchezza di specie",
     main = "Distribuzione della ricchezza di specie nell'isola Barro Colorado")
# ricchezza va da circa 75 a circa 110 specie per plot 
# la maggior parte dei plot hanno un numero di specie tra le 80 e le 95

### TEST DI CORRELAZIONE

# faccio il plot della ricchezza di specie rispetto alle variabili numeriche 
# controllo se le due variabili sono significativamente correlate grazie al test di correlazione
# l'ipotesi nulla e' che le due variabili NON siano correlate

plot(BCI_env$UTM.EW,
     BCI_env$sr) 
cor.test(BCI_env$UTM.EW,
         BCI_env$sr,
         alternative = "less") 
# p-value = 0.12 -> non significativo, non posso rifiutare l'ipotesi nulla
# intervallo di confidenza -0.4009874;  1.0000000 (passa da negativo a positivo, altra fonte di incertezza)
# cor = - 0.18

plot(BCI_env$UTM.NS,
     BCI_env$sr) 
cor.test(BCI_env$UTM.NS,
         BCI_env$sr,
         alternative = "greater") # p-value = 0.26

plot(BCI_env$Precipitation,
     BCI_env$sr) 
cor.test(BCI_env$Precipitation,
         BCI_env$sr,
         alternative = "greater") # p-value = NA
# la deviazione standard (ovvero la misura della variabilita') e' 0, perche' abbiamo un unico valore

plot(BCI_env$EnvHet,
     BCI_env$sr) 
cor.test(BCI_env$EnvHet, 
         BCI_env$sr,
         alternative = "greater") #p-value = 0.45 e cor = 0.021







