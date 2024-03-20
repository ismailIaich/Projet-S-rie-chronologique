#projet: 

#les variable:
# data : les données de fichier csv
# ts_data : la série temporelle qui représente les données
#lts : logarithme de la série temporelle des données
#SCD : la différentiation de logarithme pour éliminer la tendance
#modele : le meilleur modèle SARMA




#ETAPE 1 : Charger la base de données: 


# Charger les données CSV: 
data <- read.csv("C:/Users/hp/Documents/Rscripts/Projet/SHOES.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Afficher les premières lignes des données
head(data)


#ETAPE 2 : Créer et Présenter Notre Série temporelle

# Convertir la colonne Date en format Date
data$DATE <- as.Date(data$DATE)

# Sélectionner les lignes avec des dates dans la plage spécifiée
subset_data <- subset(data, DATE >= as.Date("1992-01-01") & DATE <= as.Date("2023-11-01"))

# Créer une série temporelle à partir des données sélectionnées
ts_data <- ts(subset_data$MSALES, start = c(1992, 1), end = c(2023, 11), frequency = 12)


#Afficher un graphe interactif pour notre données
# Charger la bibliothèque plotly
library(plotly)

# Convertir la série temporelle en un data frame
ts_df <- data.frame(Date = time(ts_data), Value = ts_data)

# Tracer la série temporelle interactive
p <- plot_ly(data = ts_df, x = ~Date, y = ~Value, type = 'scatter', mode = 'lines') %>%
  layout(title = "Les ventes au détail d'un magasin de chaussures", xaxis = list(title = "Date"), yaxis = list(title = "Millions des dollars"))

# Afficher le graphique interactif
p


#stabiliser la variance (appliquant le logarithme) :
lts=log(ts_data)

#Afficher le log des données:
# Convertir la série temporelle en un data frame
ts_df <- data.frame(Date = time(lts), Value = lts)

# Tracer la série temporelle interactive
p <- plot_ly(data = ts_df, x = ~Date, y = ~Value, type = 'scatter', mode = 'lines') %>%
  layout(title = "Série temporelle des ventes au détail (Logarithme des ventes)", xaxis = list(title = "Date"), yaxis = list(title = "Millions des dollars"))

# Afficher le graphique interactif
p

#Afficher les composants de notres séries (tendance , variance , résidus)
# Appliquer la décomposition à la série temporelle logarithmée
decomposed <- decompose(lts)

# Créer un graphique avec les composantes de la décomposition
par(mfrow=c(4,1))  
# Tracer la série temporelle d'origine
plot(decomposed$x, type="l", col="black", main="Série temporelle d'origine", ylab="Logarithme des ventes")

# Tracer la tendance estimée
plot(decomposed$trend, type="l", col="blue", main="Tendance", ylab="")

# Tracer la composante saisonnière
plot(decomposed$seasonal, type="l", col="green", main="Composante saisonnière", ylab="")

# Tracer les résidus
plot(decomposed$random, type="l", col="red", main="Résidus", ylab="")


#ETAPE 3: Etudier les caractéristique de la série temporelle du log(donnees)

#Test de la stabilité de série temporelle :
adf.test(ts_data)

#La saisonnalité :

lag.plot(rev(lts),24,layout=c(8,3),do.lines=FALSE,diag.col="red",col.main="blue") 

#ETAPE 4: éliminer la saisonnalité:

#On utilise une différentiation d'ordre 12
SCD=diff(lts,lag=12)
#Vérifier que la saisonnalité est éliminé
plot.ts(SCD)
lag.plot(rev(SCD),24,layout=c(8,3),do.lines=FALSE,diag.col="red",col.main="blue") 


#ETAPE5: Chercher les paramètres du modèle 
#Utilisons le ACF et le PACF
par(mfrow=c(1,2))
acf(SCD, lag.max = 26)
pacf(SCD, lag.max = 26)

#ETAPE6: Chercher le meilleur modèle pour notre série:

#on va utiliser la boucle suivante qui teste toutes les combinaisons possible et choisie le modèle avec le plus faible AIC
# Initialisation des variables pour stocker le meilleur modèle et son critère d'information
meilleur_modele <- NULL
meilleur_critere_info <- Inf

for (p in 0:3) {
  for (q in 0:4) {
    for (P in 0:3) {
      for (Q in 0:1) {
        # Exclure les combinaisons non valides
        if (p + q + P + Q <= 11) {
          tryCatch({
            # Ajuster le modèle SARMA
            modele <- arima(lts, order = c(p, 0, q), seasonal = list(order = c(P, 1, Q), method = "ML" ,period = 12))
            
            # Calculer le critère d'information (AIC ici)
            critere_info <- AIC(modele)
            
            # Mettre à jour le meilleur modèle si nécessaire
            if (critere_info < meilleur_critere_info) {
              meilleur_modele <- modele
              meilleur_critere_info <- critere_info
            }
          }, error = function(e) {
            cat("Erreur lors de l'ajustement du modèle avec les paramètres (p, q, P, Q):", p, q, P, Q, "\n")
          })
        }
      }
    }
  }
}

# Vérifier si le meilleur modèle a été trouvé
if (!is.null(meilleur_modele)) {
  # Afficher l'AIC et les paramètres du meilleur modèle
  cat("AIC du meilleur modèle:", meilleur_critere_info, "\n")
  cat("Paramètres du meilleur modèle SARMA:\n")
  cat("p =", meilleur_modele$arma[1], "\n")
  cat("d =", 0, "\n")  # La différenciation est fixée à 1
  cat("q =", meilleur_modele$arma[2], "\n")
  cat("P =", meilleur_modele$arma[3], "\n")
  cat("D =", 1, "\n")  # La différenciation saisonnière est fixée à 1
  cat("Q =", meilleur_modele$arma[4], "\n")
} else {
  cat("Aucun meilleur modèle trouvé.")
}





#ETAPE 7:etudier la validité de modèle choisi 

modele <-arima(lts, order = c(3, 0, 4), seasonal = list(order = c(3, 1, 1), period = 12))

#coefficients du modèle:
modele$coef


#significativité des paramètres de modèle
require(caschrono)
t_stat(modele)

#test de stabilité de variance 
Box.test.2(residuals(modele),nlag=10,type = "Box-Pierce")

#test de normalité des résidus: 
shapiro.test(residuals(modele))

#stabilté de la variance et l'indépendance des résidus (bruit blanc)
acf(residuals(modele))
pacf(residuals(modele))

#ETAPE 8 : Prévision
par(mfrow=c(1,1))
P=forecast(modele,level = 95)
#Affichage en format numérique
P
#Graphe de prévision
P_l <- ts(P$lower, start = c(2023, 12), frequency = 12)
P_u <- ts(P$upper, start = c(2023, 12), frequency = 12)
P_m <- ts(P$mean, start = c(2023, 12), frequency = 12)

lines(ts_data,col="red")
ts.plot(lts,P_m,P_l,P_u,col=c(1,2,3,4),lty=c(1,1,2,2))




