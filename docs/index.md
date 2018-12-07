# Prédiction du prix d'une course d'un taxi new-yorkais
## Présentation des données
L'ensemble de données de base est issue d'une compétition Kaggle. Il établit un lien entre le prix d'une course
les points de départ et d'arrivée et la date à la seconde près d'une course. Cet ensemble de données contient
des *données manquantes* en particulier les données spatiales manquantes sont codées par 0 plutôt que NAN.

Nous allons l'étofer par des données externes. Bien qu'il soit un peu tôt, les propositions sont actuellement 
dans l'ordre de priorités:
1. données météologiques
2. données des événements (concerts, match, réunion politique, manifestation, catastrophes, ...)
3. données du traffic routier
4. Calcul d'itinéraire (km, durée via un algorithme ou une API Internet)

## Pré-traitement
Les données ne sont pas ordonnés. Nous avons utilisés des scripts pour pouvoir à la fois ordonner les données
et ensuite les découper suivant l'année (voir les fichiers shell du dossier scripts).
Par ailleurs, les fichiers données sont tous compressés au format gzip utilisable avec tidyverse (division 
du poids du fichier par 6).


## Visualisation des données
### Découpage temporelle
La première approche que nous avons retenue pour avoir une intuition fut de ne retenir que la médiane
pour chaque semaine des prix de la course. Les données ainsi aggrégés temporellement et spatialement
forment une série temporelle. On peut observer les premières tendances mais on a enlevé l'information spatiale.

### TODO Animation de l'évolution de la densité
On peut au lieu de faire une médiane qui enlève la spatialité afficher directement les densités changeantes 
en fonction de la semaine.
Piste: Vispy, Glumpy 


