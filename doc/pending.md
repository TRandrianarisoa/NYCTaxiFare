# Suivi

## Pré-traitement
- [X] Ordonnement et partionnement du jeu de données suivant l'année mois
- [ ] Extraction des données par quadrillage via un script python			[non-prioritaire]
	- [ ] Définir la zone prise en compte et la dimension de la grille
	- Utiliser division euclidienne préserver la lattitude et longitude dans l'export
	- division en fichiers séparés via une convention de nommage typiquement 45x7.csv.gz
- [X] Réflexion sur l'usage d'une base de donnée plutot qu'une série de fichier 
	- Le choix de PGSQL a été retenue les arguments en faveur sont:
		1. Simplification du prétraitement [faible]
		2. Utilisation d'algorithmes éprouvés pour l'accès et la conservation des données [fort]
		3. Possibilité accrue de croiser des données extérieurs [justification totale]
	- MongoDB n'a pas été retenue à cause de mes compétences limitées et de l'inadéquation avec des
	  données aussi structurées que celle d'un CSV. N'oublions pas que les données ne sont pas modifiées.
	- Mais, ... nécessité de documenter TOUT le procédé de configuration pour qu'il soit le plus reproductible
	  et nécessité d'apprendre des notions nouvelles rapidement.
	- Import des données

## Visualisation
- [ ] Série temporelle pour une région donnée						[facile]
- [ ] Visualisation des événements d'une journée seulement en temps-réel		[difficile]+[non-prioritaire]
	- [X] Choix des technologies + preuve de faisabilité: Python + Glumpy
	- [ ] Affichage de points de manière statique avec coord Long/Lat		[moyen]
	- [ ] Prise en compte de la composante temporelle				[nécessite réflexion]
	- [ ] Gestion du téléversement vers GPU des donnéees de manière efficace	[nécissite réflexion]

# Modélisation
- Restriction à une zone quadrillée							[trop tot necessite visu]
