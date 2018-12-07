# Gestion des données avec PostGres

## Configuration

### Recette
- Création utilisateur administrateur au niveau UNIX (typiquement *postgres*)
- Création du répertoire censé contenir le groupe de base de données
- Passage en utilisateur *postgres*
- *initdb* option importante pour initialisation base de données
  - --pgdata/-D : chemin vers le répertoire du cluster
  - --pwprompt/-W : mot de passe admin DB
  - --locale typiquement "en_US.UTF-8"
  - -E encodage "UTF8"
- Fichiers importants
   - Postgresql.conf : paramètres importants pour le processus (ressource allocation, logging, ...)
   - pg_hba.conf
- Extensions (en particulier intégration des langages R et Python ayavec Posgres)
- Création des utilisateurs avec leurs droits associés

### Pour OpenBSD
Pour la configuration avec OpenBSD (voir le fichier readme fournie avec l'installation de Postgres).
Une des particularités de cette configuration est que l'utilisateur UNIX qui démarre le démon a un nom 
différent que l'admin PostGres (_postgresql pour l'utilisateur UNIX, postgres pour l'admin).
Par ailleurs, il est possible de démarrer le démon avec rcctl start postgresql, qui démarre le démon 
avec un répertoire de groupe de base de données vers /var/postgres/data.

Le readme conseille à juste titre de ne pas utiliser le compte admin pour faire des opérations courantes
et de le réservé à une fonction de  maintenance et configuration.

Le passage d'une version à une autre de PostGres peut imposer une importation des données.

### Pour ArchLinux

Le chemin d'accès vers le groupe de base de donnée est "/var/lib/postgres/data/".
Le démon postgres se demare avec systemd (systemctl start postgresql)


## Création de la table

On a mise en place la base de données avec:
```{sql}
CREATE TABLE fare(
       id char(40),
       fare numeric(5, 2),
       date timestamp,
       ilong double precision,
       ilat double precision,
       olong double precision,
       olat double precision,
       pcount integer);
```

On va fournir importer les dix premièrs éléments de notre base de données:
```{sql}
\copy fare FROM '10.csv.gz' WITH (FORMAT CSV);
```

On peut aussi importer des données compressées:
```{sh}
gzip -dc sorted.csv.gz |  psql -c '\copy fare FROM stdin WITH (FORMAT CSV);'
```
