Le package stacomiR vous permet d'avoir accès à votre base de données sur les poissons migrateurs et de réaliser différents figures pré-programmées. Avec ce package, nous avons développé une interface java pour permettre aux personnes qui ne connaissent pas le langage sql de mettre à jour et d'ajouter des nouvelles données dans leur base de données poissons migrateurs postgreSQL.

# Procédure d'installation

Pour réaliser l'installation assurez vous d'avoir les logiciels suivants installés :
- java 8
- PostgreSQL (avec pgAdmin - interface Open source d'administration et de développement pour PostgreSQL)
- R (version 3.5.0 ou plus récente)

Vous devrez aussi :
- Créer un lien ODBC pour votre base de données poissons migrateurs (ajouter une source de type "PostgreSQL unicode" - si vous n'avez pas le driver PostgreSQL, utilisez stack builder pour installer le driver)
- Ajouter un dossier CalcmigData sur votre ordinateur (par exemple c:\users\my_session\Documents\CalcmigData)

# Créer une base de données

Avant tout chose, créez la base de données dans pgAdmin via une console SQL en exécutant le code suivant :
CREATE DATABASE bd_contmig_nat WITH ENCODING = 'UTF8';
Puis, ajoutez les différents rôles de connexion dans pgAdmin : 
pour le rôle iav
CREATE ROLE iav LOGIN PASSWORD 'iav'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
pour le rôle invite
CREATE ROLE invite LOGIN PASSWORD 'invite'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
pour le rôle user_1
CREATE ROLE user_1 LOGIN PASSWORD 'user_1'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;

Téléchargez maintenant le fichier Install_bd_stacomi.zip (disponible dans le dossier Installation) et décompressez-le . 
Vous pouvez maintenant restaurer le fichier Install_bd_stacomi.sql. Pour cela, ouvrez une commande MS DOS (vous devez premièrement sauvegarder le chemin vers Program\PostgreSQL\your_version\bin dans les variables d’environnement) :
psql -U user  bd_contmig_nat<"path_to_the_Install_bd_stacomi.sql_file"
Remplacez user par le nom que vous utilisez pour vous connecter dans pgAdmin. Cette commande fonctionne uniquement si votre base de données est installée sur votre serveur local avec le port par défaut (5432). Si votre base de données est installée sur un serveur distant, vous devez spécifier le nom du serveur et le port en ajoutant le paramètre -h nom du serveur ou l'adresse et -p le port.
Votre base de données bd_contmig_nat est maintenant restaurée, vous trouverez dans celle-ci différents schémas (pour regarder votre base de données, ouvrez pgAdmin et cherchez la base de données appelée bd_contmig_nat):
ref : contient toutes les tables de référence pour comprendre la codification utilisée (nous avons utilisé le standard "SANDRE" pour l'ensemble des codifications)
nat : pour les utilisateurs français, ce schéma permet une compilation de l'ensemble des schémas des utilisateurs en France. Merci de ne pas modifer ce schéma.
iav : un exemple d'un schéma utilisateur. Dans ce schéma vous trouverez toutes les données de l'Etablissement Public Territorial du Bassin de la Vilaine.
user_1 : un exemple de schéma utilisateur vide. Chaque utilisateur peut écrire dans ce schéma. Il est possible de partager les informations entre utilisateurs. Pour cela vous devez sauvegarder votre schéma (en utilisant une commande MS DOS ou l'interface graphique). Par exemple dans pgAdmin, vous pouvez faire un clic droit sur votre schéma et cliquer sur "Sauvegarder". Ensuite vous pouvez envoyer cette sauvegarde à l'utilisateur auquel vous souhaitez partager vos données. Il peut restaurer votre schéma en utilisant une commande MS DOS (psql) ou l'interface graphique (clic droit dans pgAdmin et "Restaurer")
Si vous souhaitez bancariser des données dans le schéma vide, vous voudrez sûrement modifier le nom du schéma fourni. Pour cela, vous devez choisir un nom à votre schéma (acronyme de votre organisation par exemple) et exécuter les lignes suivantes dans pgAdmin :
INSERT INTO ref.ts_organisme_org VALUES ('nom_de_votre_schéma', 'description_de_votre_organisme')
ALTER SCHEMA user_1 RENAME TO nom_de_votre_schema;
Vous devez également ajouter un nouveau rôle de connexion : dans pgAdmin, vous devez créer un rôle de connexion avec le nom de votre schéma (par exemple si vous voulez écrire dans le schéma appelé "toto" vous devez créer un rôle de connexion avec le nom "toto" et le mot de passe "toto" (où le mot de passe que vous avez choisi)). Finalement, vous devez mettre à jour les GRANT dans les tables de votre base de données pour permettre à java ou au logiciel R de se connecter à vos données. Pour cela, téléchargez le fichier Updating_grant (dans le dossier d'installation), dézipper le fichier et ouvrez une commande sql dans pgAdmin. Utilisez chercher/remplacer pour remplacer tous les user_1 dans le script par le nom que vous avez choisi pour votre schéma. Quand vous avez fait ces modifications, sélectionnez l'ensemble du script (Ctrl+a) et exécutez (Ctrl+e).
Ajouter le dossier stacomi
Vous devez télécharger le fichier stacomi.zip, dézippez le fichier et ajoutez le dossier stacomi à la racine de votre répertoire c:\Program Files. Ce dossier contient 6 fichiers  : 
base_local.xml : informations de connexion pour le programme java (pour permettre d'ajouter ou de mettre à jour votre base de données avec l'interface graphique)
calcmig.csv : informations de connexion pour le package R stacomiR (permet au package de se connecter à votre base de données). Par défaut calcmig.csv est configuré pour se connecter au schéma iav pour permettre à un nouvel utilisateur de regarder les différentes fonctionnalités du package. Une fois que le dossier stacomi est installé sur votre ordinateur vous pouvez changer la configuration du fichier calcmig.csv pour se connecter à votre schéma (remplacez iav/iav par le nom et le mot de passe de votre schéma). 
logo_stacomi.jpeg : logo pour le programme java
stacomi.bat : lance le programme java pour accéder à l'interface graphique (pour ajouter ou mettre à jour des données dans votre base de données poissons migrateurs)
stacomi.ico : icône pour le programme java
stacomi[numéro de la version].jar : programme java
Utilisez le bloc note en mode administrateur pour modifier les paramètres de connexion dans ces différents fichiers : 
base_local.xml :
<bdd bdurl="jdbc:postgresql://localhost:5432/bd_contmig_nat" user="iav" password="iav" />
avec : localhost si votre base de données est stockée en local (si la base de données est sur un serveur distant, ajoutez l'adresse du serveur); 5432 est le port de connexion (par défaut 5432, si votre base de données est sur un serveur distant, changez le par celui de votre serveur); bd_contmig_nat nom de la base de données des poissons migrateurs; user="iav" remplacez iav par le nom de votre schéma; mot de passe="iav" remplacez le par le mot de passe de connexion du rôle de votre schéma (regarder la partie Créer une base de données de ce fichier) 
calcmig.csv : remplacer le nom du lien ODBC par le nom de la source ODBC que vous avez créée, remplacez uid et pwd par le nom et le mot de passe de votre rôle de connexion, vérifiez l'hôte et le port, ajoutez le lien du répertoire CalcmigData que vous avez créé (colonne datawd du fichier calcmig.csv)
stacomi.bat : mettez à jour si nécessaire le nom du programme .jar (si vous changez la version du fichier .jar par exemple). Le nom du fichier .jar doit impérativement être identique au nom du fichier .jar de votre dossier stacomi.
Installer le package StacomiR
Dans la console R, écrivez
install.packages("stacomiR") pour la version du package sur CRAN
ou
install.packages("stacomiR", repos="https://R-forge.R-project.org") pour la version en développement (sur rforge)
Pour obtenir de l'aide sur l'utilisation du package
Une fois que l'installation est terminée et que vous avez besoin d'aide sur l'utilisation du package, vous pouvez consulter la vignette du package
library(stacomiR)
vignette("stacomir")
Si vous avez besoin d'aide, n'hésitez pas à nous contacter!
