The stacomiR package allows you to have access to your fish migratory database and to plot different pre-programmed figures. With this package, we developped a java program to allow non-sql speaker to update or insert new data inside their postgreSQL fish migratory database.

# Installation instructions

For complete installation please be sure to have these softwares installed :
- java 8
- PostgreSQL (with pgAdmin - Open Source administration and development platform for PostgreSQL)
- R >= 3.5.0

You need also to:
- create ODBC link for your fish migratory database (add a source of type "PostgreSQL unicode")
- add a **CalcmigData** folder on your computer (for example c:\users\my_session\Documents\CalcmigData)

## Create a database
Install_bd_stacomi.sql (available in the [Installation folder](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation)) allows you to create an empty database. **You first need to decompress the file Install_bd_stacomi.zip**.
Before restauring this file you will need to add different connections roles inside pgAdmin:
- for iav
```
CREATE ROLE iav LOGIN PASSWORD 'iav'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
```
- for invite
```
CREATE ROLE invite LOGIN PASSWORD 'invite'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
```
- for user_1
```
CREATE ROLE user_1 LOGIN PASSWORD 'user_1'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
```
- for user_2
```
CREATE ROLE user_2 LOGIN PASSWORD 'user_2'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
```
You now need to restaure Install_bd_stacomi.sql. To do that open an MS DOS command and write (you first need to save the path to the Program\PostgreSQL\your_version\bin in Environment Variables):
```
psql -U user <"path_to_the_Install_bd_stacomi.sql_file"
```
*with user the name you use to log in pgAdmin*
This command line works only if your database is installed on your localhost server with the default port (5432). If your database is installed on a distant server you need to specify the server name and the port adding the parameter -h *server name or adress* and -p *port*

Now that your bd_contmig_nat database is restaured, you will find in your database different schemas (to look at your database, open pgAdmin and search for a database called bd_contmig_nat):
- ref : with all the reference tables to understand codification used (we used the french standard 'SANDRE' for all codifications)
- nat : for french users, this schema allow a national compilation of all users's schema in France. Please don't write inside this schema
- iav : an example of a real user schema. In this schema you will find all data of the Etablissement Public Territorial du Bassin de la Vilaine.
- user_1 & user_2 : two examples of empty user's schemas. Each user write in his schema. It is possible to share information between user. To do this you need to dump your schema (using MS DOS commande pg_dump or graphical interface). For example inside pgAdmin you can do right click on your schema and click "Backup". Then send this backup file to the user with whom you want to share data. He can restore your schema using MS DOS command (psql) or graphical interface (inside pgAdmin right click and "Restore")

If you want to add data in one of this empty user schema you will probably want to change the name of one of the empty schema provided. To do so, you just need to choose a name for your schema (short name of your organization for example) and execute these lines :

```
INSERT INTO ref.ts_organisme_org VALUES
    ('name_of_your_schema', 'description_of_your_organization')

ALTER SCHEMA user_1 RENAME TO name_of_your_schema;
```
You also need to add new connection role:
In pgAdmin, you need to create a connexion role with the same name as your schema (for example if I write inside schema called 'toto' I need to create a connexion role with name 'toto' and password 'toto' (or wathever password you choose)).


## Add stacomi folder
You need to [download and put the **stacomi folder**](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation) at the root of your **c:\Program Files** repository.
You will find 6 files inside the stacomi folder:
- base_local.xml: Informations of connection for the java program (to be able to add new data or update your database with graphical interface)
- calcmig.csv: informations of connection for the R package stacomiR (allow the package to connect to your database). By default calcmig.csv is **configured to log on the iav schema** to allow new user to look at the functionnalities of the package. Once the stacomi folder has been installed on your computer you can change the configuration of calcmig.csv to log on your schema (just replace iav/iav by your schema's name and passeword).
- logo_stacomi.jpeg: Logo for the java program
- stacomi.bat: launch the java program to access to the graphical interface (to update or insert new data in your fish migratory database)
- stacomi.ico: icon for the java program
- stacomi[some version].jar: java program

Using pad in administrator mode you need to customize your connection settings in these different files:
- base_local.xml: 
```
<bdd bdurl="jdbc:postgresql://localhost:5432/bd_contmig_nat" user="user_1" password="user_1" />
```
with: 
`localhost` if you have local database (if the database is on a server put the server adress); 
`5432` is the port of connexion (by default 5432, if on a server you will need to change the port to the one of your server); 
`bd_contmig_nat` name of the fish migratory database; 
`user="user_1"` replace "user_1" by the name of your database schema; 
`password="user_1"` replace with the password you choose for the connexion role of your schema (see the **Create database** part of this file).
- calcmig.csv: replace the `lienODBC` name by the name of the ODBC source you created, replace `uid` and `pwd` by the name of your schema and password of your connexion role, verify `host` and `port`, add the link to the CalcmigData folder you created (`datawd` column of the calcmig.csv file)
- stacomi.bat: update if necessary the name of the .jar program (if you change the version of the .jar file for example). The name of the .jar file must be exactly the same as the name of the .jar file in your stacomi folder.

## install StacomiR package
In a R console write :
- ```install.packages("stacomiR")``` for the version of the package on CRAN
- ```install.packages("stacomiR", repos="https://R-forge.R-project.org")``` for the development version (on rforge)

**If you need some help, don't hesitate to contact us !**
