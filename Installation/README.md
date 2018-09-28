The stacomiR package allow you to have access to your fish migratory database and to plot different pre-programmed figures. With this package, we developped a java program to allow non-sql speaker to update or insert new data inside the postgreSQL fish migratory database.

# Installation instructions

For complete installation please be sure to have these softwares installed :
- java 8
- PostgreSQL
- R >= 3.5.0
You need also to create:
- ODBC link for your fish migratory database (add a source of type "PostgreSQL ANSI")
- add a **CalcmigData** folder on your computer (for example c:\users\my_session\Documents\CalcmigData)

## Create a database
Install_bd_stacomi.sql (available in the [Installation folder](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation)) allow you to create an empty database.
You will find different schemas:
- ref : with all the reference tables to understand codification used (we used the french standard 'SANDRE' for all codifications)
- nat : for french users, this schema allow a national compilation of all users's schema in France. Please don't write inside this schema
- user_1 & user_2 : two examples of empty user's schemas. Each user write in his schema. It is possible to share information between user. To do this you need to dump your schema (using MS DOS commande pg_dump or graphical interface. For example inside pgAdmin you can do right click on your schema and click "Backup". Then send this backup file to the user with whom you want to share data. He can restore your schema using MS DOS command (psql) or graphical interface (inside pgAdmin3 right click and "Restore")

You need to create a connexion role with the same name as your database schema (for example if I write inside schema called 'toto' I need to create a connexion role with name 'toto' and password 'toto' (or wathever password you choose)

## Add stacomi folder
You need to [download and put the **stacomi folder**](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation) at the root of your **c:\Program Files** repository.
You will find 6 files inside the stacomi folder:
- base_local.xml: Informations of connection for the java program (to be able to add new data or update your database with graphical interface)
- calcmig.csv: informations of connection for the R package stacomiR (allow the package to connect to your database)
- logo_stacomi.jpeg: Logo for the java program
- stacomi.bat: launch the java program to access to the graphical interface (to update or insert new data in your fish migratory database)
- stacomi.ico: icon for the java program
- stacomi[some version].jar: java program

Using pad in administrator mode you need to customize your connection settings in these different files:
- base_local.xml: 
```
<bdd bdurl="jdbc:postgresql://localhost:5432/bd_contmig_nat" user="user_1" password="user_1" />
```
`localhost` if you have local database (if the database is on a server put the server adress
`5432` is the port of connexion (by default 5432, if on a server you will need to change the port to the one of your server)
`bd_contmig_nat` name of the fish migratory database
`user="user_1"` replace "user_1" by the name of your database schema
`password="user_1"` replace with the password you choose for the connexion role of your schema (see the **Create database** part of this file)
- calcmig.csv: replace the `lienODBC` name by the name of the ODBC source you created, replace `uid` and `pwd` by the name of your schema and password of your connexion role, verify `host` and `port`, add the link to the CalcmigData folder you created (`datawd` column of the calcmig.csv file)
- stacomi.bat: update if necessary the name of the .jar program (if you change the version of the .jar file for example. The name of the .jar file must be exactly the same as the name of the .jar file in your stacomi folder)

## install StacomiR package
In a R consol write :
```install.packages("stacomiR")``` for the version of the package on CRAN
```install.packages("stacomiR", repos="https://R-forge.R-project.org")```

**If you need some help, don't hesitate to contact us !**
