# Installation instructions

For complete installation please be sure to have these softwares installed :
- java 8
- PostgreSQL
- R >= 3.5.0

## Create a database
Install_bd_stacomi.sql (available in the [Installation folder](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation)) allow you to create an empty database.
You will find different schema :
- ref : with all the reference tables to understand codification used (we used the french standard 'SANDRE' for all codification
- nat : for french users, this schema allow a national compilation of all users's schema in France. Please don't write inside this schema
- user_1 & user_2 : two examples of empty user schema. Each user write in his schema. It possible to share information between user. To do this you need to dump your schema (using MS DOS commande pg_dump or graphical interface. For example inside pgAdmin you can do right click on your schema and click "Backup". Then send this backup file to the user you want to share data. He can restore your schema using MS DOS command (psql) or graphical interface (inside pgAdmin3 right click and "Restore")

## Add stacomi folder


