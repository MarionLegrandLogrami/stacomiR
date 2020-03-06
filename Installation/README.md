
The stacomiR package allows you to have access to your fish migratory database and to plot different pre-programmed figures. With this package, we developed a java program to allow non-sql speaker to update or insert new data inside their postgreSQL fish migratory database.


# Table of contents
1. [Requirement ](#dependencies)

2. [Database automated installation](#auto-install)

	2.1. [On Windows](#win-install)
	
	2.2. [On Linux](#linux-install)

3. [Manual installation](#manual-install)

	3.1. [Create a database](#manual-install-db)

	3.2. [Populating it](#manual-install-db-fill)

	3.3. [Create an ODBC connector](#manual-install-odbc)

4. [Install and launch StacomiR package](#install)

5. [Final words](#final)

6. [More Help](#more-help)


# 1 Requirement <a name="dependencies"></a>

For minimal installation, the following softwares must already be installed on your computer:
- PostgreSQL ≥ 9.2 (with pgAdmin - Open Source administration and development platform for PostgreSQL)
- PostgreSQL ODBC driver (Link: [PostgreSQL website](https://www.postgresql.org/ftp/odbc/versions/) - if you need more help on how to install PostgreSQL ODBC driver please visit the section [More help](#more-help))
- R ≥ 3.5.0

You also need to download the complete 'Installation' folder available on github. To do this you can go to [DownGit](https://minhaskamal.github.io/DownGit/#/home) and enter the url of the folder (https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation), then click "download".
When the folder is downloaded, you need to dezip the folder.

This wiki will help you to prepare your environment, by:
- creating the database,
- populating it (with data provided by the établissement public territorial du bassin de la Vilaine),
- creating an ODBC link to this database.

# 2 Database automated installation <a name="auto-install"></a>
We made some scripts to help you creating the database, filling it (with the test dataset) and init the ODBC link.
They have been tested on:
- Microsoft Windows 7 pro & Windows 10
- Linux Debian 9 (should work with Ubuntu)
- Linux CentOS 7 (should work with RedHat / Fedora)


## 2.1 On Windows host <a name="win-install"></a>
Open the 'Installation' folder previously downloaded and dezipped and look for the file named:
`install_stacomi.cmd`

You have several ways to launch this file:

- double-click on `install_stacomi.cmd`: this will launch the program to create the database and to fill it. It will also create an ODBC link to this database. When double-clicking you use the default settings that are: localhost database on default port (5432) and user name=postgres and password=postgres.

- if you want to change the default settings you will have to execute the `install_stacomi.cmd` inside a console. To do this:
	- open a cmd console (Select the Start button and type cmd+enter)
	- type cd + the path to your 'Installation' folder (e.g. cd C:\Users\Public\stacomir\Installation)
	- change the options you need (to see options first start to type install_stacomi.cmd -h in your command prompt)

```
install_stacomi.cmd -h
Usage:
    install_stacomi.cmd [-h] [-u sql_user] [-p sql_pass] [-P port] [-H host]

Options:
    -h     Print this help
    -u     Define the Postgres user to use     (default = postgres)
    -p     Define the Postgres user's password (default = postgres)
    -P     Define the Postgres server port     (default = 5432)
    -H     Define the Postgres server host     (default = localhost)
```

- if you want to execute the program inside R it is possible to do this calling the function shell from the 'base' package: 

```R
#If you want to launch the program with the default settings
scriptAddr<-"path_to_install_stacomi.cmd" (e.g. "C:/Users/Public/stacomir/Installation/install_stacomi.cmd")
shell(scriptAddr,"cmd")

#If you need to change some settings, you first have to look at the options provided by the install_stacomi.cmd program. To do this type:
scriptAddr<-"path_to_install_stacomi.cmd" (e.g. "C:/Users/Public/stacomir/Installation/install_stacomi.cmd")
command<-paste(scriptAddr,"-h")
shell(command,"cmd")

#To change some options:
scriptAddr<-"path_to_install_stacomi.cmd" (e.g. "C:/Users/Public/stacomir/Installation/install_stacomi.cmd")
command<-paste(scriptAddr,"-H hostname -P port -u user -p password")
#Example with hostname=localhost (=default settings), port=6543, user=foo, password=foofoo
command<-paste(scriptAddr,"-P 6543 -u foo -p foofoo") #I don't need to add -H localhost as it is the default settings
shell(command,"cmd")
```

The SQL user (-u) must have admin rights (i.e. rights to create a database and to create a connection role). If you installed postgreSQL yourself on your computer, this is the username/password you specified during the installation.

Everything is working ? go to the [next step](#install)) !

## 2.2 On Linux host <a name="linux-install"></a>
The script is:
`install_stacomi.sh`

Calling it with `-h` will display the possible options:
```
/bin/bash install_stacomi.sh -h
Install Stacomi database on Linux
*** This script must be executed with administrative rights! ***

Usages:

  Server on same host than this script (default behavior):
  --------------------------------------------------------
  install_stacomi.sh [--sudo=account]

  Options :
    -h, --help                 This help
    -s, --sudo=account         Unix account to execute postgreSQL commands
                                 (default = postgres)



  Server on a distant host:
  -------------------------
  install_stacomi.sh --user=sqlUser --password=sqlPassword
                     --host=sqlHost --port=sqlPort

  Options :
    -h, --help                 This help
    -u, --user=sqlUser         SQL user with admin rights (default = postgres)
    -p, --password=sqlPass     User's password (default = postgres)
    -H, --host=sqlHost         PostgreSQL host (default = localhost)
    -P, --port=sqlPort         PostgreSQL port (default = 5432)

```

If you call this script with no arguments, it will assume your PostgreSQL server is on your computer. If you have a **distant** PostgreSQL server, use the `-u` `-p` `-H` and `-P` arguments to specify the connection credentials.

You **MUST** launch this script as root (directly or by sudo), else it will fail.

Everything is working ? go to the [next step](#install)) !

# 3 Manual installation<a name="manual-install"></a>

## 3.1 Create a database<a name="manual-install-db"></a>
Open pgAdmin, and in a SQL script console execute the following code:
```sql
CREATE DATABASE bd_contmig_nat WITH ENCODING = 'UTF8';
```

You then need to add different connections roles (still with pgAdmin):
- `iav` (needed by the R package to access the database):
```sql
CREATE ROLE iav LOGIN PASSWORD 'iav'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
```
- `invite`
```sql
CREATE ROLE invite LOGIN PASSWORD 'invite'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
```
- `user_1` (needed if you want to use the database for your own data)
```sql
CREATE ROLE user_1 LOGIN PASSWORD 'user_1'
  NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
```
## 3.2 Populating it<a name="manual-install-db-fill"></a>
Download `install_bd_contmig_nat.zip` (available in the [Installation folder](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation/data/install_bd_contmig_nat.zip) and **decompress the file**.
You will have a `install_bd_contmig_nat.sql` file, which contains all the data.

### 3.2.1 On Windows host
To import the data in your new database:
- be sure you have PostgreSQL executables in your `PATH` environment variable (Something like C:\Programs\PostgreSQL\<your_version>\bin)
- open a Windows command line (< Windows > key, `cmd` < Enter >),
- execute the following command:
```console
psql -U <user> bd_contmig_nat < "path_to_the_install_bd_contmig_nat.sql_file"
```
*with `user` the name you used to log in pgAdmin*

This command will only work if your database is installed on your localhost server with the default port (5432). If your database is installed on a distant server, you must specify the server hostname and port, by adding the parameters `-h server_name_or_adress` and `-p *port*`



### 3.2.2 On Linux host

 - Put the .sql file in a public folder (for example, `/tmp`)
 - Switch as the `postgres` Linux user (the one who has admin rights on your PostgreSQL server)
	 ```console
	 su - postgres
	 ```
 - Import the data on your `bd_contmig_nat` database:
	```console
	 psql bd_contmig_nat < "path_to_the_install_bd_contmig_nat.sql_file"
	```


## 3.3 Create an ODBC connector<a name="manual-install-odbc"></a>
The R package needs an ODBC connection to the database. To add it:

### 3.3.1 On Windows host
 - open the ODBC data sources program
	 - Press < Windows > key,
	 - type `odbc` (without validating by < Enter >)
	 - select the 32 or 64 version, depending of you PostgreSQL server
 - Create a new "User" datasource
	 - PostgreSQL **Unicode** (**NOT** Ansi)
		 - Data Source: `bd_contmig_nat`
		 - Database: `bd_contmig_nat`
		 - Server: your server name (`localhost` ?)
		 - Port: your server port (`5432` ?)
		 - User name: a SQL user with admin rights (`postgres` ?)
		 - Password: the SQL user password
	 - `Test` your connection
	 - When it is OK, `Save` it
	 
**⚠ If you don't have a PostgreSQL ODBC driver please use stack builder to install it first, or download it from the [PostgreSQL website](https://www.postgresql.org/ftp/odbc/versions/)** 

### 3.3.2 On Linux host
You need UnixODBC + the PostgreSQL ODBC packages. For example:
 - Debian like: `apt-get -y install unixodbc odbc-postgresql`
 - RedHat like: `yum -y install unixODBC postgresql-odbc`

PostgreSQL comes with 2 possible drivers: ANSI and Unicode. We use the Unicode one, called `psqlodbcw.so`.
The default places are:
 - Debian like: `/usr/lib/x86_64-linux-gnu/odbc/psqlodbcw.so`
 - RedHat like: `/usr/lib64/psqlodbcw.so`
To find it manually, try: `find / -iname psqlodbcw.so`

To add the ODBC link, create or edit the `/etc/odbc.ini` file, and append the following code:
```ini
[bd_contmig_nat]
Description         = PostgreSQL Unicode - StacomiR
Driver              = /path/to/your/psqlodbcw.so   <-- put your real path here
Trace               = No
TraceFile           = /tmp/psqlodbcstacomi.log
Database            = bd_contmig_nat
Servername          = localhost
UserName            = iav
Password            = iav
Port                =
ReadOnly            = Yes
RowVersioning       = No
ShowSystemTables    = No
ShowOidColumn       = No
FakeOidIndex        = No
ConnSettings        =
```
You must allow `iav` SQL user to locally connect on your server, on database `bd_contmig_nat`. This is done in the `pg_hba.conf` file of your server.

To find it, either:
 - use `find`, like this: `find / -name pg_hba.conf`
 - ask you PG server, in SQL: `SHOW hba_file;`

At the __beginning__ of the file, add:
```R
# Added for StacomiR ODBC connection
host   bd_contmig_nat  iav      127.0.0.1/32    md5
host   bd_contmig_nat  iav      ::1/128         md5
```

You're almost done. Reload your PostgreSQL server config, either:
 - asking your server in SQL: `SELECT pg_reload_conf();`
 - using Systemd: `service postgresql reload` (The service name can vary on your system...)

Just test if everything is OK:
```bash
@centos7:~# isql bd_contmig_nat -v
+---------------------------------------+
| Connected!                            |
|                                       |
| sql-statement                         |
| help [tablename]                      |
| quit                                  |
|                                       |
+---------------------------------------+
SQL> quit
root@centos7:~#
```
Congratulation!


# 4. Install and launch StacomiR package<a name="install"></a>
In a R console, execute the following command to install the package:

For the "Production" version of the package on CRAN:
```R
install.packages("stacomiR")
``` 
For the "Development" version (on R-Forge):
```R
install.packages("stacomiR", repos="https://R-forge.R-project.org")
``` 
To launch stacomiR just execute the following commands, then click on "login" (default value iav/iav). You can choose your langage using Sys.setenv() with LANG="en" for english and LANG="fr" for french (default is french so if you need french you don't have to specified the Sys.setenv command line)
```R
library(stacomiR)
Sys.setenv(LANG = "en")
stacomi()
``` 

**⚠ To get some help on how to use the package**: once everything is installed and if you need some help on how to use our package, please have a look at our vignette.
```R
library(stacomiR)
vignette("stacomir")
```

# 5. Add a stacomi folder 
You need to download stacomi.zip, unzip this file and add a stacomi folder in your program folder c:\Program Files. The folder contains 6 files  : 
- base_local.xml : Connexion informations for the java program (this will allow access to an interface allowing to load, correct, import files into the stacomi database)
- calcmig.csv : Specific informations of control for the stacomiR program (this will help you to manage the connexion to your database). By default calcmig.csv is configured to provide an access to the iav folder to allow to a new user to test the package functionalities. Once the stacomi folder has been installed on your computer, you may want to change the calcmig.csv configuration to connect to you schema (repace iav/iav by your schema name and the associated password to the user with the same name as the schema). 
The calcmig.csv is as following

| | lienODBC |	uid	| pwd | dbname | host | port | pgwd | datawd| lang|
| --------- | ------- |	----	| --- | --- | --- | ---- | ---- | ---- | --- |
| example | bd_contmig_nat | iav | iav | bd_contmig_nat | localhost | 5432 | C:/path/to/the/folder/ | French | 
| description | name of the ODBC link, you need to configure it | name of the schema, must be similar to your user name | password name (associated with username) | name of the database | host name, there you can point to an external server | port name | path to the folder where I want to store the results | Language French Spainish or English | 

- logo_stacomi.jpeg : logo pour le programme java
- stacomi.bat : lance le programme java pour accéder à l'interface graphique (pour ajouter ou mettre à jour des données dans votre base de données poissons migrateurs)
- stacomi.ico : icône pour le programme java
- stacomi[numéro de la version].jar : programme java
Utilisez le bloc note en mode administrateur pour modifier les paramètres de connexion dans ces différents fichiers : 
base_local.xml :
```
<bdd bdurl="jdbc:postgresql://localhost:5432/bd_contmig_nat" user="iav" password="iav" />
```
avec : 
`localhost` si votre base de données est stockée en local (si la base de données est sur un serveur distant, ajoutez l'adresse du serveur); 
`5432` est le port de connexion (par défaut 5432, si votre base de données est sur un serveur distant, changez le par celui de votre serveur); 
`bd_contmig_nat` nom de la base de données des poissons migrateurs; 
`user="iav"` remplacez iav par le nom de votre schéma; 
`mot de passe="iav"` remplacez le par le mot de passe de connexion du rôle de votre schéma (regarder la partie Créer une base de données de ce fichier) 
- calcmig.csv : remplacer le nom du lien ODBC par le nom de la source ODBC que vous avez créée, remplacez uid et pwd par le nom et le mot de passe de votre rôle de connexion, vérifiez l'hôte et le port, ajoutez le lien du répertoire CalcmigData que vous avez créé (colonne datawd du fichier calcmig.csv)
- stacomi.bat : mettez à jour si nécessaire le nom du programme .jar (si vous changez la version du fichier .jar par exemple). Le nom du fichier .jar doit impérativement être identique au nom du fichier .jar de votre dossier stacomi.


# 6. Final words<a name="final"></a>
You are now ready to use the R package. If you want to go further, and use it for your own structure, you will find some help on it's usage on the [How To page](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/HOWTO.md).

**If you need some help, don't hesitate to contact us !** 

# 7. More help<a name="more-help"></a>
## 7.1 Step-by-Step to install a PostgreSQL ODBC Driver
1. Choose the good ODBC Driver version

If you are a windows user you need to download one of the files of the [msi folder](https://www.postgresql.org/ftp/odbc/versions/msi/). Unless you have a specific reason not to do so, you should always download the latest version. Before downloading the file you need to choose the file corresponding to your windows system (32 or 64 bits).

2. Install the ODBC Driver

After having downloaded the good ODBC Driver version, you need to dezip the folder and launch the installer (double-click on the .msi file). Follow the instructions of the installer.
