The stacomiR package allows you to have access to your fish migratory database and to plot different pre-programmed figures. With this package, we developed a java program to allow non-sql speaker to update or insert new data inside their postgreSQL fish migratory database.


# Table of contents
1. [Requirement ](#dependencies)
2. [Database automated installation](#auto-install)
2.1. [On Linux](#linux-install)
2.2. [On Windows](#win-install)
3. [Manual installation](#manual-install)
3.1. [Create a database](#manual-install-db)
3.2. [Populating it](#manual-install-db-fill)
3.3. [Create an ODBC connector](#manual-install-odbc)
4. [Final words](#final)


# 1 Requirement <a name="dependencies"></a>

For minimal installation, the following softwares must already be installed on your computer:
- PostgreSQL ≥ 9.5 (with pgAdmin - Open Source administration and development platform for PostgreSQL)
- PostgreSQL ODBC driver (Link: [PostgreSQL website](https://www.postgresql.org/ftp/odbc/versions/))
- R ≥ 3.5.0

For complete installation, you  also need:
- java ≥ 8

This wiki will help you to prepare your environment, by:
- creating the test database,
- populating it,
- create an ODBC link to this database.

############## Jérémie : à virer ? ################
You will also need to:
- create an ODBC link for your fish migratory database (add a source of type "PostgreSQL unicode". If you don't have a PostgreSQL ODBC driver please use stack builder to install it first, or download it from the [PostgreSQL website](https://www.postgresql.org/ftp/odbc/versions/))
- [optional - if you want to choose where the folder must be created] add a **CalcmigData** folder on your computer (for example `c:\users\<my_session>\Documents\CalcmigData` on Windows, or `~/CalcmigData` on Linux)

############## fin ################

# 2 Database automated installation <a name="auto-install"></a>
We made some scripts to help you creating the database, filling it and init the ODBC link.
They have been tested on:
- Microsoft Windows 10
- Linux Debian 9 (should work with Ubuntu)
- Linux CentOS 7 (should work with RedHat)


## 2.1 On Linux host <a name="linux-install"></a>
The script is:
`install_stacomi.sh`

Calling it with `-h` will display the possible options:
```
/bin/batch install_stacomi.sh -h
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

If you call this script with no arguments, it will assume your PostgreSQL server is on the same computer than you. If you have a **distant** PostgreSQL server, use the `-u` `-p` `-H` and `-P` arguments to specify the connection credentials.

You **MUST** launch this script as root (directly or by sudo), else it will fail.


## 2.2 On Windows host <a name="win-install"></a>
The script is:
`install_stacomi.cmd`

Calling it with `-h` will display the possible options:
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

You *WILL* have to give a SQL user with admin rights (create a database, create a connection role). Else the install will fail.

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
Download `Install_bd_stacomi.zip` (available in the [Installation folder](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation) and **decompress the file**.
You will have a `Install_bd_stacomi.sql` file, which contains all the data.

### 3.2.1 On Windows host
To import the data in your new database:
- be sure you have PostgreSQL executables in your `PATH` environment variable (Something like C:\Programs\PostgreSQL\<your_version>\bin)
- open a Windows command line (< Windows > key, `cmd` < Enter >),
- execute the following command:
```console
psql -U <user> bd_contmig_nat < "path_to_the_Install_bd_stacomi.sql_file"
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
	 psql bd_contmig_nat < "path_to_the_Install_bd_stacomi.sql_file"
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

### 3.3.2 On Linux host
There should be a PostgreSQL ODBC package in your favorite distribution. The only thing to do is to install it.
For example:
 - Debian like: `apt-get install odbc-postgresql`
 - RedHat like: `yum install postgresql-odbc`


# 4. Final words<a name="final"></a>
You are now ready to use the R package. If you want to go further, and use it for your own structure, you will find some help on it's usage on the [How To page](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/HOWTO.md).
