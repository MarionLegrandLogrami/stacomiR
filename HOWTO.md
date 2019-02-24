**⚠ If you just want to TRY the stacomiR package you don't need to follow all the steps above ⚠**, as inside the stacomiR package you will have everything you need. This wiki is dedicated to people wanting to use the stacomiR package on their own data (with a specific postgresql schema).


# How to use it for your own structure
We assume that you have a valid `bd_contmig_nat` database on your PostgreSQL server. If not, please read our [Installation](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation) section before!

## Understanding

On your `bd_contmig_nat` database, you will find different schemas:
*(to look at your database, open pgAdmin and search for a database called bd_contmig_nat)*
- `ref`: contains all the reference tables, to understand codifications used (we used the french standard 'SANDRE' for all our codifications)
- `nat`: [for French users] this schema is a national compilation of all users' schemas in France. ⚠ Please **do not** write in it!
- `iav`: an example of a real user schema. In it, you will find all data of the Établissement Public Territorial du Bassin de la Vilaine.
- `user_1`: an example of empty user's schema.

Each user writes in his own schema. But it is possible to share information between users.
To do that, you need to:
 - dump your schema (using `pg_dump`command line or graphical interface), which will generate a backup of all your data in a big SQL file
  	 - (For example, inside pgAdmin,  right click on your schema and select "Backup")
 - send it to another user
 - The other user could restore it on his own PostgreSQL server (using `psql`command line or graphical interface)
	 - (inside pgAdmin right click and "Restore")


## Personalize user schema (change name, etc.)

If you want to add data in the empty user schema provided, you will probably want to change its name. To do so, you just need to choose a name for your schema (short name of your organization for example) and execute these lines in your favorite SQL client:

```sql
INSERT INTO ref.ts_organisme_org VALUES
    ('name_of_your_schema', 'description_of_your_organization')

ALTER SCHEMA user_1 RENAME TO name_of_your_schema;
```
You then need to add new connection role, to allow the R package to access your data.
🡆 In pgAdmin, you need to create a connection role with the same name as your schema (If your schema is called `foo`, you will need a connection role also called `foo` -- but with the password of your choice -- ).

Finally, you need to update access rights (GRANT) in the different tables of your database to allow Java or R program to connect to your data. to do so, download the Updating_grant file (in the installation folder), unzip the file and open the sql script in your pgAdmin. Using search/replace update all the user_1 in the script by the name you choose for your schema. When you have made all the modifications select all the script and execute (Ctrl+e).

## Add stacomi folder

You need to [download the stacomi.zip file, unzip the file and put the **stacomi folder**](https://github.com/MarionLegrandLogrami/stacomiR/tree/master/Installation) at the root of your **c:\Program Files** repository.
You will find 6 files inside the stacomi folder:
- `base_local.xml`: connection information for the Java program (a graphical interface to add / update data)
- `calcmig.csv`: connection information for the R package stacomiR (allow the package to connect to your database). By default calcmig.csv is **configured to log on the iav schema** to allow new user to look at the functionalities of the package. Once the stacomi folder is installed on your computer, you can change the configuration of calcmig.csv to log on your schema (just replace `iav/iav` by your schema's name and password).
- `logo_stacomi.jpeg`: Logo for the Java program
- `stacomi.bat`: launcher of the Java program, a graphical interface to update or insert new data in your fish migratory database
- `stacomi.ico`: icon for the Java program
- `stacomi<some_version>.jar`: Java program

⚠ To edit them, use your favorite text editor, but **in Administrator mode**! (as they are in your "Program Files" folder, which is protected).

You need to customize your connection settings in these different files:

**`base_local.xml`**
```xml
<bdd bdurl="jdbc:postgresql://localhost:5432/bd_contmig_nat" user="iav" password="iav" />
```
notes: 
- `localhost` if you have local database (if the database is on a server put the server adress) ; 
- `5432` is the port of connection (by default 5432, if on a server you will need to change the port to the one of your server); 
- `bd_contmig_nat` name of the fish migratory database; 
- `user="iav"` replace "iav" by the name of your database schema; 
- `password="iav"` replace with the password you choose for the connection role of your schema (see the **Create database** part of this file).

**`calcmig.csv`**
Replace the `lienODBC` name by the name of the ODBC source you created, replace `uid` and `pwd` by the name of your schema and password of your connection role, verify `host` and `port`, add the link to the CalcmigData folder you created (`datawd` column of the calcmig.csv file)

**`stacomi.bat`**
Update if necessary the name of the .jar program (if you change the version of the .jar file for example). The name of the .jar file must be exactly the same as the name of the .jar file in your stacomi folder.

## Install StacomiR package
In a R console, execute the following command:

For the "Production" version of the package on CRAN:
```R
install.packages("stacomiR")
``` 
For the "Development" version (on R-Forge):
```R
install.packages("stacomiR", repos="https://R-forge.R-project.org")
``` 

## To get some help on how to use the package
Once everything is installed and if you need some help on how to use our package, please have a look at our vignette.
```R
library(stacomiR)
vignette("stacomir")
```

**If you need some help, don't hesitate to contact us !** The stacomiR package allows you to have access to your fish migratory database and to plot different pre-programmed figures. With this package, we developed a Java program to allow non-sql speakers to update or insert new data inside their own postgreSQL fish migratory database.

