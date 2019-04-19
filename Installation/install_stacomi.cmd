@echo off
SETLOCAL EnableDelayedExpansion

:: Install script for Stacomi database on Windows
:: (c)2019 LOGRAMI      - Marion LEGRAND
:: (c)2019 EPTB Vilaine - Cédric BRIAND
:: 
:: Return codes:
::	  0	  no error
::    1   Missing dependency
::    2   Error while processing
::	255	  Display usage
::
:: unzip software comes from GnuWin32:
::    http://gnuwin32.sourceforge.net/

:: Default variables
set sql_user=postgres
set sql_pass=postgres
set sql_host=localhost
set sql_port=5432
set prog_psql=psql

:: ------------- Do not change anything above this line -------------
set prog_unzip=%~dp0%bin\unzip.exe
set sql_data_zip=%~dp0%data\install_bd_contmig_nat.zip
set sql_data_unzip=%~dp0%data\install_bd_contmig_nat.sql
set sql_rep_unzip=%~dp0%data\
set db_name=bd_contmig_nat
set odbc_driver_name_64=PostgreSQL Unicode(x64)
set odbc_driver_name_32=PostgreSQL Unicode
set ret=0


:: Parsing arguments
:: -----------------
:GetOpts
if "%1" == "-h" goto Usage
if "%1" == "-u" set sql_user=%2 & shift
if "%1" == "-p" set sql_pass=%2 & shift
if "%1" == "-P" set sql_port=%2 & shift
if "%1" == "-H" set sql_host=%2 & shift
shift
if not "%1" == "" goto GetOpts
goto CheckDependencies


:: Display usage
:: -------------
:Usage
echo Usage:
echo     %0 [-h] [-u sql_user] [-p sql_pass] [-P port] [-H host]
echo.
echo Options:
echo     -h     Print this help
echo     -u     Define the Postgres user to use     (default = %sql_user%)
echo     -p     Define the Postgres user's password (default = %sql_user%)
echo     -P     Define the Postgres server port     (default = %sql_port%)
echo     -H     Define the Postgres server host     (default = %sql_host%)
echo.
set ret=255 && goto End


:: Check dependencies
:: ------------------
:CheckDependencies
echo Checking dependencies
:: Is there a pSQL?
where /Q %prog_psql%
if "%ERRORLEVEL%" neq "0" (
	echo   * Oops: psql.exe not found in your PATH environment variable... we will need it.
	echo     Please give the full path of the executable.
	echo      --^> It should be in a folder like: C:\Programs\PostgreSQL\^<version^>\bin
	set /P prog_psql=Full path? 
	where /Q !prog_psql!
	if "!ERRORLEVEL!" neq "0" (
		echo This is not a valid path. Sorry.
		set ret=1 && goto End
	)
)
echo   * pSQL executable: OK
:: Is there unzip?
if not exist !prog_unzip! (
	echo   * Oops: unzip.exe not found in 'bin' folder...
	echo        Did you really get ALL the install files GitHub?
	set ret=1 && goto End
)
echo   * unzip.exe: OK
:: Is there the SQL dump?
if not exist !sql_data_zip! (
	echo   * Oops: database archive not found in 'data' folder...
	echo        ^(Looking for !sql_data_zip!^)
	echo        Did you really get ALL the install files GitHub?
	set ret=1 && goto End
)
echo   * Database archive: OK
:: Can we connect to PostgreSQL server?
set PGPASSWORD=%sql_pass%&& !prog_psql! -U %sql_user% -h %sql_host% -p %sql_port% -c "SELECT 1" >nul
if "%ERRORLEVEL%" neq "0" (
	echo   * Oops: can't connect to PostgreSQL server. Please check your credentials,
	echo        and be sure the server is running
	set ret=1 && goto End
)
echo   * Database connection: OK

:: DEBUG ONLY
:: echo prog_psql = !prog_psql!
:: echo prog_unzip = !prog_unzip!
:: echo host = !sql_host!
:: echo port = !sql_port!
:: echo user = !sql_user!
:: echo pass = !sql_pass!


:: Here we go
:: ----------
:Process
echo Processing
echo     * Extracting database archive
::%prog_unzip% -o -q %sql_data_zip% -d %~dp0%data
%prog_unzip% -o -q %sql_data_zip% -d %sql_rep_unzip%
if "%ERRORLEVEL%" neq "0" (
	echo         * Error while extracting. Operation aborted
	set ret=2 && goto End
)
echo     * Creating database '%db_name%'
set PGPASSWORD=%sql_pass%&& !prog_psql! -U %sql_user% -h %sql_host% -p %sql_port% -c "CREATE DATABASE %db_name% WITH ENCODING = 'UTF8';" >nul
if "%ERRORLEVEL%" neq "0" (
	echo         * Error while creating database.
	echo           If it already exists, you will have to drop it manually, and launch this script again later.
	echo           --^> I won't drop this existing database, to avoid all risks of data loss.
	set ret=2 && goto End
)
echo     * Creating connection role 'nat'
set PGPASSWORD=%sql_pass%&& !prog_psql! -U %sql_user% -h %sql_host% -p %sql_port% -c "CREATE ROLE nat LOGIN PASSWORD 'nat' NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;" >nul

echo     * Creating connection role 'iav'
set PGPASSWORD=%sql_pass%&& !prog_psql! -U %sql_user% -h %sql_host% -p %sql_port% -c "CREATE ROLE iav LOGIN PASSWORD 'iav' NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;" >nul

echo     * Creating connection role 'invite'
set PGPASSWORD=%sql_pass%&& !prog_psql! -U %sql_user% -h %sql_host% -p %sql_port% -c "CREATE ROLE invite LOGIN PASSWORD 'invite' NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;" >nul

echo     * Creating connection role 'user_1'
set PGPASSWORD=%sql_pass%&& !prog_psql! -U %sql_user% -h %sql_host% -p %sql_port% -c "CREATE ROLE user_1 LOGIN PASSWORD 'user_1' NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;" >nul

echo     * Filling database '%db_name%' ^(please be patient, it will take a while^)
set PGPASSWORD=%sql_pass%&& !prog_psql! -U %sql_user% -h %sql_host% -p %sql_port% -f %sql_data_unzip% %db_name%
if "%ERRORLEVEL%" neq "0" (
	echo         * Error while extracting. Operation aborted
	set ret=2 && goto End
)


:: ODBC link
::----------
echo     * Creating ODBC link '%db_name%'
:: Is there something in the 64 bits app folder?
::  If yes, we assume we can use the 64 bits driver
set odbc_driver=%odbc_driver_name_32%
if exist "%ProgramFiles%\psqlODBC" (
	echo         * Assuming we have a 64 bits driver
	echo           ^(I found a "%ProgramFiles%\psqlODBC" folder^)
	set odbc_driver=!odbc_driver_name_64!
) else (
	echo         * Assuming we have a 32 bits driver
	echo           ^(I did not find any "psqlODBC" folder in "%ProgramFiles%"^)
)
odbcconf /A {ConfigDSN "!odbc_driver!" "DSN=%db_name%|UID=%sql_user%|PWD=%sql_pass%|SERVER=%sql_host%|PORT=%sql_port%|DATABASE=%db_name%|DESCRIPTION=ODBC link created for stacomiR project"}


:: Bye bye
:: -------
:End

REM if exist %~dp0%data\install_bd_contmig_nat.sql (
	REM echo     * Removing extracted archive
	REM del %~dp0%data\install_bd_contmig_nat.sql
REM )
if "%ret%" equ "0" (
	echo * Finished
)
pause
exit /B %ret%
