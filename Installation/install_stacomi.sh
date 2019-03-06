#!/bin/bash
#
# Install script for Stacomi database on Linux
# (c)2019 LOGRAMI      - Marion LEGRAND
# (c)2019 EPTB Vilaine - Cédric BRILLANT
# 
# Return codes:
#	 0   no error
#    1   Missing dependency
#    2   Error while processing
#  255   Display usage
#

# ------------- Do not change anything above this line -------------

# Defaults files
_ROOT=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)
ZIP_ARCH_NAME=install_bd_contmig_nat
ZIP_ARCH=data/${ZIP_ARCH_NAME}.zip
ODBC_LINK=data/odbc.ini

# Default variables
DB_NAME=bd_contmig_nat
SQL_SUDO=postgres
SQL_USER=postgres
SQL_PASS=postgres
SQL_IAV_USER=iav
SQL_IAV_PASS=iav
SQL_HOST=localhost
SQL_PORT=5432
REMOTE=0

# Return codes
RET_OK=0
RET_MISS=1
RET_ERR=2
RET_USAGE=255
RET=$RET_OK

# Colors
WHITE="\E[0m"
BOLD="\E[1m"
RED="$WHITE\E[31m"
GREEN="$WHITE\E[32m"
ORANGE="$WHITE\E[33m"
BLUE="$WHITE\E[36m"

# Executables
PROG_UNZIP=unzip
PROG_PSQL=psql


# Supported OS
#-------------
OS_UPD=0
OS=
PACKAGE_MANAGER=
[ -f /etc/redhat-release ] && OS=redhat && PACKAGE_MANAGER="yum"
[ -f /etc/debian_version ] && OS=debian && PACKAGE_MANAGER="apt-get"


#---------------------------------------------------------
# Test dependency
#	$1	program to test (empty = search for installed package name)
#	$1	Description
#	$2	Debian package name (regexp)
#	$3	RedHat package name (regexp)
#---------------------------------------------------------
test_dependency() {
	local prog=$1
	local descr=$2
	local package_debian=$3
	local package_redhat=$4
	echo -en "    [${BLUE}*${WHITE}] $descr"
	
	# What is the package name for the distribution?
	local package=$(eval echo \$$"package_$OS")
	
	# Program given? We test if we find it in the PATH
	local found=
	if [ -n "$prog" ]; then
		found=$(which $prog)
	# If not, we check if the corresponding package is installed
	# (/!\ It could have been compiled by hand, and so not being detected here...)
	else
		if [ "$OS" = "debian" ]; then
			found=$(dpkg -l | grep -E "^ii\s+$package\s" 2>/dev/null | awk '{print $2}')
		elif [ "$OS" = "redhat" ]; then
			found=$(rpm -qa | grep -E "^${package}-[0-9]" 2>/dev/null)
		fi
	fi
	# found?
	if [ -n "$found" ]; then
		echo -e "[${GREEN}found${WHITE}] ($found)"
	# Not found?
	else
		echo -e "[${RED}not found${WHITE}]"
		# If we don't know this OS, we can't do anything
		if [ -z "$OS" ]; then
			echo "        --> Please install it manually (I can't do it by myself, I don't"
			echo "            know how to do it with your Linux distrib, sorry...)"
			exit $RET_MISS
		fi
		# If it is ok, we propose to install it automatically
		echo "          I can install it automatically for you with $PACKAGE_MANAGER"
		local choice=
		read -p "          Install this dependency? (Y/n): " choice
		local install=0
		[ -z "$choice" -o "$choice" = "y" -o "$choice" = "Y" ] && install=1
		if [ $install -eq 0 ]; then
			echo -e "\n--> OK. So please install ${BOLD}$package${WHITE}, and come back later :-)"
			exit $RET_MISS
		fi
		# Install automatically
		package=$(echo "$package" | sed -r 's~\.\*~~')
		if [ "$OS" = "debian" ]; then
			# The 1st time, update packages list
			if [ $OS_UPD -eq 0 ]; then
				echo -e "              [${BLUE}*${WHITE}] Doing 'apt-get update'"
				apt-get update 1>/dev/null
				OS_UPD=1
			fi
			echo -e "              [${BLUE}*${WHITE}] Doing 'apt-get -y install $package'"
			apt-get -y install $package 1>/dev/null
			result=$?
		elif [ "$OS" = "redhat" ]; then
			# The 1st time, update packages list
			#if [ $OS_UPD -eq 0 ]; then
			#	echo -e "              [${BLUE}*${WHITE}] Doing 'yum upgrade'"
			#	yum upgrade 1>/dev/null
			#	OS_UPD=1
			#fi
			echo -e "              [${BLUE}*${WHITE}] Doing 'yum -y install $package'"
			yum -y install $package 1>/dev/null
			result=$?
		fi
		if [ $result -eq 0 ]; then
			echo -e "              [${GREEN}*${WHITE}] Done"
		fi
	fi
}


#---------------------------------------------------------
# Display program usage
#	$1 : If exists, error message to display before
#---------------------------------------------------------
usage() {
	if [ -n "$1" ]; then
		echo -e "Error: ${RED}$1${WHITE}\n"
	fi
	echo -e "Install script for Stacomi database on Linux
${BOLD}*** This script must be executed with administrative rights! ***${WHITE}

Usages:
${BLUE}
  Server on same host than this script (default behavior):
  --------------------------------------------------------${WHITE}
  $0 [${GREEN}--sudo=account${WHITE}]
  
  ${ORANGE}Options :
    ${GREEN}-h, --help                 ${WHITE}This help
    ${GREEN}-s, --sudo=account         ${WHITE}Unix account to execute postgreSQL commands
                                 (default = ${BLUE}$SQL_SUDO${WHITE})

	
${BLUE}  
  Server on a distant host:
  -------------------------${WHITE}
  $0 ${GREEN}--user=sqlUser${WHITE} ${GREEN}--password=sqlPassword${WHITE}
                     [${GREEN}--host=sqlHost${WHITE}] [${GREEN}--port=sqlPort${WHITE}]

  ${ORANGE}Options :
    ${GREEN}-h, --help                 ${WHITE}This help
    ${GREEN}-u, --user=sqlUser         ${WHITE}SQL user with admin rights (default = ${BLUE}$SQL_USER${WHITE})
    ${GREEN}-p, --password=sqlPass     ${WHITE}User's password (default = ${BLUE}$SQL_PASS${WHITE})
    ${GREEN}-H, --host=sqlHost         ${WHITE}PostgreSQL host (default = ${BLUE}$SQL_HOST${WHITE})
    ${GREEN}-P, --port=sqlPort         ${WHITE}PostgreSQL port (default = ${BLUE}$SQL_PORT${WHITE})
"
	exit $RET_USAGE
}

# Parsing arguments
#------------------
OPTS=$(getopt -o hu:p:H:P: --long help,user:,password:host:,port: -- "$@" 2>/dev/null)
eval set -- "$OPTS"
while [ $# -gt 0 ]; do
	case $1 in
		-h|--help)		usage;			shift 	;;
		-u|--user)		SQL_USER=$2;	REMOTE=1;	shift 2	;;
		-p|--password)	SQL_PASS=$2;	REMOTE=1;	shift 2	;;
		-H|--host)		SQL_HOST=$2;	REMOTE=1;	shift 2	;;
		-P|--port)		SQL_PORT=$2;	REMOTE=1;	shift 2	;;
		-s|--sudo)		SQL_SUDO=$2;				shift 2	;;
		--) shift; break ;;	
		*)	usage "unknown argument: $1"	;;
	esac
done
# Arguments validity
if [ $REMOTE -eq 1 ]; then
	echo -e "[${GREEN}*${WHITE}] Arguments for remote SQL server given."
	echo -e "    [${GREEN}*${WHITE}] I assume that I must connect to a remote host (not using sockets)"
fi

# Are we root?
[ -z "$(whoami | grep '^root$')" ] && usage "This script must be executed as root. Please try:\n${BOLD}       su - root $_ROOT/$0${WHITE}"


# Check dependencies
#-------------------
echo -e "[${BLUE}*${WHITE}] Checking dependencies:"
# Supported OS?
echo -en "    [${BLUE}*${WHITE}] Supported Linux distrib   "
if [ -n "$OS" ]; then
	echo -e "[${GREEN}found${WHITE}] ($OS mode)"
else
	echo -e "[${ORANGE}unknown${WHITE}]"
	echo "        --> We haven't tested on your Linux distribution. It could work out of the box,"
	echo "            or not. If dependencies are missing, you will have to install them manually."
	echo
fi
# Testing needed packages / programs
#				prod	descr						 debian-name			redhat-name
test_dependency ""		"UnixODBC...               " 'unixodbc'				'unixODBC'
test_dependency ""		"PostgreSQL ODBC...        " 'odbc-postgresql'		'postgresql.*-odbc'
test_dependency "psql"	"pSQL client...            " 'postgresql-client' 	'postgresql'
test_dependency "unzip"	"Unzip program...          " 'unzip'				'unzip'

# Is there the SQL dump?
echo -en "    [${BLUE}*${WHITE}] SQL dump...               "
test -f $_ROOT/$ZIP_ARCH
if [ $? -eq 0 ]; then
	echo -e "[${GREEN}found${WHITE}] ($ZIP_ARCH)"
else
	echo -e "[${RED}not found${WHITE}]"
	echo -e "\n--> I was looking for $ZIP_ARCH"
	echo "     Did you really get ALL the install files GitHub?"
	exit $RET_MISS
fi
# Is there the ODBC link?
echo -en "    [${BLUE}*${WHITE}] ODBC link...              "
test -f $_ROOT/$ODBC_LINK
if [ $? -eq 0 ]; then
	echo -e "[${GREEN}found${WHITE}] ($ODBC_LINK)"
else
	echo -e "[${RED}not found${WHITE}]"
	echo -e "\n--> I was looking for $ODBC_LINK"
	echo "     Did you really get ALL the install files GitHub?"
	exit $RET_MISS
fi

# pSQL command detection
if [ $REMOTE -eq 0 ]; then
	cd /tmp
	PSQL="sudo -u $SQL_SUDO $PROG_PSQL"
else
	export PGPASSWORD="$SQL_PASS"
	PSQL="$PROG_PSQL -U $SQL_USER -h $SQL_HOST -p $SQL_PORT"
fi
# Can we connect to PostgreSQL server?
echo -en "    [${BLUE}*${WHITE}] PSQL server connection... "
$PSQL -c "SELECT 1" 1> /dev/null
if [ $? -eq 0 ]; then
	echo -e "[${GREEN}found${WHITE}]"
else
	echo -e "[${RED}error${WHITE}]"
	echo -e "\n--> Please check your credentials, and be sure the server is running"
	exit $RET_MISS
fi
# Can we find pg_hba.conf?
echo -en "    [${BLUE}*${WHITE}] Asking for pg_hba.conf... "
PG_HBA=$($PSQL -Atc "SHOW hba_file;")
if [ -z "$PG_HBA" ]; then
	PG_HBA=$(find / -name pg_hba.conf 2>/dev/null | head -n1)
fi
if [ -n "$PG_HBA" ]; then
	echo -e "[${GREEN}found${WHITE}] ($PG_HBA)"
else
	echo -e "[${RED}error${WHITE}]"
	echo -e "\n--> Can't find pg_hba.conf file"
	exit $RET_MISS
fi
# Can we find the Postgres Unicode ODBC driver? (psqlodbcw.so)
# (we test on all the known .so places on the system)
echo -en "    [${BLUE}*${WHITE}] Detecting psqlodbcw.so... "
libs=$(cat /etc/ld.so.conf /etc/ld.so.conf.d/* | grep ^/ | tr "\n" " ")
libs="$libs /usr/lib /usr/lib64"
SO_FILE=$(find $libs -iname psqlodbcw.so 2> /dev/null | head -n 1)
if [ -n "$SO_FILE" ]; then
	echo -e "[${GREEN}found${WHITE}] ($SO_FILE)"
else
	echo -e "[${RED}error${WHITE}]"
	echo -e "\n--> Can't find it (it is the Unicode ODBC driver for Postgres)"
	exit $RET_MISS
fi


# Processing
#-----------
echo -e "[${BLUE}*${WHITE}] Processing:"

# Extracting SQL archive
echo -e "    [${BLUE}*${WHITE}] Extracting database archive"
$PROG_UNZIP -qo $_ROOT/$ZIP_ARCH -d /tmp
SQL_ARCH=/tmp/$ZIP_ARCH_NAME.sql
if [ $? -ne 0 ]; then
	RET=$RET_ERR
	echo -e "        [${RED}*${WHITE}] Error while extracting. Operation aborted"	
	
# Creating database
else
	echo -e "    [${BLUE}*${WHITE}] Creating database ${BLUE}$DB_NAME${WHITE}"
	$PSQL -c "CREATE DATABASE $DB_NAME WITH ENCODING = 'UTF8';" 1>/dev/null
	if [ $? -ne 0 ]; then
		RET=$RET_ERR
		echo -e "        [${RED}*${WHITE}] Error while creating database."	
		echo -e "            If it already exists, you will have to drop it manually, and launch"
		echo -e "            this script again later. I won't drop this existing database, to"
		echo -e "            avoid all risks of data loss."
		echo -e "            --> To remove it ${RED}AT YOUR OWN RISKS${WHITE}, use the following command,"
		echo -e "                ${RED}${BOLD}which WILL ERASE ALL THIS DATABASE, WITH NO COMING BACK:${WHITE}"
		echo -e "                  $PSQL -c \"DROP DATABASE $DB_NAME;\""
		
	# Creating connection roles (silently ignore errors if roles already exist)
	else
		for con_role in nat iav invite user_1; do
			echo -e "    [${BLUE}*${WHITE}] Creating connection role ${BLUE}$con_role${WHITE}"
			$PSQL -c "CREATE ROLE $con_role LOGIN PASSWORD '$con_role' NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;" &>/dev/null
		done
		
		# Inserting data
		echo -e "    [${BLUE}*${WHITE}] Filling database (please be patient, it will take a while)"
		$PSQL -f $SQL_ARCH $DB_NAME 1>/dev/null
		if [ $? -ne 0 ]; then
			RET=$RET_ERR
			echo -e "        [${RED}*${WHITE}] Error while extracting. Operation aborted"
		fi
		
		# Adding ODBC link, using iav user to connect
		if [ -z "$(grep $DB_NAME /etc/odbc.ini 2>/dev/null)" ]; then
			echo -e "    [${BLUE}*${WHITE}] Creating ODBC link in /etc/odbc.ini"
			cat $_ROOT/$ODBC_LINK \
				| sed -r 's~^(Driver\s*=\s*).+$~\1'$SO_FILE'~' \
				>> /etc/odbc.ini
		fi
		# Add an entrey in pg_hba.conf for iav (only on local!)
		if [ -z "$(grep $DB_NAME $PG_HBA)" ]; then
			echo -e "    [${BLUE}*${WHITE}] Adding 'iav' connection role in $PG_HBA"
			# Add to the beginning of the file (works because "()" are interpreted BEFORE we actually write in the same file we read ;)
			echo -e "# Added for StacomiR ODBC connection\n" \
					"host\t$DB_NAME\tiav\t\t127.0.0.1/32\t\tmd5\n" \
					"host\t$DB_NAME\tiav\t\t::1/128\t\t\tmd5\n\n" \
					"$(cat $PG_HBA)" \
				> $PG_HBA
			# Reload the config
			echo -e "    [${BLUE}*${WHITE}] Reloading the config"
			$PSQL -c "SELECT pg_reload_conf();" &>/dev/null
		fi
		# Test the ODBC connection
		echo -e "    [${BLUE}*${WHITE}] Testing the ODBC link..."
		echo "SELECT 1;" | isql $DB_NAME -b &>/dev/null
		if [ $? -eq 0 ]; then
			echo -e "        [${GREEN}*${WHITE}] Valid"
		else
			echo -e "        [${RED}*${WHITE}] Uh oh... can't login. Ident method failed."
			echo -e "        [${RED}*${WHITE}] You will have to do it by yourself:"
			echo -e "        [${RED}*${WHITE}]  - edit $PG_HBA"
			echo -e "        [${RED}*${WHITE}]  - add an entrey for user 'iav' on database '$DB_NAME', method 'ident'"
			RET=1
		fi
	fi
fi


# Ending
#-------
if [ $RET -eq 0 ]; then
	echo -e "    [${BLUE}*${WHITE}] Great, it is successfully Installed"
fi

# Removing temporary files at the end
echo -e "    [${BLUE}*${WHITE}] Removing temporary files"
rm -f $SQL_ARCH 2>/dev/null

exit $RET
