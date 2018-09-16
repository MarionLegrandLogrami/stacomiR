--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: bd_contmig_nat; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE bd_contmig_nat WITH TEMPLATE = template0 ENCODING = 'UTF8' LC_COLLATE = 'French_France.1252' LC_CTYPE = 'French_France.1252';


ALTER DATABASE bd_contmig_nat OWNER TO postgres;

\connect bd_contmig_nat

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: bd_contmig_nat; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON DATABASE bd_contmig_nat IS 'Base au format national v0.3.1274 du 05/01/2010';


--
-- Name: nat; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA nat;


ALTER SCHEMA nat OWNER TO postgres;

--
-- Name: SCHEMA nat; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA nat IS 'standard public schema';


--
-- Name: ref; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA ref;


ALTER SCHEMA ref OWNER TO postgres;

--
-- Name: tiger; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA tiger;


ALTER SCHEMA tiger OWNER TO postgres;

--
-- Name: tiger_data; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA tiger_data;


ALTER SCHEMA tiger_data OWNER TO postgres;

--
-- Name: topology; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA topology;


ALTER SCHEMA topology OWNER TO postgres;

--
-- Name: user_1; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA user_1;


ALTER SCHEMA user_1 OWNER TO postgres;

--
-- Name: SCHEMA user_1; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA user_1 IS 'standard public schema';


--
-- Name: user_2; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA user_2;


ALTER SCHEMA user_2 OWNER TO postgres;

--
-- Name: SCHEMA user_2; Type: COMMENT; Schema: -; Owner: postgres
--

COMMENT ON SCHEMA user_2 IS 'standard public schema';


--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


--
-- Name: address_standardizer; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS address_standardizer WITH SCHEMA public;


--
-- Name: EXTENSION address_standardizer; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION address_standardizer IS 'Used to parse an address into constituent elements. Generally used to support geocoding address normalization step.';


--
-- Name: fuzzystrmatch; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS fuzzystrmatch WITH SCHEMA public;


--
-- Name: EXTENSION fuzzystrmatch; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION fuzzystrmatch IS 'determine similarities and distance between strings';


--
-- Name: ogr_fdw; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS ogr_fdw WITH SCHEMA public;


--
-- Name: EXTENSION ogr_fdw; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION ogr_fdw IS 'foreign-data wrapper for GIS data access';


--
-- Name: postgis; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA public;


--
-- Name: EXTENSION postgis; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis IS 'PostGIS geometry, geography, and raster spatial types and functions';


--
-- Name: pgrouting; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS pgrouting WITH SCHEMA public;


--
-- Name: EXTENSION pgrouting; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pgrouting IS 'pgRouting Extension';


--
-- Name: pointcloud; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS pointcloud WITH SCHEMA public;


--
-- Name: EXTENSION pointcloud; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pointcloud IS 'data type for lidar point clouds';


--
-- Name: pointcloud_postgis; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS pointcloud_postgis WITH SCHEMA public;


--
-- Name: EXTENSION pointcloud_postgis; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION pointcloud_postgis IS 'integration for pointcloud LIDAR data and PostGIS geometry data';


--
-- Name: postgis_sfcgal; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS postgis_sfcgal WITH SCHEMA public;


--
-- Name: EXTENSION postgis_sfcgal; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis_sfcgal IS 'PostGIS SFCGAL functions';


--
-- Name: postgis_tiger_geocoder; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS postgis_tiger_geocoder WITH SCHEMA tiger;


--
-- Name: EXTENSION postgis_tiger_geocoder; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis_tiger_geocoder IS 'PostGIS tiger geocoder and reverse geocoder';


--
-- Name: postgis_topology; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS postgis_topology WITH SCHEMA topology;


--
-- Name: EXTENSION postgis_topology; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION postgis_topology IS 'PostGIS topology spatial types and functions';


--
-- Name: tablefunc; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS tablefunc WITH SCHEMA public;


--
-- Name: EXTENSION tablefunc; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION tablefunc IS 'functions that manipulate whole tables, including crosstab';


SET search_path = public, pg_catalog;

--
-- Name: dblink_pkey_results; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE dblink_pkey_results AS (
	"position" integer,
	colname text
);


ALTER TYPE dblink_pkey_results OWNER TO postgres;

SET search_path = ref, pg_catalog;

--
-- Name: breakpoint; Type: TYPE; Schema: ref; Owner: postgres
--

CREATE TYPE breakpoint AS (
	func oid,
	linenumber integer,
	targetname text
);


ALTER TYPE breakpoint OWNER TO postgres;

--
-- Name: dblink_pkey_results; Type: TYPE; Schema: ref; Owner: postgres
--

CREATE TYPE dblink_pkey_results AS (
	"position" integer,
	colname text
);


ALTER TYPE dblink_pkey_results OWNER TO postgres;

--
-- Name: frame; Type: TYPE; Schema: ref; Owner: postgres
--

CREATE TYPE frame AS (
	level integer,
	targetname text,
	func oid,
	linenumber integer,
	args text
);


ALTER TYPE frame OWNER TO postgres;

--
-- Name: proxyinfo; Type: TYPE; Schema: ref; Owner: postgres
--

CREATE TYPE proxyinfo AS (
	serverversionstr text,
	serverversionnum integer,
	proxyapiver integer,
	serverprocessid integer
);


ALTER TYPE proxyinfo OWNER TO postgres;

--
-- Name: targetinfo; Type: TYPE; Schema: ref; Owner: postgres
--

CREATE TYPE targetinfo AS (
	target oid,
	schema oid,
	nargs integer,
	argtypes oidvector,
	targetname name,
	argmodes "char"[],
	argnames text[],
	targetlang oid,
	fqname text,
	returnsset boolean,
	returntype oid
);


ALTER TYPE targetinfo OWNER TO postgres;

--
-- Name: var; Type: TYPE; Schema: ref; Owner: postgres
--

CREATE TYPE var AS (
	name text,
	varclass character(1),
	linenumber integer,
	isunique boolean,
	isconst boolean,
	isnotnull boolean,
	dtype oid,
	value text
);


ALTER TYPE var OWNER TO postgres;

SET search_path = user_1, pg_catalog;

--
-- Name: breakpoint; Type: TYPE; Schema: user_1; Owner: postgres
--

CREATE TYPE breakpoint AS (
	func oid,
	linenumber integer,
	targetname text
);


ALTER TYPE breakpoint OWNER TO postgres;

--
-- Name: dblink_pkey_results; Type: TYPE; Schema: user_1; Owner: postgres
--

CREATE TYPE dblink_pkey_results AS (
	"position" integer,
	colname text
);


ALTER TYPE dblink_pkey_results OWNER TO postgres;

--
-- Name: frame; Type: TYPE; Schema: user_1; Owner: postgres
--

CREATE TYPE frame AS (
	level integer,
	targetname text,
	func oid,
	linenumber integer,
	args text
);


ALTER TYPE frame OWNER TO postgres;

--
-- Name: proxyinfo; Type: TYPE; Schema: user_1; Owner: postgres
--

CREATE TYPE proxyinfo AS (
	serverversionstr text,
	serverversionnum integer,
	proxyapiver integer,
	serverprocessid integer
);


ALTER TYPE proxyinfo OWNER TO postgres;

--
-- Name: targetinfo; Type: TYPE; Schema: user_1; Owner: postgres
--

CREATE TYPE targetinfo AS (
	target oid,
	schema oid,
	nargs integer,
	argtypes oidvector,
	targetname name,
	argmodes "char"[],
	argnames text[],
	targetlang oid,
	fqname text,
	returnsset boolean,
	returntype oid
);


ALTER TYPE targetinfo OWNER TO postgres;

--
-- Name: var; Type: TYPE; Schema: user_1; Owner: postgres
--

CREATE TYPE var AS (
	name text,
	varclass character(1),
	linenumber integer,
	isunique boolean,
	isconst boolean,
	isnotnull boolean,
	dtype oid,
	value text
);


ALTER TYPE var OWNER TO postgres;

SET search_path = user_2, pg_catalog;

--
-- Name: breakpoint; Type: TYPE; Schema: user_2; Owner: postgres
--

CREATE TYPE breakpoint AS (
	func oid,
	linenumber integer,
	targetname text
);


ALTER TYPE breakpoint OWNER TO postgres;

--
-- Name: dblink_pkey_results; Type: TYPE; Schema: user_2; Owner: postgres
--

CREATE TYPE dblink_pkey_results AS (
	"position" integer,
	colname text
);


ALTER TYPE dblink_pkey_results OWNER TO postgres;

--
-- Name: frame; Type: TYPE; Schema: user_2; Owner: postgres
--

CREATE TYPE frame AS (
	level integer,
	targetname text,
	func oid,
	linenumber integer,
	args text
);


ALTER TYPE frame OWNER TO postgres;

--
-- Name: proxyinfo; Type: TYPE; Schema: user_2; Owner: postgres
--

CREATE TYPE proxyinfo AS (
	serverversionstr text,
	serverversionnum integer,
	proxyapiver integer,
	serverprocessid integer
);


ALTER TYPE proxyinfo OWNER TO postgres;

--
-- Name: targetinfo; Type: TYPE; Schema: user_2; Owner: postgres
--

CREATE TYPE targetinfo AS (
	target oid,
	schema oid,
	nargs integer,
	argtypes oidvector,
	targetname name,
	argmodes "char"[],
	argnames text[],
	targetlang oid,
	fqname text,
	returnsset boolean,
	returntype oid
);


ALTER TYPE targetinfo OWNER TO postgres;

--
-- Name: var; Type: TYPE; Schema: user_2; Owner: postgres
--

CREATE TYPE var AS (
	name text,
	varclass character(1),
	linenumber integer,
	isunique boolean,
	isconst boolean,
	isnotnull boolean,
	dtype oid,
	value text
);


ALTER TYPE var OWNER TO postgres;

SET search_path = nat, pg_catalog;

--
-- Name: compile(character varying); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION compile(myschema character varying) RETURNS integer
    LANGUAGE plpgsql
    AS $$
	DECLARE
	totalcount int;
	linetablecount int;
	BEGIN	  
		SELECT INTO linetablecount nat.compiletable(myschema,'t_station_sta');
		totalcount=linetablecount;
		SELECT INTO linetablecount nat.compiletable(myschema,'t_ouvrage_ouv');
		totalcount=totalcount+linetablecount;	
		SELECT INTO linetablecount nat.compiletable(myschema,'tg_dispositif_dis');
		totalcount=totalcount+linetablecount;	
		SELECT INTO linetablecount nat.compiletable(myschema,'t_dispositiffranchissement_dif');
		totalcount=totalcount+linetablecount;	
		SELECT INTO linetablecount nat.compiletable(myschema,'t_dispositifcomptage_dic');
		totalcount=totalcount+linetablecount;	
		SELECT INTO linetablecount nat.compile_table_per_bunch(myschema,'t_operation_ope',1000);
		totalcount=totalcount+linetablecount;			
		SELECT INTO linetablecount nat.compile_table_per_bunch(myschema,'t_lot_lot',1000);
		totalcount=totalcount+linetablecount;			
		SELECT INTO linetablecount nat.compile_table_per_bunch(myschema,'tj_caracteristiquelot_car',1000);
		totalcount=totalcount+linetablecount;			
		SELECT INTO linetablecount nat.compiletable(myschema,'t_periodefonctdispositif_per');
		totalcount=totalcount+linetablecount;			
		SELECT INTO linetablecount nat.compiletable(myschema,'t_bilanmigrationjournalier_bjo');
		totalcount=totalcount+linetablecount;			
		SELECT INTO linetablecount nat.compiletable(myschema,'tj_stationmesure_stm');
		totalcount=totalcount+linetablecount;			
		SELECT INTO linetablecount nat.compiletable(myschema,'tj_coefficientconversion_coe');
		totalcount=totalcount+linetablecount;			
		SELECT INTO linetablecount nat.compiletable(myschema,'t_operationmarquage_omq');
		totalcount=totalcount+linetablecount;			
		SELECT INTO linetablecount nat.compiletable(myschema,'tj_dfesttype_dft');
		totalcount=totalcount+linetablecount;			
		SELECT INTO linetablecount nat.compiletable(myschema,'t_marque_mqe');
		totalcount=totalcount+linetablecount;
		SELECT INTO linetablecount nat.compiletable(myschema,'tj_actionmarquage_act');
		totalcount=totalcount+linetablecount;
		SELECT INTO linetablecount nat.compiletable(myschema,'tj_prelevementlot_prl');
		totalcount=totalcount+linetablecount;
		SELECT INTO linetablecount nat.compiletable(myschema,'tj_dfestdestinea_dtx');
		totalcount=totalcount+linetablecount;		
		SELECT INTO linetablecount nat.compile_table_per_bunch(myschema,'tj_conditionenvironnementale_env',1000);
		totalcount=totalcount+linetablecount;		
		SELECT INTO linetablecount nat.compiletable(myschema,'t_bilanmigrationmensuel_bme');
		totalcount=totalcount+linetablecount;		
		SELECT INTO linetablecount nat.compiletable(myschema,'tj_tauxechappement_txe');
		totalcount=totalcount+linetablecount;			
	RETURN totalcount;
	END;
	$$;


ALTER FUNCTION nat.compile(myschema character varying) OWNER TO postgres;

--
-- Name: FUNCTION compile(myschema character varying); Type: COMMENT; Schema: nat; Owner: postgres
--

COMMENT ON FUNCTION compile(myschema character varying) IS 'fonction pour compiler le schema d''un operateur';


--
-- Name: compile_table_per_bunch(character varying, character varying, integer); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION compile_table_per_bunch(myschema character varying, mytable character varying, _size integer) RETURNS integer
    LANGUAGE plpgsql
    AS $$
	DECLARE
	linetablecount int;
	currentrow int;
	result record;
	pk varchar;	
	BEGIN	  	
		RAISE NOTICE 'table source  =%',myschema||'.'||mytable;
		EXECUTE 'SELECT count(*) FROM '||myschema||'.'||mytable INTO linetablecount;
		--OPEN curs FOR EXECUTE 'select * from '||myschema||'.'||mytable;	
		currentrow=0;
		-- récupération de la clé primaire integer de la table
		EXECUTE 'SELECT attname from (SELECT pg_attribute.attname, 
		format_type(pg_attribute.atttypid, pg_attribute.atttypmod)
		FROM pg_index, pg_class, pg_attribute WHERE 
		pg_class.oid = '''||myschema||'.'||mytable||'''::regclass AND
		indrelid = pg_class.oid AND
		pg_attribute.attrelid = pg_class.oid AND 
		pg_attribute.attnum = any(pg_index.indkey)
		AND indisprimary) sub
		where format_type=''integer''' INTO pk;
		--pk=split_part(mytable,'_',3)||'_identifiant';
		while (currentrow<linetablecount) LOOP 
			EXECUTE 'INSERT INTO nat.'||mytable||' select * from '||myschema||'.'||mytable||' ORDER BY '||pk||' OFFSET '||currentrow|| ' LIMIT '|| _size ;		
			currentrow=currentrow+_size;
			RAISE NOTICE 'lignes insérées =%',currentrow;	
		END LOOP;
	--RAISE NOTICE 'lignes insérées =%',linetablecount;
	RETURN linetablecount;
	END;
	$$;


ALTER FUNCTION nat.compile_table_per_bunch(myschema character varying, mytable character varying, _size integer) OWNER TO postgres;

--
-- Name: FUNCTION compile_table_per_bunch(myschema character varying, mytable character varying, _size integer); Type: COMMENT; Schema: nat; Owner: postgres
--

COMMENT ON FUNCTION compile_table_per_bunch(myschema character varying, mytable character varying, _size integer) IS 'function allowing to write _size lines at a time ';


--
-- Name: compiletable(character varying, character varying); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION compiletable(myschema character varying, mytable character varying) RETURNS integer
    LANGUAGE plpgsql
    AS $$
	DECLARE
	linetablecount int;
	BEGIN	  	
		RAISE NOTICE 'table source  =%',myschema||'.'||mytable;
		EXECUTE 'SELECT count(*) FROM '||myschema||'.'||mytable INTO linetablecount;
		EXECUTE 'INSERT INTO nat.'||mytable||' select * from '||myschema||'.'||mytable;	
		RAISE NOTICE 'lignes insérées =%',linetablecount;
	RETURN linetablecount;
	END;
	$$;


ALTER FUNCTION nat.compiletable(myschema character varying, mytable character varying) OWNER TO postgres;

--
-- Name: FUNCTION compiletable(myschema character varying, mytable character varying); Type: COMMENT; Schema: nat; Owner: postgres
--

COMMENT ON FUNCTION compiletable(myschema character varying, mytable character varying) IS 'fonction pour compiler un schema et une table appelee par la fonction compile ';


--
-- Name: do_not_delete_default(); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION do_not_delete_default() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   
DECLARE
_mas_code character varying(15);
    BEGIN      
        IF OLD.mas_code = 'ope_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas supprimer le masque par défaut';
        END IF;
          IF OLD.mas_code = 'lot_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas supprimer le masque par défaut';
        END IF;
RETURN OLD;
    END;
$$;


ALTER FUNCTION nat.do_not_delete_default() OWNER TO postgres;

--
-- Name: do_not_update_default(); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION do_not_update_default() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   
DECLARE
_mas_code character varying(15);
    BEGIN      
     IF OLD.mas_code = 'ope_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas modifier le masque par défaut';
        END IF;
          IF OLD.mas_code = 'lot_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas modifier le masque par défaut';
        END IF;
RETURN NEW;
    END;
$$;


ALTER FUNCTION nat.do_not_update_default() OWNER TO postgres;

--
-- Name: fct_coe_date(); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION fct_coe_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   





 	DECLARE nbChevauchements INTEGER ;





 	BEGIN


 	 	-- verification des non-chevauchements pour les taux


 	 	SELECT COUNT(*) INTO nbChevauchements


 	 	FROM   nat.tj_coefficientconversion_coe


 	 	WHERE  coe_tax_code = NEW.coe_tax_code


 	 	       AND coe_std_code = NEW.coe_std_code


 	 	       AND coe_qte_code = NEW.coe_qte_code


 	 	       AND (coe_date_debut, coe_date_fin) OVERLAPS (NEW.coe_date_debut, NEW.coe_date_fin)


 	 	;





		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !


 	 	IF (nbChevauchements > 1) THEN


 	 	 	RAISE EXCEPTION 'Les taux de conversion ne peuvent se chevaucher.'  ;


 	 	END IF  ;





		RETURN NEW ;


 	END  ;


$$;


ALTER FUNCTION nat.fct_coe_date() OWNER TO postgres;

--
-- Name: fct_ope_date(); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION fct_ope_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	DECLARE disCreation  	 	TIMESTAMP  ;
 	DECLARE disSuppression  	TIMESTAMP  ;
 	DECLARE nbChevauchements 	INTEGER    ;

 	BEGIN
 	 	-- Recuperation des dates du dispositif dans des variables
 	 	SELECT dis_date_creation, dis_date_suppression INTO disCreation, disSuppression
 	 	FROM   nat.tg_dispositif_dis
 	 	WHERE  dis_identifiant = NEW.ope_dic_identifiant
 	 	;

 	 	-- verification de la date de debut
 	 	IF ((NEW.ope_date_debut < disCreation) OR (NEW.ope_date_debut > disSuppression)) THEN
 	 	 	RAISE EXCEPTION 'Le debut de l operation doit etre inclus dans la periode d existence du DC.'  ;
 	 	END IF  ;

 	 	-- verification de la date de fin
 	 	IF ((NEW.ope_date_fin < disCreation) OR (NEW.ope_date_fin > disSuppression)) THEN
 	 	 	RAISE EXCEPTION 'La fin de l operation doit etre incluse dans la periode d existence du DC.'  ;
 	 	END IF  ;

 	 	-- verification des non-chevauchements pour les operations du dispositif
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   nat.t_operation_ope
 	 	WHERE  ope_dic_identifiant = NEW.ope_dic_identifiant 
 	 	       AND (ope_date_debut, ope_date_fin) OVERLAPS (NEW.ope_date_debut, NEW.ope_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN 
 	 		RAISE EXCEPTION 'Les operations ne peuvent se chevaucher.'  ;
 	 	END IF  ;

		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION nat.fct_ope_date() OWNER TO postgres;

--
-- Name: fct_per_date(); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION fct_per_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$ 

 	DECLARE disCreation  	 	TIMESTAMP  ;
 	DECLARE disSuppression  	TIMESTAMP  ;
 	DECLARE nbChevauchements 	INTEGER    ;
 	BEGIN


 	 	-- Recuperation des dates du dispositif dans des variables


 	 	SELECT dis_date_creation, dis_date_suppression INTO disCreation, disSuppression
 	 	FROM   nat.tg_dispositif_dis
 	 	WHERE  dis_identifiant = NEW.per_dis_identifiant
 	 	AND    dis_org_code=NEW.per_org_code;

 	 	-- verification de la date de debut


 	 	IF ((NEW.per_date_debut < disCreation) OR (NEW.per_date_debut > disSuppression)) THEN
 	 		RAISE EXCEPTION 'Le debut de la periode doit etre inclus dans la periode d existence du dispositif.'  ;
 	 	END IF  ;





 	 	-- verification de la date de fin


 	 	IF ((NEW.per_date_fin < disCreation) OR (NEW.per_date_fin > disSuppression)) THEN
 	 	 	RAISE EXCEPTION 'La fin de la periode doit etre incluse dans la periode d existence du dispositif.'  ;

 	 	END IF  ;





 	 	-- verification des non-chevauchements pour les periodes du dispositif


 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   nat.t_periodefonctdispositif_per
 	 	WHERE  (per_dis_identifiant,per_org_code) = (NEW.per_dis_identifiant,NEW.per_org_code)
 	 	       AND (per_date_debut, per_date_fin) OVERLAPS (NEW.per_date_debut, NEW.per_date_fin);
	-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !


 	 	IF (nbChevauchements > 1) THEN
 	 	 	RAISE EXCEPTION 'Les periodes ne peuvent se chevaucher.'  ;

 	 	END IF  ;
		RETURN NEW ;

 	END  ;


$$;


ALTER FUNCTION nat.fct_per_date() OWNER TO postgres;

--
-- Name: fct_per_suppression(); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION fct_per_suppression() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   





 	BEGIN


 	 	-- La periode precedent celle supprimee est prolongee


 	 	-- jusqu a la fin de la periode supprimee


 	 	UPDATE nat.t_periodefonctdispositif_per 


 	 	SET    per_date_fin = OLD.per_date_fin 


 	 	WHERE  per_date_fin= OLD.per_date_debut 


 	 	       AND per_dis_identifiant = OLD.per_dis_identifiant 


 	 	;


		RETURN NEW ;


 	END  ;





$$;


ALTER FUNCTION nat.fct_per_suppression() OWNER TO postgres;

--
-- Name: fct_txe_date(); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION fct_txe_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   





 	DECLARE nbChevauchements INTEGER ;





 	BEGIN


 	 	-- verification des non-chevauchements pour les taux


 	 	SELECT COUNT(*) INTO nbChevauchements


 	 	FROM   nat.tj_tauxechappement_txe


 	 	WHERE  txe_ouv_identifiant = NEW.txe_ouv_identifiant


 	 	       AND txe_tax_code = NEW.txe_tax_code


 	 	       AND txe_std_code = NEW.txe_std_code


 	 	       AND (txe_date_debut, txe_date_fin) OVERLAPS (NEW.txe_date_debut, NEW.txe_date_fin)


 	 	;





		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !


 	 	IF (nbChevauchements > 1) THEN


 	 	 	RAISE EXCEPTION 'Les taux d echappement ne peuvent se chevaucher.'  ;


 	 	END IF  ;





		RETURN NEW ;





 	END  ;


$$;


ALTER FUNCTION nat.fct_txe_date() OWNER TO postgres;

--
-- Name: masqueope(); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION masqueope() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   
DECLARE test text;


 	BEGIN
 	 	-- Recuperation du type
 	 	SELECT mas_type into test
 	 	FROM   nat.ts_masque_mas
 	 	WHERE  mas_id = NEW.mao_mas_id
 	 	;

 	 	-- verification 
 	 	IF (test!='ope') THEN
 	 	 	RAISE EXCEPTION 'Attention le masque n'' est pas un masque operation, changez le type de masque'  ;
 	 	END IF  ; 	 	

		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION nat.masqueope() OWNER TO postgres;

--
-- Name: nettoye(); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION nettoye() RETURNS void
    LANGUAGE plpgsql
    AS $$
	DECLARE
	totalcount int;
	linetablecount int;
	BEGIN	  
		delete from nat.tj_caracteristiquelot_car;					
		delete from nat.tj_coefficientconversion_coe;					
		delete from nat.tj_dfesttype_dft;
		delete from nat.tj_actionmarquage_act;		
		delete from nat.tj_dfesttype_dft;		
		delete from nat.tj_prelevementlot_prl;		
		delete from nat.tj_dfestdestinea_dtx;				
		delete from nat.tj_conditionenvironnementale_env;
		delete from nat.tj_stationmesure_stm;				
		delete from nat.tj_tauxechappement_txe;
		delete from nat.t_lot_lot;
		delete from nat.t_operation_ope;	
		delete from nat.t_periodefonctdispositif_per;
		delete from nat.t_dispositifcomptage_dic;	
		delete from nat.t_dispositiffranchissement_dif;			
		delete from nat.tg_dispositif_dis;					
		delete from nat.t_ouvrage_ouv;					
		delete from nat.t_station_sta;		
		delete from nat.t_marque_mqe;		
		delete from nat.t_bilanmigrationjournalier_bjo;
		delete from nat.t_bilanmigrationmensuel_bme;
		delete from nat.t_operationmarquage_omq;			
	RETURN;
	END;
	$$;


ALTER FUNCTION nat.nettoye() OWNER TO postgres;

--
-- Name: nettoye(character varying); Type: FUNCTION; Schema: nat; Owner: postgres
--

CREATE FUNCTION nettoye(myschema character varying) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
totalcount int;
linetablecount int;
BEGIN  
EXECUTE 'delete from nat.tj_caracteristiquelot_car where car_org_code='''||myschema||'''';
EXECUTE 'delete from nat.tj_coefficientconversion_coe where coe_org_code='''||myschema||'''';
EXECUTE 'delete from nat.tj_actionmarquage_act where act_org_code='''||myschema||'''';
EXECUTE 'delete from nat.tj_dfesttype_dft where dft_org_code='''||myschema||'''';
EXECUTE 'delete from nat.tj_prelevementlot_prl where prl_org_code='''||myschema||'''';
EXECUTE 'delete from nat.tj_dfestdestinea_dtx where dtx_org_code='''||myschema||'''';
EXECUTE 'delete from nat.tj_conditionenvironnementale_env where env_org_code='''||myschema||'''';
EXECUTE 'delete from nat.tj_stationmesure_stm where stm_org_code='''||myschema||'''';
EXECUTE 'delete from nat.tj_tauxechappement_txe where txe_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_lot_lot where lot_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_operation_ope where ope_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_periodefonctdispositif_per where per_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_dispositifcomptage_dic where dic_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_dispositiffranchissement_dif where dif_org_code='''||myschema||'''';
EXECUTE 'delete from nat.tg_dispositif_dis where dis_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_ouvrage_ouv where ouv_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_station_sta where sta_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_marque_mqe where mqe_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_bilanmigrationjournalier_bjo where bjo_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_bilanmigrationmensuel_bme where bme_org_code='''||myschema||'''';
EXECUTE 'delete from nat.t_operationmarquage_omq where omq_org_code='''||myschema||'''';
RETURN;
END;
$$;


ALTER FUNCTION nat.nettoye(myschema character varying) OWNER TO postgres;

--
-- Name: FUNCTION nettoye(myschema character varying); Type: COMMENT; Schema: nat; Owner: postgres
--

COMMENT ON FUNCTION nettoye(myschema character varying) IS 'Fonction pour supprimer les données d''un schema seulement dans la table nationale';


SET search_path = public, pg_catalog;

--
-- Name: addgeometrycolumn(character varying, character varying, integer, character varying, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION addgeometrycolumn(character varying, character varying, integer, character varying, integer) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
	ret  text;
BEGIN
	SELECT AddGeometryColumn('','',$1,$2,$3,$4,$5) into ret;
	RETURN ret;
END;
$_$;


ALTER FUNCTION public.addgeometrycolumn(character varying, character varying, integer, character varying, integer) OWNER TO postgres;

--
-- Name: FUNCTION addgeometrycolumn(character varying, character varying, integer, character varying, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION addgeometrycolumn(character varying, character varying, integer, character varying, integer) IS 'args: table_name, column_name, srid, type, dimension - Adds a geometry column to an existing table of attributes.';


--
-- Name: addgeometrycolumn(character varying, character varying, character varying, integer, character varying, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION addgeometrycolumn(character varying, character varying, character varying, integer, character varying, integer) RETURNS text
    LANGUAGE plpgsql STABLE STRICT
    AS $_$
DECLARE
	ret  text;
BEGIN
	SELECT AddGeometryColumn('',$1,$2,$3,$4,$5,$6) into ret;
	RETURN ret;
END;
$_$;


ALTER FUNCTION public.addgeometrycolumn(character varying, character varying, character varying, integer, character varying, integer) OWNER TO postgres;

--
-- Name: FUNCTION addgeometrycolumn(character varying, character varying, character varying, integer, character varying, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION addgeometrycolumn(character varying, character varying, character varying, integer, character varying, integer) IS 'args: schema_name, table_name, column_name, srid, type, dimension - Adds a geometry column to an existing table of attributes.';


--
-- Name: addgeometrycolumn(character varying, character varying, character varying, character varying, integer, character varying, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION addgeometrycolumn(character varying, character varying, character varying, character varying, integer, character varying, integer) RETURNS text
    LANGUAGE plpgsql STRICT
    AS $_$
DECLARE
	catalog_name alias for $1;
	schema_name alias for $2;
	table_name alias for $3;
	column_name alias for $4;
	new_srid alias for $5;
	new_type alias for $6;
	new_dim alias for $7;
	rec RECORD;
	sr varchar;
	real_schema name;
	sql text;

BEGIN

	-- Verify geometry type
	IF ( NOT ( (new_type = 'GEOMETRY') OR
			   (new_type = 'GEOMETRYCOLLECTION') OR
			   (new_type = 'POINT') OR
			   (new_type = 'MULTIPOINT') OR
			   (new_type = 'POLYGON') OR
			   (new_type = 'MULTIPOLYGON') OR
			   (new_type = 'LINESTRING') OR
			   (new_type = 'MULTILINESTRING') OR
			   (new_type = 'GEOMETRYCOLLECTIONM') OR
			   (new_type = 'POINTM') OR
			   (new_type = 'MULTIPOINTM') OR
			   (new_type = 'POLYGONM') OR
			   (new_type = 'MULTIPOLYGONM') OR
			   (new_type = 'LINESTRINGM') OR
			   (new_type = 'MULTILINESTRINGM') OR
			   (new_type = 'CIRCULARSTRING') OR
			   (new_type = 'CIRCULARSTRINGM') OR
			   (new_type = 'COMPOUNDCURVE') OR
			   (new_type = 'COMPOUNDCURVEM') OR
			   (new_type = 'CURVEPOLYGON') OR
			   (new_type = 'CURVEPOLYGONM') OR
			   (new_type = 'MULTICURVE') OR
			   (new_type = 'MULTICURVEM') OR
			   (new_type = 'MULTISURFACE') OR
			   (new_type = 'MULTISURFACEM')) )
	THEN
		RAISE EXCEPTION 'Invalid type name - valid ones are:
	POINT, MULTIPOINT,
	LINESTRING, MULTILINESTRING,
	POLYGON, MULTIPOLYGON,
	CIRCULARSTRING, COMPOUNDCURVE, MULTICURVE,
	CURVEPOLYGON, MULTISURFACE,
	GEOMETRY, GEOMETRYCOLLECTION,
	POINTM, MULTIPOINTM,
	LINESTRINGM, MULTILINESTRINGM,
	POLYGONM, MULTIPOLYGONM,
	CIRCULARSTRINGM, COMPOUNDCURVEM, MULTICURVEM
	CURVEPOLYGONM, MULTISURFACEM,
	or GEOMETRYCOLLECTIONM';
		RETURN 'fail';
	END IF;


	-- Verify dimension
	IF ( (new_dim >4) OR (new_dim <0) ) THEN
		RAISE EXCEPTION 'invalid dimension';
		RETURN 'fail';
	END IF;

	IF ( (new_type LIKE '%M') AND (new_dim!=3) ) THEN
		RAISE EXCEPTION 'TypeM needs 3 dimensions';
		RETURN 'fail';
	END IF;


	-- Verify SRID
	IF ( new_srid != -1 ) THEN
		SELECT SRID INTO sr FROM spatial_ref_sys WHERE SRID = new_srid;
		IF NOT FOUND THEN
			RAISE EXCEPTION 'AddGeometryColumns() - invalid SRID';
			RETURN 'fail';
		END IF;
	END IF;


	-- Verify schema
	IF ( schema_name IS NOT NULL AND schema_name != '' ) THEN
		sql := 'SELECT nspname FROM pg_namespace ' ||
			'WHERE text(nspname) = ' || quote_literal(schema_name) ||
			'LIMIT 1';
		RAISE DEBUG '%', sql;
		EXECUTE sql INTO real_schema;

		IF ( real_schema IS NULL ) THEN
			RAISE EXCEPTION 'Schema % is not a valid schemaname', quote_literal(schema_name);
			RETURN 'fail';
		END IF;
	END IF;

	IF ( real_schema IS NULL ) THEN
		RAISE DEBUG 'Detecting schema';
		sql := 'SELECT n.nspname AS schemaname ' ||
			'FROM pg_catalog.pg_class c ' ||
			  'JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace ' ||
			'WHERE c.relkind = ' || quote_literal('r') ||
			' AND n.nspname NOT IN (' || quote_literal('pg_catalog') || ', ' || quote_literal('pg_toast') || ')' ||
			' AND pg_catalog.pg_table_is_visible(c.oid)' ||
			' AND c.relname = ' || quote_literal(table_name);
		RAISE DEBUG '%', sql;
		EXECUTE sql INTO real_schema;

		IF ( real_schema IS NULL ) THEN
			RAISE EXCEPTION 'Table % does not occur in the search_path', quote_literal(table_name);
			RETURN 'fail';
		END IF;
	END IF;


	-- Add geometry column to table
	sql := 'ALTER TABLE ' ||
		quote_ident(real_schema) || '.' || quote_ident(table_name)
		|| ' ADD COLUMN ' || quote_ident(column_name) ||
		' geometry ';
	RAISE DEBUG '%', sql;
	EXECUTE sql;


	-- Delete stale record in geometry_columns (if any)
	sql := 'DELETE FROM geometry_columns WHERE
		f_table_catalog = ' || quote_literal('') ||
		' AND f_table_schema = ' ||
		quote_literal(real_schema) ||
		' AND f_table_name = ' || quote_literal(table_name) ||
		' AND f_geometry_column = ' || quote_literal(column_name);
	RAISE DEBUG '%', sql;
	EXECUTE sql;


	-- Add record in geometry_columns
	sql := 'INSERT INTO geometry_columns (f_table_catalog,f_table_schema,f_table_name,' ||
										  'f_geometry_column,coord_dimension,srid,type)' ||
		' VALUES (' ||
		quote_literal('') || ',' ||
		quote_literal(real_schema) || ',' ||
		quote_literal(table_name) || ',' ||
		quote_literal(column_name) || ',' ||
		new_dim::text || ',' ||
		new_srid::text || ',' ||
		quote_literal(new_type) || ')';
	RAISE DEBUG '%', sql;
	EXECUTE sql;


	-- Add table CHECKs
	sql := 'ALTER TABLE ' ||
		quote_ident(real_schema) || '.' || quote_ident(table_name)
		|| ' ADD CONSTRAINT '
		|| quote_ident('enforce_srid_' || column_name)
		|| ' CHECK (ST_SRID(' || quote_ident(column_name) ||
		') = ' || new_srid::text || ')' ;
	RAISE DEBUG '%', sql;
	EXECUTE sql;

	sql := 'ALTER TABLE ' ||
		quote_ident(real_schema) || '.' || quote_ident(table_name)
		|| ' ADD CONSTRAINT '
		|| quote_ident('enforce_dims_' || column_name)
		|| ' CHECK (ST_NDims(' || quote_ident(column_name) ||
		') = ' || new_dim::text || ')' ;
	RAISE DEBUG '%', sql;
	EXECUTE sql;

	IF ( NOT (new_type = 'GEOMETRY')) THEN
		sql := 'ALTER TABLE ' ||
			quote_ident(real_schema) || '.' || quote_ident(table_name) || ' ADD CONSTRAINT ' ||
			quote_ident('enforce_geotype_' || column_name) ||
			' CHECK (GeometryType(' ||
			quote_ident(column_name) || ')=' ||
			quote_literal(new_type) || ' OR (' ||
			quote_ident(column_name) || ') is null)';
		RAISE DEBUG '%', sql;
		EXECUTE sql;
	END IF;

	RETURN
		real_schema || '.' ||
		table_name || '.' || column_name ||
		' SRID:' || new_srid::text ||
		' TYPE:' || new_type ||
		' DIMS:' || new_dim::text || ' ';
END;
$_$;


ALTER FUNCTION public.addgeometrycolumn(character varying, character varying, character varying, character varying, integer, character varying, integer) OWNER TO postgres;

--
-- Name: FUNCTION addgeometrycolumn(character varying, character varying, character varying, character varying, integer, character varying, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION addgeometrycolumn(character varying, character varying, character varying, character varying, integer, character varying, integer) IS 'args: catalog_name, schema_name, table_name, column_name, srid, type, dimension - Adds a geometry column to an existing table of attributes.';


--
-- Name: affine(geometry, double precision, double precision, double precision, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION affine(geometry, double precision, double precision, double precision, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT affine($1,  $2, $3, 0,  $4, $5, 0,  0, 0, 1,  $6, $7, 0)$_$;


ALTER FUNCTION public.affine(geometry, double precision, double precision, double precision, double precision, double precision, double precision) OWNER TO postgres;

--
-- Name: asgml(geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION asgml(geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML(2, $1, 15, 0)$_$;


ALTER FUNCTION public.asgml(geometry) OWNER TO postgres;

--
-- Name: asgml(geometry, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION asgml(geometry, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML(2, $1, $2, 0)$_$;


ALTER FUNCTION public.asgml(geometry, integer) OWNER TO postgres;

--
-- Name: askml(geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION askml(geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML(2, transform($1,4326), 15)$_$;


ALTER FUNCTION public.askml(geometry) OWNER TO postgres;

--
-- Name: askml(geometry, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION askml(geometry, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML(2, transform($1,4326), $2)$_$;


ALTER FUNCTION public.askml(geometry, integer) OWNER TO postgres;

--
-- Name: askml(integer, geometry, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION askml(integer, geometry, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML($1, transform($2,4326), $3)$_$;


ALTER FUNCTION public.askml(integer, geometry, integer) OWNER TO postgres;

--
-- Name: bdmpolyfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION bdmpolyfromtext(text, integer) RETURNS geometry
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
	geomtext alias for $1;
	srid alias for $2;
	mline geometry;
	geom geometry;
BEGIN
	mline := MultiLineStringFromText(geomtext, srid);

	IF mline IS NULL
	THEN
		RAISE EXCEPTION 'Input is not a MultiLinestring';
	END IF;

	geom := multi(BuildArea(mline));

	RETURN geom;
END;
$_$;


ALTER FUNCTION public.bdmpolyfromtext(text, integer) OWNER TO postgres;

--
-- Name: bdpolyfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION bdpolyfromtext(text, integer) RETURNS geometry
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
	geomtext alias for $1;
	srid alias for $2;
	mline geometry;
	geom geometry;
BEGIN
	mline := MultiLineStringFromText(geomtext, srid);

	IF mline IS NULL
	THEN
		RAISE EXCEPTION 'Input is not a MultiLinestring';
	END IF;

	geom := BuildArea(mline);

	IF GeometryType(geom) != 'POLYGON'
	THEN
		RAISE EXCEPTION 'Input returns more then a single polygon, try using BdMPolyFromText instead';
	END IF;

	RETURN geom;
END;
$_$;


ALTER FUNCTION public.bdpolyfromtext(text, integer) OWNER TO postgres;

--
-- Name: buffer(geometry, double precision, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION buffer(geometry, double precision, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Buffer($1, $2, $3)$_$;


ALTER FUNCTION public.buffer(geometry, double precision, integer) OWNER TO postgres;

--
-- Name: chercheoverlaps(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION chercheoverlaps() RETURNS SETOF text
    LANGUAGE plpgsql
    AS $$
DECLARE
    nbChevauchements 	INTEGER    ;
    idope               INTEGER ;
    r 			adourtemp.t_operation_ope2%rowtype;
    resultat           text;
BEGIN
    FOR r IN SELECT * FROM adourtemp.t_operation_ope2
    LOOP
        SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   adourtemp.t_operation_ope2
 	 	WHERE  (ope_date_debut, ope_date_fin) OVERLAPS (r.ope_date_debut, r.ope_date_fin);
		IF (nbChevauchements > 1) THEN
		resultat=r.ope_identifiant;
		-- je vais chercher la deuxième opération incriminée
		SELECT ope_identifiant INTO idope
 	 	        FROM   adourtemp.t_operation_ope2
 	 	       WHERE  (ope_date_debut, ope_date_fin) OVERLAPS (r.ope_date_debut, r.ope_date_fin)
 	 	       AND ope_identifiant<>r.ope_identifiant;
		-- Il existe un chevauchement		
 	 	RAISE NOTICE 'Les dates se chevauchent : premiere operation --->%',resultat;
 	 	RAISE NOTICE 'deuxième opération --->%',idope;
 	 	END IF  ;
 	 RETURN NEXT r;         
    END LOOP;
    RETURN;
END;
$$;


ALTER FUNCTION public.chercheoverlaps() OWNER TO postgres;

--
-- Name: chercheoverlaps(integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION chercheoverlaps(_ope_dic_identifiant integer) RETURNS SETOF text
    LANGUAGE plpgsql
    AS $$
DECLARE
    nbChevauchements 	INTEGER    ;
    idope               INTEGER ;
    r 			adourtemp.t_operation_ope%rowtype;
    resultat           text;
BEGIN
    FOR r IN SELECT * FROM adourtemp.t_operation_ope where ope_dic_identifiant=_ope_dic_identifiant
    LOOP
        SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   adourtemp.t_operation_ope
 	 	WHERE  (ope_date_debut, ope_date_fin) OVERLAPS (r.ope_date_debut, r.ope_date_fin)
		and ope_dic_identifiant=_ope_dic_identifiant
		and ope_date_debut>r.ope_date_debut;
		IF (nbChevauchements > 1) THEN
		resultat=r.ope_identifiant;
		-- je vais chercher la deuxième opération incriminée
		SELECT ope_identifiant INTO idope
 	 	        FROM   adourtemp.t_operation_ope
 	 	       WHERE  (ope_date_debut, ope_date_fin) OVERLAPS (r.ope_date_debut, r.ope_date_fin)
 	 	       AND ope_identifiant<>r.ope_identifiant
 	 	       AND ope_dic_identifiant=_ope_dic_identifiant
 	 	       and ope_date_debut>r.ope_date_debut;
		-- Il existe un chevauchement	
		EXECUTE 'insert into public.chevauchement values ('||resultat||','||idope||','||_ope_dic_identifiant||')';
 	 	RAISE NOTICE 'Les dates se chevauchent : premiere operation --->%',resultat;
 	 	RAISE NOTICE 'deuxième opération --->%',idope;
 	 	END IF  ;
 	 RETURN NEXT r;         
    END LOOP;
    RETURN;
END;
$$;


ALTER FUNCTION public.chercheoverlaps(_ope_dic_identifiant integer) OWNER TO postgres;

--
-- Name: dblink(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink(text) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_record';


ALTER FUNCTION public.dblink(text) OWNER TO postgres;

--
-- Name: dblink(text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink(text, boolean) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_record';


ALTER FUNCTION public.dblink(text, boolean) OWNER TO postgres;

--
-- Name: dblink(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink(text, text) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_record';


ALTER FUNCTION public.dblink(text, text) OWNER TO postgres;

--
-- Name: dblink(text, text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink(text, text, boolean) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_record';


ALTER FUNCTION public.dblink(text, text, boolean) OWNER TO postgres;

--
-- Name: dblink_build_sql_delete(text, int2vector, integer, text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_build_sql_delete(text, int2vector, integer, text[]) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_build_sql_delete';


ALTER FUNCTION public.dblink_build_sql_delete(text, int2vector, integer, text[]) OWNER TO postgres;

--
-- Name: dblink_build_sql_insert(text, int2vector, integer, text[], text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_build_sql_insert(text, int2vector, integer, text[], text[]) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_build_sql_insert';


ALTER FUNCTION public.dblink_build_sql_insert(text, int2vector, integer, text[], text[]) OWNER TO postgres;

--
-- Name: dblink_build_sql_update(text, int2vector, integer, text[], text[]); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_build_sql_update(text, int2vector, integer, text[], text[]) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_build_sql_update';


ALTER FUNCTION public.dblink_build_sql_update(text, int2vector, integer, text[], text[]) OWNER TO postgres;

--
-- Name: dblink_cancel_query(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_cancel_query(text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_cancel_query';


ALTER FUNCTION public.dblink_cancel_query(text) OWNER TO postgres;

--
-- Name: dblink_close(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_close(text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_close';


ALTER FUNCTION public.dblink_close(text) OWNER TO postgres;

--
-- Name: dblink_close(text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_close(text, boolean) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_close';


ALTER FUNCTION public.dblink_close(text, boolean) OWNER TO postgres;

--
-- Name: dblink_close(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_close(text, text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_close';


ALTER FUNCTION public.dblink_close(text, text) OWNER TO postgres;

--
-- Name: dblink_close(text, text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_close(text, text, boolean) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_close';


ALTER FUNCTION public.dblink_close(text, text, boolean) OWNER TO postgres;

--
-- Name: dblink_connect(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_connect(text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_connect';


ALTER FUNCTION public.dblink_connect(text) OWNER TO postgres;

--
-- Name: dblink_connect(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_connect(text, text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_connect';


ALTER FUNCTION public.dblink_connect(text, text) OWNER TO postgres;

--
-- Name: dblink_connect_u(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_connect_u(text) RETURNS text
    LANGUAGE c STRICT SECURITY DEFINER
    AS '$libdir/dblink', 'dblink_connect';


ALTER FUNCTION public.dblink_connect_u(text) OWNER TO postgres;

--
-- Name: dblink_connect_u(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_connect_u(text, text) RETURNS text
    LANGUAGE c STRICT SECURITY DEFINER
    AS '$libdir/dblink', 'dblink_connect';


ALTER FUNCTION public.dblink_connect_u(text, text) OWNER TO postgres;

--
-- Name: dblink_current_query(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_current_query() RETURNS text
    LANGUAGE c
    AS '$libdir/dblink', 'dblink_current_query';


ALTER FUNCTION public.dblink_current_query() OWNER TO postgres;

--
-- Name: dblink_disconnect(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_disconnect() RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_disconnect';


ALTER FUNCTION public.dblink_disconnect() OWNER TO postgres;

--
-- Name: dblink_disconnect(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_disconnect(text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_disconnect';


ALTER FUNCTION public.dblink_disconnect(text) OWNER TO postgres;

--
-- Name: dblink_error_message(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_error_message(text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_error_message';


ALTER FUNCTION public.dblink_error_message(text) OWNER TO postgres;

--
-- Name: dblink_exec(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_exec(text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_exec';


ALTER FUNCTION public.dblink_exec(text) OWNER TO postgres;

--
-- Name: dblink_exec(text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_exec(text, boolean) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_exec';


ALTER FUNCTION public.dblink_exec(text, boolean) OWNER TO postgres;

--
-- Name: dblink_exec(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_exec(text, text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_exec';


ALTER FUNCTION public.dblink_exec(text, text) OWNER TO postgres;

--
-- Name: dblink_exec(text, text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_exec(text, text, boolean) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_exec';


ALTER FUNCTION public.dblink_exec(text, text, boolean) OWNER TO postgres;

--
-- Name: dblink_fetch(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_fetch(text, integer) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_fetch';


ALTER FUNCTION public.dblink_fetch(text, integer) OWNER TO postgres;

--
-- Name: dblink_fetch(text, integer, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_fetch(text, integer, boolean) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_fetch';


ALTER FUNCTION public.dblink_fetch(text, integer, boolean) OWNER TO postgres;

--
-- Name: dblink_fetch(text, text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_fetch(text, text, integer) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_fetch';


ALTER FUNCTION public.dblink_fetch(text, text, integer) OWNER TO postgres;

--
-- Name: dblink_fetch(text, text, integer, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_fetch(text, text, integer, boolean) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_fetch';


ALTER FUNCTION public.dblink_fetch(text, text, integer, boolean) OWNER TO postgres;

--
-- Name: dblink_get_connections(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_get_connections() RETURNS text[]
    LANGUAGE c
    AS '$libdir/dblink', 'dblink_get_connections';


ALTER FUNCTION public.dblink_get_connections() OWNER TO postgres;

--
-- Name: dblink_get_pkey(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_get_pkey(text) RETURNS SETOF dblink_pkey_results
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_get_pkey';


ALTER FUNCTION public.dblink_get_pkey(text) OWNER TO postgres;

--
-- Name: dblink_get_result(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_get_result(text) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_get_result';


ALTER FUNCTION public.dblink_get_result(text) OWNER TO postgres;

--
-- Name: dblink_get_result(text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_get_result(text, boolean) RETURNS SETOF record
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_get_result';


ALTER FUNCTION public.dblink_get_result(text, boolean) OWNER TO postgres;

--
-- Name: dblink_is_busy(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_is_busy(text) RETURNS integer
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_is_busy';


ALTER FUNCTION public.dblink_is_busy(text) OWNER TO postgres;

--
-- Name: dblink_open(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_open(text, text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_open';


ALTER FUNCTION public.dblink_open(text, text) OWNER TO postgres;

--
-- Name: dblink_open(text, text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_open(text, text, boolean) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_open';


ALTER FUNCTION public.dblink_open(text, text, boolean) OWNER TO postgres;

--
-- Name: dblink_open(text, text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_open(text, text, text) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_open';


ALTER FUNCTION public.dblink_open(text, text, text) OWNER TO postgres;

--
-- Name: dblink_open(text, text, text, boolean); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_open(text, text, text, boolean) RETURNS text
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_open';


ALTER FUNCTION public.dblink_open(text, text, text, boolean) OWNER TO postgres;

--
-- Name: dblink_send_query(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION dblink_send_query(text, text) RETURNS integer
    LANGUAGE c STRICT
    AS '$libdir/dblink', 'dblink_send_query';


ALTER FUNCTION public.dblink_send_query(text, text) OWNER TO postgres;

--
-- Name: find_extent(text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION find_extent(text, text) RETURNS box2d
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
	tablename alias for $1;
	columnname alias for $2;
	myrec RECORD;

BEGIN
	FOR myrec IN EXECUTE 'SELECT extent("' || columnname || '") FROM "' || tablename || '"' LOOP
		return myrec.extent;
	END LOOP;
END;
$_$;


ALTER FUNCTION public.find_extent(text, text) OWNER TO postgres;

--
-- Name: find_extent(text, text, text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION find_extent(text, text, text) RETURNS box2d
    LANGUAGE plpgsql IMMUTABLE STRICT
    AS $_$
DECLARE
	schemaname alias for $1;
	tablename alias for $2;
	columnname alias for $3;
	myrec RECORD;

BEGIN
	FOR myrec IN EXECUTE 'SELECT extent("' || columnname || '") FROM "' || schemaname || '"."' || tablename || '"' LOOP
		return myrec.extent;
	END LOOP;
END;
$_$;


ALTER FUNCTION public.find_extent(text, text, text) OWNER TO postgres;

--
-- Name: fix_geometry_columns(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION fix_geometry_columns() RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
	mislinked record;
	result text;
	linked integer;
	deleted integer;
	foundschema integer;
BEGIN

	-- Since 7.3 schema support has been added.
	-- Previous postgis versions used to put the database name in
	-- the schema column. This needs to be fixed, so we try to
	-- set the correct schema for each geometry_colums record
	-- looking at table, column, type and srid.
	UPDATE geometry_columns SET f_table_schema = n.nspname
		FROM pg_namespace n, pg_class c, pg_attribute a,
			pg_constraint sridcheck, pg_constraint typecheck
			WHERE ( f_table_schema is NULL
		OR f_table_schema = ''
			OR f_table_schema NOT IN (
					SELECT nspname::varchar
					FROM pg_namespace nn, pg_class cc, pg_attribute aa
					WHERE cc.relnamespace = nn.oid
					AND cc.relname = f_table_name::name
					AND aa.attrelid = cc.oid
					AND aa.attname = f_geometry_column::name))
			AND f_table_name::name = c.relname
			AND c.oid = a.attrelid
			AND c.relnamespace = n.oid
			AND f_geometry_column::name = a.attname

			AND sridcheck.conrelid = c.oid
		AND sridcheck.consrc LIKE '(srid(% = %)'
			AND sridcheck.consrc ~ textcat(' = ', srid::text)

			AND typecheck.conrelid = c.oid
		AND typecheck.consrc LIKE
		'((geometrytype(%) = ''%''::text) OR (% IS NULL))'
			AND typecheck.consrc ~ textcat(' = ''', type::text)

			AND NOT EXISTS (
					SELECT oid FROM geometry_columns gc
					WHERE c.relname::varchar = gc.f_table_name
					AND n.nspname::varchar = gc.f_table_schema
					AND a.attname::varchar = gc.f_geometry_column
			);

	GET DIAGNOSTICS foundschema = ROW_COUNT;

	-- no linkage to system table needed
	return 'fixed:'||foundschema::text;

END;
$$;


ALTER FUNCTION public.fix_geometry_columns() OWNER TO postgres;

--
-- Name: geomcollfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION geomcollfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(GeomFromText($1)) = 'GEOMETRYCOLLECTION'
	THEN GeomFromText($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.geomcollfromtext(text) OWNER TO postgres;

--
-- Name: geomcollfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION geomcollfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(GeomFromText($1, $2)) = 'GEOMETRYCOLLECTION'
	THEN GeomFromText($1,$2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.geomcollfromtext(text, integer) OWNER TO postgres;

--
-- Name: geomcollfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION geomcollfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(GeomFromWKB($1)) = 'GEOMETRYCOLLECTION'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.geomcollfromwkb(bytea) OWNER TO postgres;

--
-- Name: geomcollfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION geomcollfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(GeomFromWKB($1, $2)) = 'GEOMETRYCOLLECTION'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.geomcollfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: geomfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION geomfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT geometryfromtext($1)$_$;


ALTER FUNCTION public.geomfromtext(text) OWNER TO postgres;

--
-- Name: geomfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION geomfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT geometryfromtext($1, $2)$_$;


ALTER FUNCTION public.geomfromtext(text, integer) OWNER TO postgres;

--
-- Name: geomfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION geomfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT setSRID(GeomFromWKB($1), $2)$_$;


ALTER FUNCTION public.geomfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: linefromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION linefromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = 'LINESTRING'
	THEN GeomFromText($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.linefromtext(text) OWNER TO postgres;

--
-- Name: linefromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION linefromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = 'LINESTRING'
	THEN GeomFromText($1,$2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.linefromtext(text, integer) OWNER TO postgres;

--
-- Name: linefromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION linefromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'LINESTRING'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.linefromwkb(bytea) OWNER TO postgres;

--
-- Name: linefromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION linefromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = 'LINESTRING'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.linefromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: linestringfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION linestringfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT LineFromText($1)$_$;


ALTER FUNCTION public.linestringfromtext(text) OWNER TO postgres;

--
-- Name: linestringfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION linestringfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT LineFromText($1, $2)$_$;


ALTER FUNCTION public.linestringfromtext(text, integer) OWNER TO postgres;

--
-- Name: linestringfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION linestringfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'LINESTRING'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.linestringfromwkb(bytea) OWNER TO postgres;

--
-- Name: linestringfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION linestringfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = 'LINESTRING'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.linestringfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: locate_along_measure(geometry, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION locate_along_measure(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT locate_between_measures($1, $2, $2) $_$;


ALTER FUNCTION public.locate_along_measure(geometry, double precision) OWNER TO postgres;

--
-- Name: mlinefromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mlinefromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = 'MULTILINESTRING'
	THEN GeomFromText($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mlinefromtext(text) OWNER TO postgres;

--
-- Name: mlinefromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mlinefromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE
	WHEN geometrytype(GeomFromText($1, $2)) = 'MULTILINESTRING'
	THEN GeomFromText($1,$2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mlinefromtext(text, integer) OWNER TO postgres;

--
-- Name: mlinefromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mlinefromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'MULTILINESTRING'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mlinefromwkb(bytea) OWNER TO postgres;

--
-- Name: mlinefromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mlinefromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = 'MULTILINESTRING'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mlinefromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: mpointfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mpointfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = 'MULTIPOINT'
	THEN GeomFromText($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mpointfromtext(text) OWNER TO postgres;

--
-- Name: mpointfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mpointfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1,$2)) = 'MULTIPOINT'
	THEN GeomFromText($1,$2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mpointfromtext(text, integer) OWNER TO postgres;

--
-- Name: mpointfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mpointfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'MULTIPOINT'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mpointfromwkb(bytea) OWNER TO postgres;

--
-- Name: mpointfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mpointfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1,$2)) = 'MULTIPOINT'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mpointfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: mpolyfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mpolyfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = 'MULTIPOLYGON'
	THEN GeomFromText($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mpolyfromtext(text) OWNER TO postgres;

--
-- Name: mpolyfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mpolyfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = 'MULTIPOLYGON'
	THEN GeomFromText($1,$2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mpolyfromtext(text, integer) OWNER TO postgres;

--
-- Name: mpolyfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mpolyfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'MULTIPOLYGON'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mpolyfromwkb(bytea) OWNER TO postgres;

--
-- Name: mpolyfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION mpolyfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = 'MULTIPOLYGON'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.mpolyfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: multilinefromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multilinefromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'MULTILINESTRING'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.multilinefromwkb(bytea) OWNER TO postgres;

--
-- Name: multilinefromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multilinefromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = 'MULTILINESTRING'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.multilinefromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: multilinestringfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multilinestringfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_MLineFromText($1)$_$;


ALTER FUNCTION public.multilinestringfromtext(text) OWNER TO postgres;

--
-- Name: multilinestringfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multilinestringfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT MLineFromText($1, $2)$_$;


ALTER FUNCTION public.multilinestringfromtext(text, integer) OWNER TO postgres;

--
-- Name: multipointfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multipointfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT MPointFromText($1)$_$;


ALTER FUNCTION public.multipointfromtext(text) OWNER TO postgres;

--
-- Name: multipointfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multipointfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT MPointFromText($1, $2)$_$;


ALTER FUNCTION public.multipointfromtext(text, integer) OWNER TO postgres;

--
-- Name: multipointfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multipointfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'MULTIPOINT'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.multipointfromwkb(bytea) OWNER TO postgres;

--
-- Name: multipointfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multipointfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1,$2)) = 'MULTIPOINT'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.multipointfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: multipolyfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multipolyfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'MULTIPOLYGON'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.multipolyfromwkb(bytea) OWNER TO postgres;

--
-- Name: multipolyfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multipolyfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = 'MULTIPOLYGON'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.multipolyfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: multipolygonfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multipolygonfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT MPolyFromText($1)$_$;


ALTER FUNCTION public.multipolygonfromtext(text) OWNER TO postgres;

--
-- Name: multipolygonfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION multipolygonfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT MPolyFromText($1, $2)$_$;


ALTER FUNCTION public.multipolygonfromtext(text, integer) OWNER TO postgres;

--
-- Name: percentile_cont(real[], real); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION percentile_cont(myarray real[], percentile real) RETURNS real
    LANGUAGE plpgsql IMMUTABLE
    AS $$

DECLARE
  ary_cnt INTEGER;
  row_num real;
  crn real;
  frn real;
  calc_result real;
  new_array real[];
BEGIN
  ary_cnt = array_length(myarray,1);
  row_num = 1 + ( percentile * ( ary_cnt - 1 ));
  new_array = array_sort(myarray);

  crn = ceiling(row_num);
  frn = floor(row_num);

  if crn = frn and frn = row_num then
    calc_result = new_array[row_num];
  else
    calc_result = (crn - row_num) * new_array[frn] 
            + (row_num - frn) * new_array[crn];
  end if;

  RETURN calc_result;
END;
$$;


ALTER FUNCTION public.percentile_cont(myarray real[], percentile real) OWNER TO postgres;

--
-- Name: pointfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION pointfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = 'POINT'
	THEN GeomFromText($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.pointfromtext(text) OWNER TO postgres;

--
-- Name: pointfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION pointfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = 'POINT'
	THEN GeomFromText($1,$2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.pointfromtext(text, integer) OWNER TO postgres;

--
-- Name: pointfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION pointfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'POINT'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.pointfromwkb(bytea) OWNER TO postgres;

--
-- Name: pointfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION pointfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = 'POINT'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.pointfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: polyfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION polyfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1)) = 'POLYGON'
	THEN GeomFromText($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.polyfromtext(text) OWNER TO postgres;

--
-- Name: polyfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION polyfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromText($1, $2)) = 'POLYGON'
	THEN GeomFromText($1,$2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.polyfromtext(text, integer) OWNER TO postgres;

--
-- Name: polyfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION polyfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'POLYGON'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.polyfromwkb(bytea) OWNER TO postgres;

--
-- Name: polyfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION polyfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1, $2)) = 'POLYGON'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.polyfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: polygonfromtext(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION polygonfromtext(text) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT PolyFromText($1)$_$;


ALTER FUNCTION public.polygonfromtext(text) OWNER TO postgres;

--
-- Name: polygonfromtext(text, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION polygonfromtext(text, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT PolyFromText($1, $2)$_$;


ALTER FUNCTION public.polygonfromtext(text, integer) OWNER TO postgres;

--
-- Name: polygonfromwkb(bytea); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION polygonfromwkb(bytea) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1)) = 'POLYGON'
	THEN GeomFromWKB($1)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.polygonfromwkb(bytea) OWNER TO postgres;

--
-- Name: polygonfromwkb(bytea, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION polygonfromwkb(bytea, integer) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$
	SELECT CASE WHEN geometrytype(GeomFromWKB($1,$2)) = 'POLYGON'
	THEN GeomFromWKB($1, $2)
	ELSE NULL END
	$_$;


ALTER FUNCTION public.polygonfromwkb(bytea, integer) OWNER TO postgres;

--
-- Name: populate_geometry_columns(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION populate_geometry_columns() RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
	inserted    integer;
	oldcount    integer;
	probed      integer;
	stale       integer;
	gcs         RECORD;
	gc          RECORD;
	gsrid       integer;
	gndims      integer;
	gtype       text;
	query       text;
	gc_is_valid boolean;

BEGIN
	SELECT count(*) INTO oldcount FROM geometry_columns;
	inserted := 0;

	EXECUTE 'TRUNCATE geometry_columns';

	-- Count the number of geometry columns in all tables and views
	SELECT count(DISTINCT c.oid) INTO probed
	FROM pg_class c,
		 pg_attribute a,
		 pg_type t,
		 pg_namespace n
	WHERE (c.relkind = 'r' OR c.relkind = 'v')
	AND t.typname = 'geometry'
	AND a.attisdropped = false
	AND a.atttypid = t.oid
	AND a.attrelid = c.oid
	AND c.relnamespace = n.oid
	AND n.nspname NOT ILIKE 'pg_temp%';

	-- Iterate through all non-dropped geometry columns
	RAISE DEBUG 'Processing Tables.....';

	FOR gcs IN
	SELECT DISTINCT ON (c.oid) c.oid, n.nspname, c.relname
		FROM pg_class c,
			 pg_attribute a,
			 pg_type t,
			 pg_namespace n
		WHERE c.relkind = 'r'
		AND t.typname = 'geometry'
		AND a.attisdropped = false
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid
		AND c.relnamespace = n.oid
		AND n.nspname NOT ILIKE 'pg_temp%'
	LOOP

	inserted := inserted + populate_geometry_columns(gcs.oid);
	END LOOP;

	-- Add views to geometry columns table
	RAISE DEBUG 'Processing Views.....';
	FOR gcs IN
	SELECT DISTINCT ON (c.oid) c.oid, n.nspname, c.relname
		FROM pg_class c,
			 pg_attribute a,
			 pg_type t,
			 pg_namespace n
		WHERE c.relkind = 'v'
		AND t.typname = 'geometry'
		AND a.attisdropped = false
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid
		AND c.relnamespace = n.oid
	LOOP

	inserted := inserted + populate_geometry_columns(gcs.oid);
	END LOOP;

	IF oldcount > inserted THEN
	stale = oldcount-inserted;
	ELSE
	stale = 0;
	END IF;

	RETURN 'probed:' ||probed|| ' inserted:'||inserted|| ' conflicts:'||probed-inserted|| ' deleted:'||stale;
END

$$;


ALTER FUNCTION public.populate_geometry_columns() OWNER TO postgres;

--
-- Name: FUNCTION populate_geometry_columns(); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION populate_geometry_columns() IS 'Ensures geometry columns have appropriate spatial constraints and exist in the geometry_columns table.';


--
-- Name: populate_geometry_columns(oid); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION populate_geometry_columns(tbl_oid oid) RETURNS integer
    LANGUAGE plpgsql
    AS $$
DECLARE
	gcs         RECORD;
	gc          RECORD;
	gsrid       integer;
	gndims      integer;
	gtype       text;
	query       text;
	gc_is_valid boolean;
	inserted    integer;

BEGIN
	inserted := 0;

	-- Iterate through all geometry columns in this table
	FOR gcs IN
	SELECT n.nspname, c.relname, a.attname
		FROM pg_class c,
			 pg_attribute a,
			 pg_type t,
			 pg_namespace n
		WHERE c.relkind = 'r'
		AND t.typname = 'geometry'
		AND a.attisdropped = false
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid
		AND c.relnamespace = n.oid
		AND n.nspname NOT ILIKE 'pg_temp%'
		AND c.oid = tbl_oid
	LOOP

	RAISE DEBUG 'Processing table %.%.%', gcs.nspname, gcs.relname, gcs.attname;

	DELETE FROM geometry_columns
	  WHERE f_table_schema = quote_ident(gcs.nspname)
	  AND f_table_name = quote_ident(gcs.relname)
	  AND f_geometry_column = quote_ident(gcs.attname);

	gc_is_valid := true;

	-- Try to find srid check from system tables (pg_constraint)
	gsrid :=
		(SELECT replace(replace(split_part(s.consrc, ' = ', 2), ')', ''), '(', '')
		 FROM pg_class c, pg_namespace n, pg_attribute a, pg_constraint s
		 WHERE n.nspname = gcs.nspname
		 AND c.relname = gcs.relname
		 AND a.attname = gcs.attname
		 AND a.attrelid = c.oid
		 AND s.connamespace = n.oid
		 AND s.conrelid = c.oid
		 AND a.attnum = ANY (s.conkey)
		 AND s.consrc LIKE '%srid(% = %');
	IF (gsrid IS NULL) THEN
		-- Try to find srid from the geometry itself
		EXECUTE 'SELECT srid(' || quote_ident(gcs.attname) || ')
				 FROM ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
				 WHERE ' || quote_ident(gcs.attname) || ' IS NOT NULL LIMIT 1'
			INTO gc;
		gsrid := gc.srid;

		-- Try to apply srid check to column
		IF (gsrid IS NOT NULL) THEN
			BEGIN
				EXECUTE 'ALTER TABLE ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
						 ADD CONSTRAINT ' || quote_ident('enforce_srid_' || gcs.attname) || '
						 CHECK (srid(' || quote_ident(gcs.attname) || ') = ' || gsrid || ')';
			EXCEPTION
				WHEN check_violation THEN
					RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not apply constraint CHECK (srid(%) = %)', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname), quote_ident(gcs.attname), gsrid;
					gc_is_valid := false;
			END;
		END IF;
	END IF;

	-- Try to find ndims check from system tables (pg_constraint)
	gndims :=
		(SELECT replace(split_part(s.consrc, ' = ', 2), ')', '')
		 FROM pg_class c, pg_namespace n, pg_attribute a, pg_constraint s
		 WHERE n.nspname = gcs.nspname
		 AND c.relname = gcs.relname
		 AND a.attname = gcs.attname
		 AND a.attrelid = c.oid
		 AND s.connamespace = n.oid
		 AND s.conrelid = c.oid
		 AND a.attnum = ANY (s.conkey)
		 AND s.consrc LIKE '%ndims(% = %');
	IF (gndims IS NULL) THEN
		-- Try to find ndims from the geometry itself
		EXECUTE 'SELECT ndims(' || quote_ident(gcs.attname) || ')
				 FROM ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
				 WHERE ' || quote_ident(gcs.attname) || ' IS NOT NULL LIMIT 1'
			INTO gc;
		gndims := gc.ndims;

		-- Try to apply ndims check to column
		IF (gndims IS NOT NULL) THEN
			BEGIN
				EXECUTE 'ALTER TABLE ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
						 ADD CONSTRAINT ' || quote_ident('enforce_dims_' || gcs.attname) || '
						 CHECK (ndims(' || quote_ident(gcs.attname) || ') = '||gndims||')';
			EXCEPTION
				WHEN check_violation THEN
					RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not apply constraint CHECK (ndims(%) = %)', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname), quote_ident(gcs.attname), gndims;
					gc_is_valid := false;
			END;
		END IF;
	END IF;

	-- Try to find geotype check from system tables (pg_constraint)
	gtype :=
		(SELECT replace(split_part(s.consrc, '''', 2), ')', '')
		 FROM pg_class c, pg_namespace n, pg_attribute a, pg_constraint s
		 WHERE n.nspname = gcs.nspname
		 AND c.relname = gcs.relname
		 AND a.attname = gcs.attname
		 AND a.attrelid = c.oid
		 AND s.connamespace = n.oid
		 AND s.conrelid = c.oid
		 AND a.attnum = ANY (s.conkey)
		 AND s.consrc LIKE '%geometrytype(% = %');
	IF (gtype IS NULL) THEN
		-- Try to find geotype from the geometry itself
		EXECUTE 'SELECT geometrytype(' || quote_ident(gcs.attname) || ')
				 FROM ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
				 WHERE ' || quote_ident(gcs.attname) || ' IS NOT NULL LIMIT 1'
			INTO gc;
		gtype := gc.geometrytype;
		--IF (gtype IS NULL) THEN
		--    gtype := 'GEOMETRY';
		--END IF;

		-- Try to apply geometrytype check to column
		IF (gtype IS NOT NULL) THEN
			BEGIN
				EXECUTE 'ALTER TABLE ONLY ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
				ADD CONSTRAINT ' || quote_ident('enforce_geotype_' || gcs.attname) || '
				CHECK ((geometrytype(' || quote_ident(gcs.attname) || ') = ' || quote_literal(gtype) || ') OR (' || quote_ident(gcs.attname) || ' IS NULL))';
			EXCEPTION
				WHEN check_violation THEN
					-- No geometry check can be applied. This column contains a number of geometry types.
					RAISE WARNING 'Could not add geometry type check (%) to table column: %.%.%', gtype, quote_ident(gcs.nspname),quote_ident(gcs.relname),quote_ident(gcs.attname);
			END;
		END IF;
	END IF;

	IF (gsrid IS NULL) THEN
		RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not determine the srid', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname);
	ELSIF (gndims IS NULL) THEN
		RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not determine the number of dimensions', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname);
	ELSIF (gtype IS NULL) THEN
		RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not determine the geometry type', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname);
	ELSE
		-- Only insert into geometry_columns if table constraints could be applied.
		IF (gc_is_valid) THEN
			INSERT INTO geometry_columns (f_table_catalog,f_table_schema, f_table_name, f_geometry_column, coord_dimension, srid, type)
			VALUES ('', gcs.nspname, gcs.relname, gcs.attname, gndims, gsrid, gtype);
			inserted := inserted + 1;
		END IF;
	END IF;
	END LOOP;

	-- Add views to geometry columns table
	FOR gcs IN
	SELECT n.nspname, c.relname, a.attname
		FROM pg_class c,
			 pg_attribute a,
			 pg_type t,
			 pg_namespace n
		WHERE c.relkind = 'v'
		AND t.typname = 'geometry'
		AND a.attisdropped = false
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid
		AND c.relnamespace = n.oid
		AND n.nspname NOT ILIKE 'pg_temp%'
		AND c.oid = tbl_oid
	LOOP
		RAISE DEBUG 'Processing view %.%.%', gcs.nspname, gcs.relname, gcs.attname;

		EXECUTE 'SELECT ndims(' || quote_ident(gcs.attname) || ')
				 FROM ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
				 WHERE ' || quote_ident(gcs.attname) || ' IS NOT NULL LIMIT 1'
			INTO gc;
		gndims := gc.ndims;

		EXECUTE 'SELECT srid(' || quote_ident(gcs.attname) || ')
				 FROM ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
				 WHERE ' || quote_ident(gcs.attname) || ' IS NOT NULL LIMIT 1'
			INTO gc;
		gsrid := gc.srid;

		EXECUTE 'SELECT geometrytype(' || quote_ident(gcs.attname) || ')
				 FROM ' || quote_ident(gcs.nspname) || '.' || quote_ident(gcs.relname) || '
				 WHERE ' || quote_ident(gcs.attname) || ' IS NOT NULL LIMIT 1'
			INTO gc;
		gtype := gc.geometrytype;

		IF (gndims IS NULL) THEN
			RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not determine ndims', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname);
		ELSIF (gsrid IS NULL) THEN
			RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not determine srid', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname);
		ELSIF (gtype IS NULL) THEN
			RAISE WARNING 'Not inserting ''%'' in ''%.%'' into geometry_columns: could not determine gtype', quote_ident(gcs.attname), quote_ident(gcs.nspname), quote_ident(gcs.relname);
		ELSE
			query := 'INSERT INTO geometry_columns (f_table_catalog,f_table_schema, f_table_name, f_geometry_column, coord_dimension, srid, type) ' ||
					 'VALUES ('''', ' || quote_literal(gcs.nspname) || ',' || quote_literal(gcs.relname) || ',' || quote_literal(gcs.attname) || ',' || gndims || ',' || gsrid || ',' || quote_literal(gtype) || ')';
			EXECUTE query;
			inserted := inserted + 1;
		END IF;
	END LOOP;

	RETURN inserted;
END

$$;


ALTER FUNCTION public.populate_geometry_columns(tbl_oid oid) OWNER TO postgres;

--
-- Name: FUNCTION populate_geometry_columns(tbl_oid oid); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION populate_geometry_columns(tbl_oid oid) IS 'args: relation_oid - Ensures geometry columns have appropriate spatial constraints and exist in the geometry_columns table.';


--
-- Name: probe_geometry_columns(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION probe_geometry_columns() RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
	inserted integer;
	oldcount integer;
	probed integer;
	stale integer;
BEGIN

	SELECT count(*) INTO oldcount FROM geometry_columns;

	SELECT count(*) INTO probed
		FROM pg_class c, pg_attribute a, pg_type t,
			pg_namespace n,
			pg_constraint sridcheck, pg_constraint typecheck

		WHERE t.typname = 'geometry'
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid
		AND c.relnamespace = n.oid
		AND sridcheck.connamespace = n.oid
		AND typecheck.connamespace = n.oid
		AND sridcheck.conrelid = c.oid
		AND sridcheck.consrc LIKE '(srid('||a.attname||') = %)'
		AND typecheck.conrelid = c.oid
		AND typecheck.consrc LIKE
		'((geometrytype('||a.attname||') = ''%''::text) OR (% IS NULL))'
		;

	INSERT INTO geometry_columns SELECT
		''::varchar as f_table_catalogue,
		n.nspname::varchar as f_table_schema,
		c.relname::varchar as f_table_name,
		a.attname::varchar as f_geometry_column,
		2 as coord_dimension,
		trim(both  ' =)' from
			replace(replace(split_part(
				sridcheck.consrc, ' = ', 2), ')', ''), '(', ''))::integer AS srid,
		trim(both ' =)''' from substr(typecheck.consrc,
			strpos(typecheck.consrc, '='),
			strpos(typecheck.consrc, '::')-
			strpos(typecheck.consrc, '=')
			))::varchar as type
		FROM pg_class c, pg_attribute a, pg_type t,
			pg_namespace n,
			pg_constraint sridcheck, pg_constraint typecheck
		WHERE t.typname = 'geometry'
		AND a.atttypid = t.oid
		AND a.attrelid = c.oid
		AND c.relnamespace = n.oid
		AND sridcheck.connamespace = n.oid
		AND typecheck.connamespace = n.oid
		AND sridcheck.conrelid = c.oid
		AND sridcheck.consrc LIKE '(st_srid('||a.attname||') = %)'
		AND typecheck.conrelid = c.oid
		AND typecheck.consrc LIKE
		'((geometrytype('||a.attname||') = ''%''::text) OR (% IS NULL))'

			AND NOT EXISTS (
					SELECT oid FROM geometry_columns gc
					WHERE c.relname::varchar = gc.f_table_name
					AND n.nspname::varchar = gc.f_table_schema
					AND a.attname::varchar = gc.f_geometry_column
			);

	GET DIAGNOSTICS inserted = ROW_COUNT;

	IF oldcount > probed THEN
		stale = oldcount-probed;
	ELSE
		stale = 0;
	END IF;

	RETURN 'probed:'||probed::text||
		' inserted:'||inserted::text||
		' conflicts:'||(probed-inserted)::text||
		' stale:'||stale::text;
END

$$;


ALTER FUNCTION public.probe_geometry_columns() OWNER TO postgres;

--
-- Name: FUNCTION probe_geometry_columns(); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION probe_geometry_columns() IS 'Scans all tables with PostGIS geometry constraints and adds them to the geometry_columns table if they are not there.';


--
-- Name: rename_geometry_table_constraints(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION rename_geometry_table_constraints() RETURNS text
    LANGUAGE sql IMMUTABLE
    AS $$
SELECT 'rename_geometry_table_constraint() is obsoleted'::text
$$;


ALTER FUNCTION public.rename_geometry_table_constraints() OWNER TO postgres;

--
-- Name: rotate(geometry, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION rotate(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT rotateZ($1, $2)$_$;


ALTER FUNCTION public.rotate(geometry, double precision) OWNER TO postgres;

--
-- Name: rotatex(geometry, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION rotatex(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT affine($1, 1, 0, 0, 0, cos($2), -sin($2), 0, sin($2), cos($2), 0, 0, 0)$_$;


ALTER FUNCTION public.rotatex(geometry, double precision) OWNER TO postgres;

--
-- Name: rotatey(geometry, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION rotatey(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT affine($1,  cos($2), 0, sin($2),  0, 1, 0,  -sin($2), 0, cos($2), 0,  0, 0)$_$;


ALTER FUNCTION public.rotatey(geometry, double precision) OWNER TO postgres;

--
-- Name: rotatez(geometry, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION rotatez(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT affine($1,  cos($2), -sin($2), 0,  sin($2), cos($2), 0,  0, 0, 1,  0, 0, 0)$_$;


ALTER FUNCTION public.rotatez(geometry, double precision) OWNER TO postgres;

--
-- Name: scale(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION scale(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT scale($1, $2, $3, 1)$_$;


ALTER FUNCTION public.scale(geometry, double precision, double precision) OWNER TO postgres;

--
-- Name: scale(geometry, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION scale(geometry, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT affine($1,  $2, 0, 0,  0, $3, 0,  0, 0, $4,  0, 0, 0)$_$;


ALTER FUNCTION public.scale(geometry, double precision, double precision, double precision) OWNER TO postgres;

--
-- Name: se_envelopesintersect(geometry, geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION se_envelopesintersect(geometry, geometry) RETURNS boolean
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ 
	SELECT $1 && $2
	$_$;


ALTER FUNCTION public.se_envelopesintersect(geometry, geometry) OWNER TO postgres;

--
-- Name: se_locatealong(geometry, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION se_locatealong(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT locate_between_measures($1, $2, $2) $_$;


ALTER FUNCTION public.se_locatealong(geometry, double precision) OWNER TO postgres;

--
-- Name: snaptogrid(geometry, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION snaptogrid(geometry, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT SnapToGrid($1, 0, 0, $2, $2)$_$;


ALTER FUNCTION public.snaptogrid(geometry, double precision) OWNER TO postgres;

--
-- Name: snaptogrid(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION snaptogrid(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT SnapToGrid($1, 0, 0, $2, $3)$_$;


ALTER FUNCTION public.snaptogrid(geometry, double precision, double precision) OWNER TO postgres;

--
-- Name: st_area(geography); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_area(geography) RETURNS double precision
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_Area($1, true)$_$;


ALTER FUNCTION public.st_area(geography) OWNER TO postgres;

--
-- Name: FUNCTION st_area(geography); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_area(geography) IS 'args: g1 - Returns the area of the surface if it is a polygon or multi-polygon. For "geometry" type area is in SRID units. For "geography" area is in square meters.';


--
-- Name: st_asbinary(text); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asbinary(text) RETURNS bytea
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$ SELECT ST_AsBinary($1::geometry);  $_$;


ALTER FUNCTION public.st_asbinary(text) OWNER TO postgres;

--
-- Name: st_asgeojson(geography); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgeojson(geography) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGeoJson(1, $1, 15, 0)$_$;


ALTER FUNCTION public.st_asgeojson(geography) OWNER TO postgres;

--
-- Name: FUNCTION st_asgeojson(geography); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgeojson(geography) IS 'args: g1 - Return the geometry as a GeoJSON element.';


--
-- Name: st_asgeojson(geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgeojson(geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGeoJson(1, $1, 15, 0)$_$;


ALTER FUNCTION public.st_asgeojson(geometry) OWNER TO postgres;

--
-- Name: FUNCTION st_asgeojson(geometry); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgeojson(geometry) IS 'args: g1 - Return the geometry as a GeoJSON element.';


--
-- Name: st_asgeojson(integer, geography); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgeojson(integer, geography) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGeoJson($1, $2, 15, 0)$_$;


ALTER FUNCTION public.st_asgeojson(integer, geography) OWNER TO postgres;

--
-- Name: FUNCTION st_asgeojson(integer, geography); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgeojson(integer, geography) IS 'args: version, g1 - Return the geometry as a GeoJSON element.';


--
-- Name: st_asgeojson(integer, geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgeojson(integer, geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGeoJson($1, $2, 15, 0)$_$;


ALTER FUNCTION public.st_asgeojson(integer, geometry) OWNER TO postgres;

--
-- Name: FUNCTION st_asgeojson(integer, geometry); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgeojson(integer, geometry) IS 'args: version, g1 - Return the geometry as a GeoJSON element.';


--
-- Name: st_asgeojson(geography, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgeojson(geography, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGeoJson(1, $1, $2, 0)$_$;


ALTER FUNCTION public.st_asgeojson(geography, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgeojson(geography, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgeojson(geography, integer) IS 'args: g1, max_decimal_digits - Return the geometry as a GeoJSON element.';


--
-- Name: st_asgeojson(geometry, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgeojson(geometry, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGeoJson(1, $1, $2, 0)$_$;


ALTER FUNCTION public.st_asgeojson(geometry, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgeojson(geometry, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgeojson(geometry, integer) IS 'args: g1, max_decimal_digits - Return the geometry as a GeoJSON element.';


--
-- Name: st_asgeojson(integer, geography, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgeojson(integer, geography, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGeoJson($1, $2, $3, 0)$_$;


ALTER FUNCTION public.st_asgeojson(integer, geography, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgeojson(integer, geography, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgeojson(integer, geography, integer) IS 'args: version, g1, max_decimal_digits - Return the geometry as a GeoJSON element.';


--
-- Name: st_asgeojson(integer, geometry, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgeojson(integer, geometry, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGeoJson($1, $2, $3, 0)$_$;


ALTER FUNCTION public.st_asgeojson(integer, geometry, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgeojson(integer, geometry, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgeojson(integer, geometry, integer) IS 'args: version, g1, max_decimal_digits - Return the geometry as a GeoJSON element.';


--
-- Name: st_asgml(geography); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(geography) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML(2, $1, 15, 0)$_$;


ALTER FUNCTION public.st_asgml(geography) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(geography); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(geography) IS 'args: g1 - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_asgml(geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML(2, $1, 15, 0)$_$;


ALTER FUNCTION public.st_asgml(geometry) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(geometry); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(geometry) IS 'args: g1 - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_asgml(integer, geography); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(integer, geography) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML($1, $2, 15, 0)$_$;


ALTER FUNCTION public.st_asgml(integer, geography) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(integer, geography); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(integer, geography) IS 'args: version, g1 - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_asgml(integer, geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(integer, geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML($1, $2, 15, 0)$_$;


ALTER FUNCTION public.st_asgml(integer, geometry) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(integer, geometry); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(integer, geometry) IS 'args: version, g1 - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_asgml(geography, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(geography, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML(2, $1, $2, 0)$_$;


ALTER FUNCTION public.st_asgml(geography, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(geography, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(geography, integer) IS 'args: g1, precision - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_asgml(geometry, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(geometry, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML(2, $1, $2, 0)$_$;


ALTER FUNCTION public.st_asgml(geometry, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(geometry, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(geometry, integer) IS 'args: g1, precision - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_asgml(integer, geography, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(integer, geography, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML($1, $2, $3, 0)$_$;


ALTER FUNCTION public.st_asgml(integer, geography, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(integer, geography, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(integer, geography, integer) IS 'args: version, g1, precision - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_asgml(integer, geometry, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(integer, geometry, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML($1, $2, $3, 0)$_$;


ALTER FUNCTION public.st_asgml(integer, geometry, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(integer, geometry, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(integer, geometry, integer) IS 'args: version, g1, precision - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_asgml(integer, geography, integer, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(integer, geography, integer, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML($1, $2, $3, $4)$_$;


ALTER FUNCTION public.st_asgml(integer, geography, integer, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(integer, geography, integer, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(integer, geography, integer, integer) IS 'args: version, g1, precision, options - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_asgml(integer, geometry, integer, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_asgml(integer, geometry, integer, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsGML($1, $2, $3, $4)$_$;


ALTER FUNCTION public.st_asgml(integer, geometry, integer, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_asgml(integer, geometry, integer, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_asgml(integer, geometry, integer, integer) IS 'args: version, g1, precision, options - Return the geometry as a GML version 2 or 3 element.';


--
-- Name: st_askml(geography); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_askml(geography) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML(2, $1, 15)$_$;


ALTER FUNCTION public.st_askml(geography) OWNER TO postgres;

--
-- Name: FUNCTION st_askml(geography); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_askml(geography) IS 'args: g1 - Return the geometry as a KML element. Several variants. Default version=2, default precision=15';


--
-- Name: st_askml(geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_askml(geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML(2, ST_Transform($1,4326), 15)$_$;


ALTER FUNCTION public.st_askml(geometry) OWNER TO postgres;

--
-- Name: FUNCTION st_askml(geometry); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_askml(geometry) IS 'args: g1 - Return the geometry as a KML element. Several variants. Default version=2, default precision=15';


--
-- Name: st_askml(integer, geography); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_askml(integer, geography) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML($1, $2, 15)$_$;


ALTER FUNCTION public.st_askml(integer, geography) OWNER TO postgres;

--
-- Name: FUNCTION st_askml(integer, geography); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_askml(integer, geography) IS 'args: version, geom1 - Return the geometry as a KML element. Several variants. Default version=2, default precision=15';


--
-- Name: st_askml(integer, geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_askml(integer, geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML($1, ST_Transform($2,4326), 15)$_$;


ALTER FUNCTION public.st_askml(integer, geometry) OWNER TO postgres;

--
-- Name: FUNCTION st_askml(integer, geometry); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_askml(integer, geometry) IS 'args: version, geom1 - Return the geometry as a KML element. Several variants. Default version=2, default precision=15';


--
-- Name: st_askml(integer, geography, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_askml(integer, geography, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML($1, $2, $3)$_$;


ALTER FUNCTION public.st_askml(integer, geography, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_askml(integer, geography, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_askml(integer, geography, integer) IS 'args: version, geom1, precision - Return the geometry as a KML element. Several variants. Default version=2, default precision=15';


--
-- Name: st_askml(integer, geometry, integer); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_askml(integer, geometry, integer) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT _ST_AsKML($1, ST_Transform($2,4326), $3)$_$;


ALTER FUNCTION public.st_askml(integer, geometry, integer) OWNER TO postgres;

--
-- Name: FUNCTION st_askml(integer, geometry, integer); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_askml(integer, geometry, integer) IS 'args: version, geom1, precision - Return the geometry as a KML element. Several variants. Default version=2, default precision=15';


--
-- Name: st_geohash(geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_geohash(geometry) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_GeoHash($1, 0)$_$;


ALTER FUNCTION public.st_geohash(geometry) OWNER TO postgres;

--
-- Name: FUNCTION st_geohash(geometry); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_geohash(geometry) IS 'args: g1 - Return a GeoHash representation (geohash.org) of the geometry.';


--
-- Name: st_length(geography); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_length(geography) RETURNS double precision
    LANGUAGE sql IMMUTABLE
    AS $_$SELECT ST_Length($1, true)$_$;


ALTER FUNCTION public.st_length(geography) OWNER TO postgres;

--
-- Name: FUNCTION st_length(geography); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_length(geography) IS 'args: gg - Returns the 2d length of the geometry if it is a linestring or multilinestring. geometry are in units of spatial reference and geography are in meters (default spheroid)';


--
-- Name: st_minimumboundingcircle(geometry); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION st_minimumboundingcircle(geometry) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT ST_MinimumBoundingCircle($1, 48)$_$;


ALTER FUNCTION public.st_minimumboundingcircle(geometry) OWNER TO postgres;

--
-- Name: FUNCTION st_minimumboundingcircle(geometry); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON FUNCTION st_minimumboundingcircle(geometry) IS 'args: geomA - Returns the smallest circle polygon that can fully contain a geometry. Default uses 48 segments per quarter circle.';


--
-- Name: translate(geometry, double precision, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION translate(geometry, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT translate($1, $2, $3, 0)$_$;


ALTER FUNCTION public.translate(geometry, double precision, double precision) OWNER TO postgres;

--
-- Name: translate(geometry, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION translate(geometry, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT affine($1, 1, 0, 0, 0, 1, 0, 0, 0, 1, $2, $3, $4)$_$;


ALTER FUNCTION public.translate(geometry, double precision, double precision, double precision) OWNER TO postgres;

--
-- Name: transscale(geometry, double precision, double precision, double precision, double precision); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION transscale(geometry, double precision, double precision, double precision, double precision) RETURNS geometry
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT affine($1,  $4, 0, 0,  0, $5, 0,
		0, 0, 1,  $2 * $4, $3 * $5, 0)$_$;


ALTER FUNCTION public.transscale(geometry, double precision, double precision, double precision, double precision) OWNER TO postgres;

--
-- Name: update_geometry_stats(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION update_geometry_stats() RETURNS text
    LANGUAGE sql
    AS $$ SELECT 'update_geometry_stats() has been obsoleted. Statistics are automatically built running the ANALYZE command'::text$$;


ALTER FUNCTION public.update_geometry_stats() OWNER TO postgres;

--
-- Name: update_geometry_stats(character varying, character varying); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION update_geometry_stats(character varying, character varying) RETURNS text
    LANGUAGE sql
    AS $$SELECT update_geometry_stats();$$;


ALTER FUNCTION public.update_geometry_stats(character varying, character varying) OWNER TO postgres;

SET search_path = ref, pg_catalog;

--
-- Name: compile(); Type: FUNCTION; Schema: ref; Owner: postgres
--

CREATE FUNCTION compile() RETURNS integer
    LANGUAGE plpgsql
    AS $$
	DECLARE
	linetablecount int;
	totalcount int;
	BEGIN	  
		-- écriture d'une table
		--	DROP TABLE IF EXISTS ref.count;
		--	CREATE TABLE nat.count(count integer,tablename character varying (25)) ;
		totalcount=0;
		SELECT INTO linetablecount  count(*) from iav.t_station_sta;			
		insert into nat.t_station_sta select * from iav.t_station_sta;	
		RAISE NOTICE 'inserting from iav.t_station_sta =%',linetablecount;
		totalcount=linetablecount+totalcount;
		SELECT INTO linetablecount  count(*) from iav.t_dispositiffranchissement_dif;
		insert into nat.t_dispositiffranchissement_dif select * from  iav.t_dispositiffranchissement_dif;	
		RAISE NOTICE 'inserting from iav.t_dispositiffranchissement_dif =%',linetablecount;		
		totalcount=linetablecount+totalcount;
		--insert into nat.count values (linetablecount,'t_station_sta');		 
	RETURN totalcount;
	END;
	$$;


ALTER FUNCTION ref.compile() OWNER TO postgres;

--
-- Name: compile(character varying); Type: FUNCTION; Schema: ref; Owner: postgres
--

CREATE FUNCTION compile(schemaname_ character varying) RETURNS SETOF integer
    LANGUAGE plpgsql
    AS $$
	BEGIN	    	
	    insert into nat.t_station_sta select * from schemaname_.t_station_sta;	
	    RAISE NOTICE 'inserting from' ,schemaname_;
	END;
	$$;


ALTER FUNCTION ref.compile(schemaname_ character varying) OWNER TO postgres;

--
-- Name: FUNCTION compile(schemaname_ character varying); Type: COMMENT; Schema: ref; Owner: postgres
--

COMMENT ON FUNCTION compile(schemaname_ character varying) IS 'fonction pour compiler le schema d''un operateur';


--
-- Name: fct_coe_date(); Type: FUNCTION; Schema: ref; Owner: postgres
--

CREATE FUNCTION fct_coe_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	DECLARE nbChevauchements INTEGER ;

 	BEGIN
 	 	-- verification des non-chevauchements pour les taux
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   tj_coefficientconversion_coe
 	 	WHERE  coe_tax_code = NEW.coe_tax_code
 	 	       AND coe_std_code = NEW.coe_std_code
 	 	       AND coe_qte_code = NEW.coe_qte_code
 	 	       AND (coe_date_debut, coe_date_fin) OVERLAPS (NEW.coe_date_debut, NEW.coe_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN
 	 	 	RAISE EXCEPTION 'Les taux de conversion ne peuvent se chevaucher.'  ;
 	 	END IF  ;

		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION ref.fct_coe_date() OWNER TO postgres;

--
-- Name: updatesql(character varying[], text); Type: FUNCTION; Schema: ref; Owner: postgres
--

CREATE FUNCTION updatesql(myschemas character varying[], scriptsql text) RETURNS integer
    LANGUAGE plpgsql
    AS $$
	DECLARE
	totalcount int;	
	nbschema int;
	i integer;
	BEGIN	  
		select INTO nbschema array_length(myschemas,1);
		i=1;
		While (i <=nbschema) LOOP
		EXECUTE 'SET search_path TO '||myschemas[i]||', public';
		RAISE INFO 'execute sql pour schema %',myschemas[i] ;
		EXECUTE scriptsql;		
		i=i+1;
		END LOOP;	
	RETURN nbschema;
	END;
	$$;


ALTER FUNCTION ref.updatesql(myschemas character varying[], scriptsql text) OWNER TO postgres;

--
-- Name: FUNCTION updatesql(myschemas character varying[], scriptsql text); Type: COMMENT; Schema: ref; Owner: postgres
--

COMMENT ON FUNCTION updatesql(myschemas character varying[], scriptsql text) IS 'fonction pour lancer un script de mise à jour sur chaque schema';


SET search_path = user_1, pg_catalog;

--
-- Name: do_not_delete_default(); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION do_not_delete_default() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   
DECLARE
_mas_code character varying(15);
    BEGIN      
        IF OLD.mas_code = 'ope_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas supprimer le masque par défaut';
        END IF;
          IF OLD.mas_code = 'lot_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas supprimer le masque par défaut';
        END IF;
RETURN OLD;
    END;
$$;


ALTER FUNCTION user_1.do_not_delete_default() OWNER TO postgres;

--
-- Name: do_not_update_default(); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION do_not_update_default() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   
DECLARE
_mas_code character varying(15);
    BEGIN      
     IF OLD.mas_code = 'ope_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas modifier le masque par défaut';
        END IF;
          IF OLD.mas_code = 'lot_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas modifier le masque par défaut';
        END IF;
RETURN NEW;
    END;
$$;


ALTER FUNCTION user_1.do_not_update_default() OWNER TO postgres;

--
-- Name: fct_coe_date(); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION fct_coe_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	DECLARE nbChevauchements INTEGER ;

 	BEGIN
 	 	-- verification des non-chevauchements pour les taux
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   iav.tj_coefficientconversion_coe
 	 	WHERE  coe_tax_code = NEW.coe_tax_code
 	 	       AND coe_std_code = NEW.coe_std_code
 	 	       AND coe_qte_code = NEW.coe_qte_code
 	 	       AND (coe_date_debut, coe_date_fin) OVERLAPS (NEW.coe_date_debut, NEW.coe_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN
 	 	 	RAISE EXCEPTION 'Les taux de conversion ne peuvent se chevaucher.'  ;
 	 	END IF  ;

		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION user_1.fct_coe_date() OWNER TO postgres;

--
-- Name: fct_ope_date(); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION fct_ope_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	DECLARE disCreation  	 	TIMESTAMP  ;
 	DECLARE disSuppression  	TIMESTAMP  ;
 	DECLARE nbChevauchements 	INTEGER    ;

 	BEGIN
 	 	-- Recuperation des dates du dispositif dans des variables
 	 	SELECT dis_date_creation, dis_date_suppression INTO disCreation, disSuppression
 	 	FROM   iav.tg_dispositif_dis
 	 	WHERE  dis_identifiant = NEW.ope_dic_identifiant
 	 	;

 	 	-- verification de la date de debut
 	 	IF ((NEW.ope_date_debut < disCreation) OR (NEW.ope_date_debut > disSuppression)) THEN
 	 	 	RAISE EXCEPTION 'Le debut de l operation doit etre inclus dans la periode d existence du DC.'  ;
 	 	END IF  ;

 	 	-- verification de la date de fin
 	 	IF ((NEW.ope_date_fin < disCreation) OR (NEW.ope_date_fin > disSuppression)) THEN
 	 	 	RAISE EXCEPTION 'La fin de l operation doit etre incluse dans la periode d existence du DC.'  ;
 	 	END IF  ;

 	 	-- verification des non-chevauchements pour les operations du dispositif
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   iav.t_operation_ope
 	 	WHERE  ope_dic_identifiant = NEW.ope_dic_identifiant 
 	 	       AND (ope_date_debut, ope_date_fin) OVERLAPS (NEW.ope_date_debut, NEW.ope_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN 
 	 		RAISE EXCEPTION 'Les operations ne peuvent se chevaucher.'  ;
 	 	END IF  ;

		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION user_1.fct_ope_date() OWNER TO postgres;

--
-- Name: fct_per_date(); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION fct_per_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$ 

 	DECLARE disCreation  	 	TIMESTAMP  ;
 	DECLARE disSuppression  	TIMESTAMP  ;
 	DECLARE nbChevauchements 	INTEGER    ;

 	BEGIN
 	 	-- Recuperation des dates du dispositif dans des variables
 	 	SELECT dis_date_creation, dis_date_suppression INTO disCreation, disSuppression
 	 	FROM   iav.tg_dispositif_dis
 	 	WHERE  dis_identifiant = NEW.per_dis_identifiant
 	 	;

 	 	-- verification de la date de debut
 	 	IF ((NEW.per_date_debut < disCreation) OR (NEW.per_date_debut > disSuppression)) THEN
 	 		RAISE EXCEPTION 'Le debut de la periode doit etre inclus dans la periode d existence du dispositif.'  ;
 	 	END IF  ;

 	 	-- verification de la date de fin
 	 	IF ((NEW.per_date_fin < disCreation) OR (NEW.per_date_fin > disSuppression)) THEN
 	 	 	RAISE EXCEPTION 'La fin de la periode doit etre incluse dans la periode d existence du dispositif.'  ;
 	 	END IF  ;

 	 	-- verification des non-chevauchements pour les periodes du dispositif
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   iav.t_periodefonctdispositif_per
 	 	WHERE  per_dis_identifiant = NEW.per_dis_identifiant 
 	 	       AND (per_date_debut, per_date_fin) OVERLAPS (NEW.per_date_debut, NEW.per_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN
 	 	 	RAISE EXCEPTION 'Les periodes ne peuvent se chevaucher.'  ;
 	 	END IF  ;
		
		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION user_1.fct_per_date() OWNER TO postgres;

--
-- Name: fct_per_suppression(); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION fct_per_suppression() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	BEGIN
 	 	-- La periode precedent celle supprimee est prolongee
 	 	-- jusqu a la fin de la periode supprimee
 	 	UPDATE iav.t_periodefonctdispositif_per 
 	 	SET    per_date_fin = OLD.per_date_fin 
 	 	WHERE  per_date_fin= OLD.per_date_debut 
 	 	       AND per_dis_identifiant = OLD.per_dis_identifiant 
 	 	;
		RETURN NEW ;
 	END  ;

$$;


ALTER FUNCTION user_1.fct_per_suppression() OWNER TO postgres;

--
-- Name: fct_txe_date(); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION fct_txe_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	DECLARE nbChevauchements INTEGER ;

 	BEGIN
 	 	-- verification des non-chevauchements pour les taux
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   iav.tj_tauxechappement_txe
 	 	WHERE  txe_ouv_identifiant = NEW.txe_ouv_identifiant
 	 	       AND txe_tax_code = NEW.txe_tax_code
 	 	       AND txe_std_code = NEW.txe_std_code
 	 	       AND (txe_date_debut, txe_date_fin) OVERLAPS (NEW.txe_date_debut, NEW.txe_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN
 	 	 	RAISE EXCEPTION 'Les taux d echappement ne peuvent se chevaucher.'  ;
 	 	END IF  ;

		RETURN NEW ;

 	END  ;
$$;


ALTER FUNCTION user_1.fct_txe_date() OWNER TO postgres;

--
-- Name: masqueope(); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION masqueope() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   
DECLARE test text;


 	BEGIN
 	 	-- Recuperation du type
 	 	SELECT mas_type into test
 	 	FROM   iav.ts_masque_mas
 	 	WHERE  mas_id = NEW.mao_mas_id
 	 	;

 	 	-- verification 
 	 	IF (test!='ope') THEN
 	 	 	RAISE EXCEPTION 'Attention le masque n'' est pas un masque operation, changez le type de masque'  ;
 	 	END IF  ; 	 	

		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION user_1.masqueope() OWNER TO postgres;

--
-- Name: plpgsql_call_handler(); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION plpgsql_call_handler() RETURNS language_handler
    LANGUAGE c
    AS '$libdir/plpgsql', 'plpgsql_call_handler';


ALTER FUNCTION user_1.plpgsql_call_handler() OWNER TO postgres;

--
-- Name: xpath_list(text, text); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION xpath_list(text, text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT xpath_list($1,$2,',')$_$;


ALTER FUNCTION user_1.xpath_list(text, text) OWNER TO postgres;

--
-- Name: xpath_nodeset(text, text); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION xpath_nodeset(text, text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT xpath_nodeset($1,$2,'','')$_$;


ALTER FUNCTION user_1.xpath_nodeset(text, text) OWNER TO postgres;

--
-- Name: xpath_nodeset(text, text, text); Type: FUNCTION; Schema: user_1; Owner: postgres
--

CREATE FUNCTION xpath_nodeset(text, text, text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT xpath_nodeset($1,$2,'',$3)$_$;


ALTER FUNCTION user_1.xpath_nodeset(text, text, text) OWNER TO postgres;

SET search_path = user_2, pg_catalog;

--
-- Name: do_not_delete_default(); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION do_not_delete_default() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   
DECLARE
_mas_code character varying(15);
    BEGIN      
        IF OLD.mas_code = 'ope_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas supprimer le masque par défaut';
        END IF;
          IF OLD.mas_code = 'lot_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas supprimer le masque par défaut';
        END IF;
RETURN OLD;
    END;
$$;


ALTER FUNCTION user_2.do_not_delete_default() OWNER TO postgres;

--
-- Name: do_not_update_default(); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION do_not_update_default() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   
DECLARE
_mas_code character varying(15);
    BEGIN      
     IF OLD.mas_code = 'ope_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas modifier le masque par défaut';
        END IF;
          IF OLD.mas_code = 'lot_defaut'  THEN
            RAISE EXCEPTION 'Vous ne pouvez pas modifier le masque par défaut';
        END IF;
RETURN NEW;
    END;
$$;


ALTER FUNCTION user_2.do_not_update_default() OWNER TO postgres;

--
-- Name: fct_coe_date(); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION fct_coe_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	DECLARE nbChevauchements INTEGER ;

 	BEGIN
 	 	-- verification des non-chevauchements pour les taux
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   iav.tj_coefficientconversion_coe
 	 	WHERE  coe_tax_code = NEW.coe_tax_code
 	 	       AND coe_std_code = NEW.coe_std_code
 	 	       AND coe_qte_code = NEW.coe_qte_code
 	 	       AND (coe_date_debut, coe_date_fin) OVERLAPS (NEW.coe_date_debut, NEW.coe_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN
 	 	 	RAISE EXCEPTION 'Les taux de conversion ne peuvent se chevaucher.'  ;
 	 	END IF  ;

		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION user_2.fct_coe_date() OWNER TO postgres;

--
-- Name: fct_ope_date(); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION fct_ope_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	DECLARE disCreation  	 	TIMESTAMP  ;
 	DECLARE disSuppression  	TIMESTAMP  ;
 	DECLARE nbChevauchements 	INTEGER    ;

 	BEGIN
 	 	-- Recuperation des dates du dispositif dans des variables
 	 	SELECT dis_date_creation, dis_date_suppression INTO disCreation, disSuppression
 	 	FROM   iav.tg_dispositif_dis
 	 	WHERE  dis_identifiant = NEW.ope_dic_identifiant
 	 	;

 	 	-- verification de la date de debut
 	 	IF ((NEW.ope_date_debut < disCreation) OR (NEW.ope_date_debut > disSuppression)) THEN
 	 	 	RAISE EXCEPTION 'Le debut de l operation doit etre inclus dans la periode d existence du DC.'  ;
 	 	END IF  ;

 	 	-- verification de la date de fin
 	 	IF ((NEW.ope_date_fin < disCreation) OR (NEW.ope_date_fin > disSuppression)) THEN
 	 	 	RAISE EXCEPTION 'La fin de l operation doit etre incluse dans la periode d existence du DC.'  ;
 	 	END IF  ;

 	 	-- verification des non-chevauchements pour les operations du dispositif
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   iav.t_operation_ope
 	 	WHERE  ope_dic_identifiant = NEW.ope_dic_identifiant 
 	 	       AND (ope_date_debut, ope_date_fin) OVERLAPS (NEW.ope_date_debut, NEW.ope_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN 
 	 		RAISE EXCEPTION 'Les operations ne peuvent se chevaucher.'  ;
 	 	END IF  ;

		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION user_2.fct_ope_date() OWNER TO postgres;

--
-- Name: fct_per_date(); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION fct_per_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$ 

 	DECLARE disCreation  	 	TIMESTAMP  ;
 	DECLARE disSuppression  	TIMESTAMP  ;
 	DECLARE nbChevauchements 	INTEGER    ;

 	BEGIN
 	 	-- Recuperation des dates du dispositif dans des variables
 	 	SELECT dis_date_creation, dis_date_suppression INTO disCreation, disSuppression
 	 	FROM   iav.tg_dispositif_dis
 	 	WHERE  dis_identifiant = NEW.per_dis_identifiant
 	 	;

 	 	-- verification de la date de debut
 	 	IF ((NEW.per_date_debut < disCreation) OR (NEW.per_date_debut > disSuppression)) THEN
 	 		RAISE EXCEPTION 'Le debut de la periode doit etre inclus dans la periode d existence du dispositif.'  ;
 	 	END IF  ;

 	 	-- verification de la date de fin
 	 	IF ((NEW.per_date_fin < disCreation) OR (NEW.per_date_fin > disSuppression)) THEN
 	 	 	RAISE EXCEPTION 'La fin de la periode doit etre incluse dans la periode d existence du dispositif.'  ;
 	 	END IF  ;

 	 	-- verification des non-chevauchements pour les periodes du dispositif
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   iav.t_periodefonctdispositif_per
 	 	WHERE  per_dis_identifiant = NEW.per_dis_identifiant 
 	 	       AND (per_date_debut, per_date_fin) OVERLAPS (NEW.per_date_debut, NEW.per_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN
 	 	 	RAISE EXCEPTION 'Les periodes ne peuvent se chevaucher.'  ;
 	 	END IF  ;
		
		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION user_2.fct_per_date() OWNER TO postgres;

--
-- Name: fct_per_suppression(); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION fct_per_suppression() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	BEGIN
 	 	-- La periode precedent celle supprimee est prolongee
 	 	-- jusqu a la fin de la periode supprimee
 	 	UPDATE iav.t_periodefonctdispositif_per 
 	 	SET    per_date_fin = OLD.per_date_fin 
 	 	WHERE  per_date_fin= OLD.per_date_debut 
 	 	       AND per_dis_identifiant = OLD.per_dis_identifiant 
 	 	;
		RETURN NEW ;
 	END  ;

$$;


ALTER FUNCTION user_2.fct_per_suppression() OWNER TO postgres;

--
-- Name: fct_txe_date(); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION fct_txe_date() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   

 	DECLARE nbChevauchements INTEGER ;

 	BEGIN
 	 	-- verification des non-chevauchements pour les taux
 	 	SELECT COUNT(*) INTO nbChevauchements
 	 	FROM   iav.tj_tauxechappement_txe
 	 	WHERE  txe_ouv_identifiant = NEW.txe_ouv_identifiant
 	 	       AND txe_tax_code = NEW.txe_tax_code
 	 	       AND txe_std_code = NEW.txe_std_code
 	 	       AND (txe_date_debut, txe_date_fin) OVERLAPS (NEW.txe_date_debut, NEW.txe_date_fin)
 	 	;

		-- Comme le trigger est declenche sur AFTER et non pas sur BEFORE, il faut (nbChevauchements > 1) et non pas >0, car l enregistrement a deja ete ajoute, donc il se chevauche avec lui meme, ce qui est normal !
 	 	IF (nbChevauchements > 1) THEN
 	 	 	RAISE EXCEPTION 'Les taux d echappement ne peuvent se chevaucher.'  ;
 	 	END IF  ;

		RETURN NEW ;

 	END  ;
$$;


ALTER FUNCTION user_2.fct_txe_date() OWNER TO postgres;

--
-- Name: masqueope(); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION masqueope() RETURNS trigger
    LANGUAGE plpgsql
    AS $$   
DECLARE test text;


 	BEGIN
 	 	-- Recuperation du type
 	 	SELECT mas_type into test
 	 	FROM   iav.ts_masque_mas
 	 	WHERE  mas_id = NEW.mao_mas_id
 	 	;

 	 	-- verification 
 	 	IF (test!='ope') THEN
 	 	 	RAISE EXCEPTION 'Attention le masque n'' est pas un masque operation, changez le type de masque'  ;
 	 	END IF  ; 	 	

		RETURN NEW ;
 	END  ;
$$;


ALTER FUNCTION user_2.masqueope() OWNER TO postgres;

--
-- Name: plpgsql_call_handler(); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION plpgsql_call_handler() RETURNS language_handler
    LANGUAGE c
    AS '$libdir/plpgsql', 'plpgsql_call_handler';


ALTER FUNCTION user_2.plpgsql_call_handler() OWNER TO postgres;

--
-- Name: xpath_list(text, text); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION xpath_list(text, text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT xpath_list($1,$2,',')$_$;


ALTER FUNCTION user_2.xpath_list(text, text) OWNER TO postgres;

--
-- Name: xpath_nodeset(text, text); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION xpath_nodeset(text, text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT xpath_nodeset($1,$2,'','')$_$;


ALTER FUNCTION user_2.xpath_nodeset(text, text) OWNER TO postgres;

--
-- Name: xpath_nodeset(text, text, text); Type: FUNCTION; Schema: user_2; Owner: postgres
--

CREATE FUNCTION xpath_nodeset(text, text, text) RETURNS text
    LANGUAGE sql IMMUTABLE STRICT
    AS $_$SELECT xpath_nodeset($1,$2,'',$3)$_$;


ALTER FUNCTION user_2.xpath_nodeset(text, text, text) OWNER TO postgres;

SET search_path = public, pg_catalog;

--
-- Name: accum(geometry); Type: AGGREGATE; Schema: public; Owner: postgres
--

CREATE AGGREGATE accum(geometry) (
    SFUNC = public.pgis_geometry_accum_transfn,
    STYPE = pgis_abs,
    FINALFUNC = pgis_geometry_accum_finalfn
);


ALTER AGGREGATE public.accum(geometry) OWNER TO postgres;

--
-- Name: collect(geometry); Type: AGGREGATE; Schema: public; Owner: postgres
--

CREATE AGGREGATE collect(geometry) (
    SFUNC = public.pgis_geometry_accum_transfn,
    STYPE = pgis_abs,
    FINALFUNC = pgis_geometry_collect_finalfn
);


ALTER AGGREGATE public.collect(geometry) OWNER TO postgres;

--
-- Name: makeline(geometry); Type: AGGREGATE; Schema: public; Owner: postgres
--

CREATE AGGREGATE makeline(geometry) (
    SFUNC = public.pgis_geometry_accum_transfn,
    STYPE = pgis_abs,
    FINALFUNC = pgis_geometry_makeline_finalfn
);


ALTER AGGREGATE public.makeline(geometry) OWNER TO postgres;

--
-- Name: memcollect(geometry); Type: AGGREGATE; Schema: public; Owner: postgres
--

CREATE AGGREGATE memcollect(geometry) (
    SFUNC = public.st_collect,
    STYPE = geometry
);


ALTER AGGREGATE public.memcollect(geometry) OWNER TO postgres;

--
-- Name: polygonize(geometry); Type: AGGREGATE; Schema: public; Owner: postgres
--

CREATE AGGREGATE polygonize(geometry) (
    SFUNC = public.pgis_geometry_accum_transfn,
    STYPE = pgis_abs,
    FINALFUNC = pgis_geometry_polygonize_finalfn
);


ALTER AGGREGATE public.polygonize(geometry) OWNER TO postgres;

--
-- Name: st_extent3d(geometry); Type: AGGREGATE; Schema: public; Owner: postgres
--

CREATE AGGREGATE st_extent3d(geometry) (
    SFUNC = public.st_combine_bbox,
    STYPE = box3d
);


ALTER AGGREGATE public.st_extent3d(geometry) OWNER TO postgres;

--
-- Name: AGGREGATE st_extent3d(geometry); Type: COMMENT; Schema: public; Owner: postgres
--

COMMENT ON AGGREGATE st_extent3d(geometry) IS 'args: geomfield - an aggregate function that returns the box3D bounding box that bounds rows of geometries.';


SET search_path = nat, pg_catalog;

--
-- Name: serial_ann; Type: SEQUENCE; Schema: nat; Owner: postgres
--

CREATE SEQUENCE serial_ann
    START WITH 1997
    INCREMENT BY 1
    MINVALUE 1997
    MAXVALUE 2012
    CACHE 1
    CYCLE;


ALTER TABLE serial_ann OWNER TO postgres;

--
-- Name: serial_sem; Type: SEQUENCE; Schema: nat; Owner: postgres
--

CREATE SEQUENCE serial_sem
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    MAXVALUE 52
    CACHE 1
    CYCLE;


ALTER TABLE serial_sem OWNER TO postgres;

SET default_tablespace = '';

SET default_with_oids = true;

--
-- Name: t_bilanmigrationjournalier_bjo; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE t_bilanmigrationjournalier_bjo (
    bjo_identifiant integer NOT NULL,
    bjo_dis_identifiant integer NOT NULL,
    bjo_tax_code character varying(6) NOT NULL,
    bjo_std_code character varying(4) NOT NULL,
    bjo_annee integer NOT NULL,
    bjo_jour timestamp without time zone NOT NULL,
    bjo_labelquantite character varying(30),
    bjo_valeur double precision,
    bjo_horodateexport timestamp without time zone,
    bjo_org_code character varying(30) NOT NULL
);


ALTER TABLE t_bilanmigrationjournalier_bjo OWNER TO nat;

--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq OWNER TO nat;

--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq OWNED BY t_bilanmigrationjournalier_bjo.bjo_identifiant;


--
-- Name: t_bilanmigrationmensuel_bme; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE t_bilanmigrationmensuel_bme (
    bme_identifiant integer NOT NULL,
    bme_dis_identifiant integer NOT NULL,
    bme_tax_code character varying(6) NOT NULL,
    bme_std_code character varying(4) NOT NULL,
    bme_annee integer NOT NULL,
    bme_mois integer NOT NULL,
    bme_labelquantite character varying(30),
    bme_valeur double precision,
    bme_horodateexport timestamp without time zone,
    bme_org_code character varying(30) NOT NULL
);


ALTER TABLE t_bilanmigrationmensuel_bme OWNER TO nat;

--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_bilanmigrationmensuel_bme_bme_identifiant_seq OWNER TO nat;

--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq OWNED BY t_bilanmigrationmensuel_bme.bme_identifiant;


--
-- Name: t_dispositifcomptage_dic; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE t_dispositifcomptage_dic (
    dic_dis_identifiant integer NOT NULL,
    dic_dif_identifiant integer NOT NULL,
    dic_code character varying(16) NOT NULL,
    dic_tdc_code integer NOT NULL,
    dic_org_code character varying(30) NOT NULL
);


ALTER TABLE t_dispositifcomptage_dic OWNER TO postgres;

--
-- Name: t_dispositiffranchissement_dif; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE t_dispositiffranchissement_dif (
    dif_dis_identifiant integer NOT NULL,
    dif_ouv_identifiant integer NOT NULL,
    dif_code character varying(16) NOT NULL,
    dif_localisation text,
    dif_orientation character varying(20) NOT NULL,
    dif_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_dif_orientation CHECK (((upper((dif_orientation)::text) = 'DESCENTE'::text) OR (upper((dif_orientation)::text) = 'MONTEE'::text)))
);


ALTER TABLE t_dispositiffranchissement_dif OWNER TO postgres;

--
-- Name: t_lot_lot; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE t_lot_lot (
    lot_identifiant integer NOT NULL,
    lot_ope_identifiant integer NOT NULL,
    lot_tax_code character varying(6) NOT NULL,
    lot_std_code character varying(4) NOT NULL,
    lot_effectif double precision,
    lot_quantite double precision,
    lot_qte_code character varying(4),
    lot_methode_obtention character varying(10) NOT NULL,
    lot_lot_identifiant integer,
    lot_dev_code character varying(4),
    lot_commentaires text,
    lot_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_lot_lot_identifiant CHECK ((lot_lot_identifiant <> lot_identifiant)),
    CONSTRAINT c_ck_lot_methode_obtention CHECK (((((upper((lot_methode_obtention)::text) = 'MESURE'::text) OR (upper((lot_methode_obtention)::text) = 'CALCULE'::text)) OR (upper((lot_methode_obtention)::text) = 'EXPERT'::text)) OR (upper((lot_methode_obtention)::text) = 'PONCTUEL'::text)))
);


ALTER TABLE t_lot_lot OWNER TO nat;

--
-- Name: t_lot_lot_lot_identifiant_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE t_lot_lot_lot_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_lot_lot_lot_identifiant_seq OWNER TO nat;

--
-- Name: t_lot_lot_lot_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE t_lot_lot_lot_identifiant_seq OWNED BY t_lot_lot.lot_identifiant;


--
-- Name: t_marque_mqe; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE t_marque_mqe (
    mqe_reference character varying(30) NOT NULL,
    mqe_loc_code character varying(4) NOT NULL,
    mqe_nmq_code character varying(4) NOT NULL,
    mqe_omq_reference character varying(30) NOT NULL,
    mqe_commentaires text,
    mqe_org_code character varying(30) NOT NULL
);


ALTER TABLE t_marque_mqe OWNER TO postgres;

--
-- Name: t_operation_ope; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE t_operation_ope (
    ope_identifiant integer NOT NULL,
    ope_dic_identifiant integer NOT NULL,
    ope_date_debut timestamp(0) without time zone NOT NULL,
    ope_date_fin timestamp(0) without time zone NOT NULL,
    ope_organisme character varying(35),
    ope_operateur character varying(35),
    ope_commentaires text,
    ope_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_ope_date_fin CHECK (((ope_date_fin >= ope_date_debut) AND (date_part('month'::text, age(ope_date_fin, ope_date_debut)) <= (1)::double precision)))
);


ALTER TABLE t_operation_ope OWNER TO nat;

--
-- Name: t_operation_ope_ope_identifiant_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE t_operation_ope_ope_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_operation_ope_ope_identifiant_seq OWNER TO nat;

--
-- Name: t_operation_ope_ope_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE t_operation_ope_ope_identifiant_seq OWNED BY t_operation_ope.ope_identifiant;


--
-- Name: t_operationmarquage_omq; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE t_operationmarquage_omq (
    omq_reference character varying(30) NOT NULL,
    omq_commentaires text,
    omq_org_code character varying(30) NOT NULL
);


ALTER TABLE t_operationmarquage_omq OWNER TO postgres;

--
-- Name: t_ouvrage_ouv; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE t_ouvrage_ouv (
    ouv_identifiant integer NOT NULL,
    ouv_sta_code character varying(8) NOT NULL,
    ouv_code character varying(12) NOT NULL,
    ouv_libelle character varying(40) NOT NULL,
    ouv_localisation text,
    ouv_coordonnee_x integer,
    ouv_coordonnee_y integer,
    ouv_altitude smallint,
    ouv_carte_localisation bytea,
    ouv_commentaires text,
    ouv_nov_code character varying(4) NOT NULL,
    ouv_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_ouv_altitude CHECK ((ouv_altitude >= 0)),
    CONSTRAINT c_ck_ouv_coordonnee_x CHECK (((ouv_coordonnee_x >= (-400000)) AND (ouv_coordonnee_x < 1300000))),
    CONSTRAINT c_ck_ouv_coordonnee_y CHECK (((ouv_coordonnee_y >= 6000000) AND (ouv_coordonnee_y < 7100000)))
);


ALTER TABLE t_ouvrage_ouv OWNER TO nat;

--
-- Name: COLUMN t_ouvrage_ouv.ouv_coordonnee_x; Type: COMMENT; Schema: nat; Owner: nat
--

COMMENT ON COLUMN t_ouvrage_ouv.ouv_coordonnee_x IS 'Coordonnée x en Lambert 93';


--
-- Name: COLUMN t_ouvrage_ouv.ouv_coordonnee_y; Type: COMMENT; Schema: nat; Owner: nat
--

COMMENT ON COLUMN t_ouvrage_ouv.ouv_coordonnee_y IS 'Coordonnée y en Lambert 93';


--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq
    START WITH 7
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_ouvrage_ouv_ouv_identifiant_seq OWNER TO nat;

--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq OWNED BY t_ouvrage_ouv.ouv_identifiant;


--
-- Name: t_periodefonctdispositif_per; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE t_periodefonctdispositif_per (
    per_dis_identifiant integer NOT NULL,
    per_date_debut timestamp(0) without time zone NOT NULL,
    per_date_fin timestamp(0) without time zone NOT NULL,
    per_commentaires text,
    per_etat_fonctionnement boolean NOT NULL,
    per_tar_code character varying(4),
    per_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_per_date_fin CHECK ((per_date_fin >= per_date_debut))
);


ALTER TABLE t_periodefonctdispositif_per OWNER TO postgres;

--
-- Name: t_station_sta; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE t_station_sta (
    sta_code character varying(8) NOT NULL,
    sta_nom character varying(40) NOT NULL,
    sta_localisation character varying(60),
    sta_coordonnee_x integer,
    sta_coordonnee_y integer,
    sta_altitude smallint,
    sta_carte_localisation bytea,
    sta_superficie integer,
    sta_distance_mer double precision,
    sta_date_creation date,
    sta_date_suppression date,
    sta_commentaires text,
    sta_dernier_import_conditions timestamp(0) without time zone,
    sta_org_code character varying(30) NOT NULL,
    sta_geom public.geometry(Point,2154),
    CONSTRAINT c_ck_sta_altitude CHECK ((sta_altitude >= 0)),
    CONSTRAINT c_ck_sta_coordonnee_x CHECK (((sta_coordonnee_x >= (-400000)) AND (sta_coordonnee_x < 1300000))),
    CONSTRAINT c_ck_sta_coordonnee_y CHECK (((sta_coordonnee_y >= 6000000) AND (sta_coordonnee_y < 7100000))),
    CONSTRAINT c_ck_sta_date_suppression CHECK ((sta_date_suppression >= sta_date_creation)),
    CONSTRAINT c_ck_sta_distance_mer CHECK ((sta_distance_mer >= (0)::double precision)),
    CONSTRAINT c_ck_sta_superficie CHECK ((sta_superficie >= 0))
);


ALTER TABLE t_station_sta OWNER TO postgres;

--
-- Name: COLUMN t_station_sta.sta_coordonnee_x; Type: COMMENT; Schema: nat; Owner: postgres
--

COMMENT ON COLUMN t_station_sta.sta_coordonnee_x IS 'Coordonnée x en Lambert 93';


--
-- Name: COLUMN t_station_sta.sta_coordonnee_y; Type: COMMENT; Schema: nat; Owner: postgres
--

COMMENT ON COLUMN t_station_sta.sta_coordonnee_y IS 'Coordonnée y en Lambert 93';


--
-- Name: tg_dispositif_dis; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE tg_dispositif_dis (
    dis_identifiant integer NOT NULL,
    dis_date_creation date,
    dis_date_suppression date,
    dis_commentaires text,
    dis_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_dis_date_suppression CHECK ((dis_date_suppression >= dis_date_creation))
);


ALTER TABLE tg_dispositif_dis OWNER TO nat;

--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE tg_dispositif_dis_dis_identifiant_seq
    START WITH 19
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE tg_dispositif_dis_dis_identifiant_seq OWNER TO nat;

--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE tg_dispositif_dis_dis_identifiant_seq OWNED BY tg_dispositif_dis.dis_identifiant;


--
-- Name: tj_actionmarquage_act; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_actionmarquage_act (
    act_lot_identifiant integer NOT NULL,
    act_mqe_reference character varying(30) NOT NULL,
    act_action character varying(20) NOT NULL,
    act_commentaires text,
    act_org_code character varying(30) NOT NULL,
    act_nmq_code character varying(4) NOT NULL,
    CONSTRAINT c_ck_act_action CHECK ((((upper((act_action)::text) = 'POSE'::text) OR (upper((act_action)::text) = 'LECTURE'::text)) OR (upper((act_action)::text) = 'RETRAIT'::text)))
);


ALTER TABLE tj_actionmarquage_act OWNER TO postgres;

--
-- Name: tj_caracteristiquelot_car; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_caracteristiquelot_car (
    car_lot_identifiant integer NOT NULL,
    car_par_code character varying(5) NOT NULL,
    car_methode_obtention character varying(10),
    car_val_identifiant integer,
    car_valeur_quantitatif real,
    car_precision real,
    car_commentaires text,
    car_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_car CHECK ((((car_val_identifiant IS NOT NULL) AND (car_valeur_quantitatif IS NULL)) OR ((car_val_identifiant IS NULL) AND (car_valeur_quantitatif IS NOT NULL)))),
    CONSTRAINT c_ck_car_methode_obtention CHECK ((((upper((car_methode_obtention)::text) = 'MESURE'::text) OR (upper((car_methode_obtention)::text) = 'CALCULE'::text)) OR (upper((car_methode_obtention)::text) = 'EXPERT'::text)))
);


ALTER TABLE tj_caracteristiquelot_car OWNER TO postgres;

--
-- Name: tj_coefficientconversion_coe; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_coefficientconversion_coe (
    coe_tax_code character varying(6) NOT NULL,
    coe_std_code character varying(4) NOT NULL,
    coe_qte_code character varying(4) NOT NULL,
    coe_date_debut date NOT NULL,
    coe_date_fin date NOT NULL,
    coe_valeur_coefficient real,
    coe_commentaires text,
    coe_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_coe_date_fin CHECK ((coe_date_fin >= coe_date_debut))
);


ALTER TABLE tj_coefficientconversion_coe OWNER TO postgres;

--
-- Name: tj_conditionenvironnementale_env; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_conditionenvironnementale_env (
    env_date_debut timestamp(0) without time zone NOT NULL,
    env_date_fin timestamp(0) without time zone NOT NULL,
    env_methode_obtention character varying(10),
    env_val_identifiant integer,
    env_valeur_quantitatif real,
    env_stm_identifiant integer NOT NULL,
    env_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_env CHECK ((((env_val_identifiant IS NOT NULL) AND (env_valeur_quantitatif IS NULL)) OR ((env_val_identifiant IS NULL) AND (env_valeur_quantitatif IS NOT NULL)))),
    CONSTRAINT c_ck_env_date_fin CHECK ((env_date_fin >= env_date_debut)),
    CONSTRAINT c_ck_env_methode_obtention CHECK (((upper((env_methode_obtention)::text) = 'MESURE'::text) OR (upper((env_methode_obtention)::text) = 'CALCULE'::text)))
);


ALTER TABLE tj_conditionenvironnementale_env OWNER TO postgres;

--
-- Name: tj_dfestdestinea_dtx; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_dfestdestinea_dtx (
    dtx_dif_identifiant integer NOT NULL,
    dtx_tax_code character varying(6) NOT NULL,
    dtx_org_code character varying(30) NOT NULL
);


ALTER TABLE tj_dfestdestinea_dtx OWNER TO postgres;

--
-- Name: tj_dfesttype_dft; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_dfesttype_dft (
    dft_df_identifiant integer NOT NULL,
    dft_rang smallint NOT NULL,
    dft_org_code character varying(30) NOT NULL,
    dft_tdf_code character varying(2) NOT NULL,
    CONSTRAINT c_ck_dft_rang CHECK ((dft_rang >= 0))
);


ALTER TABLE tj_dfesttype_dft OWNER TO postgres;

--
-- Name: tj_pathologieconstatee_pco; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_pathologieconstatee_pco (
    pco_lot_identifiant integer NOT NULL,
    pco_pat_code character varying(4) NOT NULL,
    pco_loc_code character varying(4) NOT NULL,
    pco_commentaires text,
    pco_org_code character varying(30) NOT NULL,
    pco_imp_code integer
);


ALTER TABLE tj_pathologieconstatee_pco OWNER TO postgres;

SET default_with_oids = false;

--
-- Name: tj_prelevementlot_prl; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_prelevementlot_prl (
    prl_pre_typeprelevement character varying(15) NOT NULL,
    prl_lot_identifiant integer NOT NULL,
    prl_code character varying(12) NOT NULL,
    prl_operateur character varying(35),
    prl_loc_code character varying(4),
    prl_commentaires text,
    prl_org_code character varying(30) NOT NULL
);


ALTER TABLE tj_prelevementlot_prl OWNER TO postgres;

--
-- Name: tj_stationmesure_stm; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE tj_stationmesure_stm (
    stm_identifiant integer NOT NULL,
    stm_libelle character varying(41),
    stm_sta_code character varying(8) NOT NULL,
    stm_par_code character varying(5) NOT NULL,
    stm_description text,
    stm_org_code character varying(30) NOT NULL
);


ALTER TABLE tj_stationmesure_stm OWNER TO nat;

--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE tj_stationmesure_stm_stm_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE tj_stationmesure_stm_stm_identifiant_seq OWNER TO nat;

--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE tj_stationmesure_stm_stm_identifiant_seq OWNED BY tj_stationmesure_stm.stm_identifiant;


SET default_with_oids = true;

--
-- Name: tj_tauxechappement_txe; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_tauxechappement_txe (
    txe_tax_code character varying(6) NOT NULL,
    txe_std_code character varying(4) NOT NULL,
    txe_date_debut timestamp(0) without time zone NOT NULL,
    txe_date_fin timestamp(0) without time zone NOT NULL,
    txe_methode_estimation text,
    txe_ech_code character varying(4),
    txe_valeur_taux smallint,
    txe_commentaires text,
    txe_org_code character varying(30) NOT NULL,
    txe_sta_code character varying(8) NOT NULL,
    CONSTRAINT c_ck_txe_date_fin CHECK ((txe_date_fin >= txe_date_debut)),
    CONSTRAINT c_ck_txe_methode_estimation CHECK ((((upper(txe_methode_estimation) = 'MESURE'::text) OR (upper(txe_methode_estimation) = 'CALCULE'::text)) OR (upper(txe_methode_estimation) = 'EXPERT'::text))),
    CONSTRAINT c_ck_txe_valeur_taux CHECK ((((txe_valeur_taux >= 0) AND (txe_valeur_taux <= 100)) OR ((txe_valeur_taux IS NULL) AND (txe_ech_code IS NOT NULL))))
);


ALTER TABLE tj_tauxechappement_txe OWNER TO postgres;

SET default_with_oids = false;

--
-- Name: ts_maintenance_main; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_maintenance_main (
    main_identifiant integer NOT NULL,
    main_ticket integer,
    main_description text
);


ALTER TABLE ts_maintenance_main OWNER TO postgres;

--
-- Name: TABLE ts_maintenance_main; Type: COMMENT; Schema: nat; Owner: postgres
--

COMMENT ON TABLE ts_maintenance_main IS 'Table de suivi des operations de maintenance de la base';


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE; Schema: nat; Owner: postgres
--

CREATE SEQUENCE ts_maintenance_main_main_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_maintenance_main_main_identifiant_seq OWNER TO postgres;

--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: postgres
--

ALTER SEQUENCE ts_maintenance_main_main_identifiant_seq OWNED BY ts_maintenance_main.main_identifiant;


--
-- Name: ts_masque_mas; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE ts_masque_mas (
    mas_id integer NOT NULL,
    mas_code character varying(15) NOT NULL,
    mas_description text,
    mas_raccourci text,
    mas_type character varying(3),
    CONSTRAINT c_ck_mas_type CHECK ((((mas_type)::text = 'lot'::text) OR ((mas_type)::text = 'ope'::text)))
);


ALTER TABLE ts_masque_mas OWNER TO nat;

--
-- Name: ts_masque_mas_mas_id_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE ts_masque_mas_mas_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_masque_mas_mas_id_seq OWNER TO nat;

--
-- Name: ts_masque_mas_mas_id_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE ts_masque_mas_mas_id_seq OWNED BY ts_masque_mas.mas_id;


--
-- Name: ts_masquecaracteristiquelot_mac; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE ts_masquecaracteristiquelot_mac (
    mac_id integer NOT NULL,
    mac_mal_id integer NOT NULL,
    mac_par_code character varying(5) NOT NULL,
    mac_affichagevaleur boolean,
    mac_affichageprecision boolean,
    mac_affichagemethodeobtention boolean,
    mac_affichagecommentaire boolean,
    mac_valeurquantitatifdefaut numeric,
    mac_valeurqualitatifdefaut integer,
    mac_precisiondefaut numeric,
    mac_methodeobtentiondefaut character varying(10),
    mac_commentairedefaut text
);


ALTER TABLE ts_masquecaracteristiquelot_mac OWNER TO nat;

--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_masquecaracteristiquelot_mac_mac_id_seq OWNER TO nat;

--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq OWNED BY ts_masquecaracteristiquelot_mac.mac_id;


--
-- Name: ts_masqueconditionsenvironnementales_mae; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_masqueconditionsenvironnementales_mae (
    mae_mao_id integer NOT NULL,
    mae_stm_identifiant integer NOT NULL,
    mae_affichage boolean,
    mae_valeurdefaut text
);


ALTER TABLE ts_masqueconditionsenvironnementales_mae OWNER TO postgres;

--
-- Name: ts_masquelot_mal; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_masquelot_mal (
    mal_mas_id integer NOT NULL,
    mal_affichage boolean[],
    mal_valeurdefaut text[],
    CONSTRAINT c_ck_length_mal_affichage CHECK ((array_length(mal_affichage, 1) = 21)),
    CONSTRAINT c_ck_length_mal_valeurdefaut CHECK ((array_length(mal_valeurdefaut, 1) = 14))
);


ALTER TABLE ts_masquelot_mal OWNER TO postgres;

--
-- Name: ts_masqueope_mao; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_masqueope_mao (
    mao_mas_id integer NOT NULL,
    mao_affichage boolean[],
    mao_valeurdefaut text[],
    CONSTRAINT c_ck_length_mao_affichage CHECK ((array_length(mao_affichage, 1) = 16)),
    CONSTRAINT c_ck_length_mao_valeurdefaut CHECK ((array_length(mao_valeurdefaut, 1) = 9))
);


ALTER TABLE ts_masqueope_mao OWNER TO postgres;

--
-- Name: ts_masqueordreaffichage_maa; Type: TABLE; Schema: nat; Owner: nat; Tablespace: 
--

CREATE TABLE ts_masqueordreaffichage_maa (
    maa_id integer NOT NULL,
    maa_mal_id integer,
    maa_table character varying(40) NOT NULL,
    maa_valeur text NOT NULL,
    maa_champdumasque character varying(30),
    maa_rang integer
);


ALTER TABLE ts_masqueordreaffichage_maa OWNER TO nat;

--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: SEQUENCE; Schema: nat; Owner: nat
--

CREATE SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_masqueordreaffichage_maa_maa_id_seq OWNER TO nat;

--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: SEQUENCE OWNED BY; Schema: nat; Owner: nat
--

ALTER SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq OWNED BY ts_masqueordreaffichage_maa.maa_id;


SET default_with_oids = true;

--
-- Name: ts_taillevideo_tav; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_taillevideo_tav (
    tav_dic_identifiant integer NOT NULL,
    tav_coefconversion numeric NOT NULL,
    tav_distance character varying(3) NOT NULL,
    tav_org_code character varying(30) NOT NULL
);


ALTER TABLE ts_taillevideo_tav OWNER TO postgres;

--
-- Name: ts_taxonvideo_txv; Type: TABLE; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_taxonvideo_txv (
    txv_code character varying(3) NOT NULL,
    txv_tax_code character varying(6),
    txv_std_code character varying(4),
    txv_org_code character varying(30) NOT NULL
);


ALTER TABLE ts_taxonvideo_txv OWNER TO postgres;

--
-- Name: vue_cond_env_mj; Type: VIEW; Schema: nat; Owner: postgres
--

CREATE VIEW vue_cond_env_mj AS
 SELECT sm.stm_sta_code,
    sm.stm_libelle,
    date_trunc('day'::text, e.env_date_debut) AS date_debut,
    avg(e.env_valeur_quantitatif) AS valeur
   FROM (tj_stationmesure_stm sm
     JOIN tj_conditionenvironnementale_env e ON ((sm.stm_identifiant = e.env_stm_identifiant)))
  GROUP BY sm.stm_sta_code, sm.stm_libelle, date_trunc('day'::text, e.env_date_debut);


ALTER TABLE vue_cond_env_mj OWNER TO postgres;

--
-- Name: vue_ope_cond_env_mj; Type: VIEW; Schema: nat; Owner: postgres
--

CREATE VIEW vue_ope_cond_env_mj AS
 SELECT s.sta_nom,
    date_trunc('day'::text, op.ope_date_debut) AS date_debut,
    cemj.stm_libelle,
    cemj.valeur
   FROM (((((t_station_sta s
     JOIN t_ouvrage_ouv o ON (((s.sta_code)::text = (o.ouv_sta_code)::text)))
     JOIN t_dispositiffranchissement_dif df ON ((df.dif_ouv_identifiant = o.ouv_identifiant)))
     JOIN t_dispositifcomptage_dic dc ON ((dc.dic_dif_identifiant = df.dif_dis_identifiant)))
     JOIN t_operation_ope op ON ((op.ope_dic_identifiant = dc.dic_dis_identifiant)))
     RIGHT JOIN vue_cond_env_mj cemj ON (((date_trunc('day'::text, op.ope_date_debut) = cemj.date_debut) AND ((cemj.stm_sta_code)::text = (s.sta_code)::text))))
  GROUP BY s.sta_nom, date_trunc('day'::text, op.ope_date_debut), cemj.stm_libelle, cemj.valeur
  ORDER BY s.sta_nom;


ALTER TABLE vue_ope_cond_env_mj OWNER TO postgres;

SET search_path = public, pg_catalog;

SET default_with_oids = false;

--
-- Name: chevauchement; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE chevauchement (
    ope1 integer,
    ope2 integer,
    ope_dic_identifiant integer
);


ALTER TABLE chevauchement OWNER TO postgres;

SET search_path = ref, pg_catalog;

SET default_with_oids = true;

--
-- Name: tg_parametre_par; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tg_parametre_par (
    par_code character varying(5) NOT NULL,
    par_nom character varying(60) NOT NULL,
    par_unite character varying(20),
    par_nature character varying(20) NOT NULL,
    par_definition text,
    CONSTRAINT c_ck_par_nature CHECK (((upper((par_nature)::text) = 'BIOLOGIQUE'::text) OR (upper((par_nature)::text) = 'ENVIRONNEMENTAL'::text)))
);


ALTER TABLE tg_parametre_par OWNER TO postgres;

--
-- Name: tr_devenirlot_dev; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_devenirlot_dev (
    dev_code character varying(4) NOT NULL,
    dev_libelle character varying(100) NOT NULL,
    dev_rang smallint DEFAULT 1 NOT NULL
);


ALTER TABLE tr_devenirlot_dev OWNER TO postgres;

SET default_with_oids = false;

--
-- Name: tr_importancepatho_imp; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_importancepatho_imp (
    imp_code integer NOT NULL,
    imp_libelle character varying(100),
    imp_definition text
);


ALTER TABLE tr_importancepatho_imp OWNER TO postgres;

SET default_with_oids = true;

--
-- Name: tr_localisationanatomique_loc; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_localisationanatomique_loc (
    loc_code character varying(4) NOT NULL,
    loc_libelle character varying(40) NOT NULL
);


ALTER TABLE tr_localisationanatomique_loc OWNER TO postgres;

--
-- Name: tr_naturemarque_nmq; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_naturemarque_nmq (
    nmq_code character varying(4) NOT NULL,
    nmq_libelle character varying(60) NOT NULL
);


ALTER TABLE tr_naturemarque_nmq OWNER TO postgres;

--
-- Name: tr_natureouvrage_nov; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_natureouvrage_nov (
    nov_code character varying(4) NOT NULL,
    nov_nom character varying(40) NOT NULL
);


ALTER TABLE tr_natureouvrage_nov OWNER TO postgres;

--
-- Name: tr_niveauechappement_ech; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_niveauechappement_ech (
    ech_code character varying(4) NOT NULL,
    ech_libelle character varying(40) NOT NULL,
    ech_equivalence_tx character varying(10)
);


ALTER TABLE tr_niveauechappement_ech OWNER TO postgres;

--
-- Name: tr_niveautaxonomique_ntx; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_niveautaxonomique_ntx (
    ntx_code character varying(4) NOT NULL,
    ntx_libelle character varying(20) NOT NULL,
    ntx_mnemonique character varying(4) NOT NULL
);


ALTER TABLE tr_niveautaxonomique_ntx OWNER TO postgres;

--
-- Name: tr_parametrequalitatif_qal; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_parametrequalitatif_qal (
    qal_par_code character varying(5) NOT NULL,
    qal_valeurs_possibles character varying(150)
);


ALTER TABLE tr_parametrequalitatif_qal OWNER TO postgres;

--
-- Name: tr_parametrequantitatif_qan; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_parametrequantitatif_qan (
    qan_par_code character varying(5) NOT NULL
);


ALTER TABLE tr_parametrequantitatif_qan OWNER TO postgres;

--
-- Name: tr_pathologie_pat; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_pathologie_pat (
    pat_code character varying(4) NOT NULL,
    pat_libelle character varying(100) NOT NULL,
    pat_definition text
);


ALTER TABLE tr_pathologie_pat OWNER TO postgres;

SET default_with_oids = false;

--
-- Name: tr_prelevement_pre; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_prelevement_pre (
    pre_typeprelevement character varying(15) NOT NULL,
    pre_definition text
);


ALTER TABLE tr_prelevement_pre OWNER TO postgres;

SET default_with_oids = true;

--
-- Name: tr_stadedeveloppement_std; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_stadedeveloppement_std (
    std_code character varying(4) NOT NULL,
    std_libelle character varying(40) NOT NULL,
    std_definition text,
    std_statut character varying(10)
);


ALTER TABLE tr_stadedeveloppement_std OWNER TO postgres;

--
-- Name: tr_taxon_tax; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_taxon_tax (
    tax_code character varying(6) NOT NULL,
    tax_nom_latin character varying(30) NOT NULL,
    tax_nom_commun character varying(30),
    tax_ntx_code character varying(4) NOT NULL,
    tax_tax_code character varying(6),
    tax_rang integer DEFAULT 1 NOT NULL
);


ALTER TABLE tr_taxon_tax OWNER TO postgres;

--
-- Name: tr_typearretdisp_tar; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_typearretdisp_tar (
    tar_code character varying(4) NOT NULL,
    tar_libelle character varying(60) NOT NULL,
    tar_commentaires text
);


ALTER TABLE tr_typearretdisp_tar OWNER TO postgres;

--
-- Name: tr_typedc_tdc; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_typedc_tdc (
    tdc_code integer NOT NULL,
    tdc_libelle character varying(60) NOT NULL,
    tdc_definition text
);


ALTER TABLE tr_typedc_tdc OWNER TO postgres;

--
-- Name: tr_typedf_tdf; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_typedf_tdf (
    tdf_libelle character varying(60) NOT NULL,
    tdf_mnemonique character varying(15),
    tdf_definition text,
    tdf_code character varying(2) NOT NULL
);


ALTER TABLE tr_typedf_tdf OWNER TO postgres;

--
-- Name: tr_typequantitelot_qte; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_typequantitelot_qte (
    qte_code character varying(4) NOT NULL,
    qte_libelle character varying(40) NOT NULL
);


ALTER TABLE tr_typequantitelot_qte OWNER TO postgres;

--
-- Name: tr_valeurparametrequalitatif_val; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE tr_valeurparametrequalitatif_val (
    val_identifiant integer NOT NULL,
    val_qal_code character varying(5) NOT NULL,
    val_rang smallint NOT NULL,
    val_libelle character varying(100) NOT NULL,
    CONSTRAINT c_ck_val_rang CHECK ((val_rang >= 0))
);


ALTER TABLE tr_valeurparametrequalitatif_val OWNER TO postgres;

--
-- Name: tr_valeurparametrequalitatif_val_val_identifiant_seq; Type: SEQUENCE; Schema: ref; Owner: postgres
--

CREATE SEQUENCE tr_valeurparametrequalitatif_val_val_identifiant_seq
    START WITH 60
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE tr_valeurparametrequalitatif_val_val_identifiant_seq OWNER TO postgres;

--
-- Name: tr_valeurparametrequalitatif_val_val_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: ref; Owner: postgres
--

ALTER SEQUENCE tr_valeurparametrequalitatif_val_val_identifiant_seq OWNED BY tr_valeurparametrequalitatif_val.val_identifiant;


SET default_with_oids = false;

--
-- Name: ts_maintenance_main; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_maintenance_main (
    main_identifiant integer NOT NULL,
    main_ticket integer,
    main_description text
);


ALTER TABLE ts_maintenance_main OWNER TO postgres;

--
-- Name: TABLE ts_maintenance_main; Type: COMMENT; Schema: ref; Owner: postgres
--

COMMENT ON TABLE ts_maintenance_main IS 'Table de suivi des operations de maintenance de la base';


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE; Schema: ref; Owner: postgres
--

CREATE SEQUENCE ts_maintenance_main_main_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_maintenance_main_main_identifiant_seq OWNER TO postgres;

--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: ref; Owner: postgres
--

ALTER SEQUENCE ts_maintenance_main_main_identifiant_seq OWNED BY ts_maintenance_main.main_identifiant;


--
-- Name: ts_messager_msr; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_messager_msr (
    msr_id integer NOT NULL,
    msr_element character varying(50),
    msr_number character varying(5),
    msr_type character varying(20),
    msr_endofline boolean,
    msr_comment text
);


ALTER TABLE ts_messager_msr OWNER TO postgres;

--
-- Name: TABLE ts_messager_msr; Type: COMMENT; Schema: ref; Owner: postgres
--

COMMENT ON TABLE ts_messager_msr IS 'this table contains the name and descriptions the string msr_element.msr_number which is used in t_messagelang_mrl 
to put strings for each langage, it is separated as the comment and the place in the program applies only once';


--
-- Name: COLUMN ts_messager_msr.msr_endofline; Type: COMMENT; Schema: ref; Owner: postgres
--

COMMENT ON COLUMN ts_messager_msr.msr_endofline IS 'will the string be terminated by antislash+n';


--
-- Name: ts_messager_msr_msr_id_seq; Type: SEQUENCE; Schema: ref; Owner: postgres
--

CREATE SEQUENCE ts_messager_msr_msr_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_messager_msr_msr_id_seq OWNER TO postgres;

--
-- Name: ts_messager_msr_msr_id_seq; Type: SEQUENCE OWNED BY; Schema: ref; Owner: postgres
--

ALTER SEQUENCE ts_messager_msr_msr_id_seq OWNED BY ts_messager_msr.msr_id;


--
-- Name: ts_messagerlang_mrl; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_messagerlang_mrl (
    mrl_id integer NOT NULL,
    mrl_msr_id integer,
    mrl_text text,
    mrl_lang character varying(10),
    CONSTRAINT c_ck_mrl_lang CHECK (((((mrl_lang)::text = 'French'::text) OR ((mrl_lang)::text = 'English'::text)) OR ((mrl_lang)::text = 'Spanish'::text)))
);


ALTER TABLE ts_messagerlang_mrl OWNER TO postgres;

--
-- Name: TABLE ts_messagerlang_mrl; Type: COMMENT; Schema: ref; Owner: postgres
--

COMMENT ON TABLE ts_messagerlang_mrl IS 'this table contains the string of code to appear in the graphical R interface, one for each langage';


--
-- Name: ts_messagerlang_mrl_mrl_id_seq; Type: SEQUENCE; Schema: ref; Owner: postgres
--

CREATE SEQUENCE ts_messagerlang_mrl_mrl_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_messagerlang_mrl_mrl_id_seq OWNER TO postgres;

--
-- Name: ts_messagerlang_mrl_mrl_id_seq; Type: SEQUENCE OWNED BY; Schema: ref; Owner: postgres
--

ALTER SEQUENCE ts_messagerlang_mrl_mrl_id_seq OWNED BY ts_messagerlang_mrl.mrl_id;


--
-- Name: ts_nomenclature_nom; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_nomenclature_nom (
    nom_id integer NOT NULL,
    nom_nomtable character varying(60),
    nom_nomenclaturesandreid integer,
    nom_datemiseajour date,
    nom_commentaire text
);


ALTER TABLE ts_nomenclature_nom OWNER TO postgres;

--
-- Name: ts_nomenclature_nom_nom_id_seq; Type: SEQUENCE; Schema: ref; Owner: postgres
--

CREATE SEQUENCE ts_nomenclature_nom_nom_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_nomenclature_nom_nom_id_seq OWNER TO postgres;

--
-- Name: ts_nomenclature_nom_nom_id_seq; Type: SEQUENCE OWNED BY; Schema: ref; Owner: postgres
--

ALTER SEQUENCE ts_nomenclature_nom_nom_id_seq OWNED BY ts_nomenclature_nom.nom_id;


SET default_with_oids = true;

--
-- Name: ts_organisme_org; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_organisme_org (
    org_code character varying(30) NOT NULL,
    org_description text
);


ALTER TABLE ts_organisme_org OWNER TO postgres;

--
-- Name: TABLE ts_organisme_org; Type: COMMENT; Schema: ref; Owner: postgres
--

COMMENT ON TABLE ts_organisme_org IS 'Table système contenant les organismes fournisseurs de données.. A COMPLETER';


--
-- Name: COLUMN ts_organisme_org.org_code; Type: COMMENT; Schema: ref; Owner: postgres
--

COMMENT ON COLUMN ts_organisme_org.org_code IS 'Code de l`organisme fournisseur de données';


--
-- Name: COLUMN ts_organisme_org.org_description; Type: COMMENT; Schema: ref; Owner: postgres
--

COMMENT ON COLUMN ts_organisme_org.org_description IS 'Description de l`organisme';


SET default_with_oids = false;

--
-- Name: ts_sequence_seq; Type: TABLE; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_sequence_seq (
    seq_sequence character varying(60) NOT NULL,
    seq_table character varying(40),
    seq_column character varying(40)
);


ALTER TABLE ts_sequence_seq OWNER TO postgres;

--
-- Name: vuemessage; Type: VIEW; Schema: ref; Owner: postgres
--

CREATE VIEW vuemessage AS
 SELECT msr.msr_id,
    msr.msr_element,
    msr.msr_number,
    msr.msr_type,
    msr.msr_endofline,
    msr.msr_comment,
    fr.mrl_textfrench,
    en.mrl_textenglish,
    sp.mrl_textspanish
   FROM (((ts_messager_msr msr
     LEFT JOIN ( SELECT ts_messagerlang_mrl.mrl_msr_id,
            ts_messagerlang_mrl.mrl_text AS mrl_textfrench
           FROM ts_messagerlang_mrl
          WHERE ((ts_messagerlang_mrl.mrl_lang)::text = 'French'::text)) fr ON ((fr.mrl_msr_id = msr.msr_id)))
     LEFT JOIN ( SELECT ts_messagerlang_mrl.mrl_msr_id,
            ts_messagerlang_mrl.mrl_text AS mrl_textenglish
           FROM ts_messagerlang_mrl
          WHERE ((ts_messagerlang_mrl.mrl_lang)::text = 'English'::text)) en ON ((en.mrl_msr_id = msr.msr_id)))
     LEFT JOIN ( SELECT ts_messagerlang_mrl.mrl_msr_id,
            ts_messagerlang_mrl.mrl_text AS mrl_textspanish
           FROM ts_messagerlang_mrl
          WHERE ((ts_messagerlang_mrl.mrl_lang)::text = 'Spanish'::text)) sp ON ((sp.mrl_msr_id = msr.msr_id)))
  ORDER BY msr.msr_type, msr.msr_element, msr.msr_number;


ALTER TABLE vuemessage OWNER TO postgres;

SET search_path = user_1, pg_catalog;

--
-- Name: t_bilanmigrationjournalier_bjo; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE t_bilanmigrationjournalier_bjo (
    bjo_identifiant integer NOT NULL,
    bjo_dis_identifiant integer NOT NULL,
    bjo_tax_code character varying(6) NOT NULL,
    bjo_std_code character varying(4) NOT NULL,
    bjo_annee integer NOT NULL,
    bjo_jour timestamp without time zone NOT NULL,
    bjo_labelquantite character varying(30),
    bjo_valeur double precision,
    bjo_horodateexport timestamp without time zone,
    bjo_org_code character varying(30) NOT NULL
);


ALTER TABLE t_bilanmigrationjournalier_bjo OWNER TO postgres;

--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: SEQUENCE; Schema: user_1; Owner: postgres
--

CREATE SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq OWNER TO postgres;

--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: postgres
--

ALTER SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq OWNED BY t_bilanmigrationjournalier_bjo.bjo_identifiant;


--
-- Name: t_bilanmigrationmensuel_bme; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE t_bilanmigrationmensuel_bme (
    bme_identifiant integer NOT NULL,
    bme_dis_identifiant integer NOT NULL,
    bme_tax_code character varying(6) NOT NULL,
    bme_std_code character varying(4) NOT NULL,
    bme_annee integer NOT NULL,
    bme_mois integer NOT NULL,
    bme_labelquantite character varying(30),
    bme_valeur double precision,
    bme_horodateexport timestamp without time zone,
    bme_org_code character varying(30) NOT NULL
);


ALTER TABLE t_bilanmigrationmensuel_bme OWNER TO postgres;

--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: SEQUENCE; Schema: user_1; Owner: postgres
--

CREATE SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_bilanmigrationmensuel_bme_bme_identifiant_seq OWNER TO postgres;

--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: postgres
--

ALTER SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq OWNED BY t_bilanmigrationmensuel_bme.bme_identifiant;


SET default_with_oids = true;

--
-- Name: t_dispositifcomptage_dic; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE t_dispositifcomptage_dic (
    dic_dis_identifiant integer NOT NULL,
    dic_dif_identifiant integer NOT NULL,
    dic_code character varying(16) NOT NULL,
    dic_tdc_code integer NOT NULL,
    dic_org_code character varying(30) NOT NULL
);


ALTER TABLE t_dispositifcomptage_dic OWNER TO postgres;

--
-- Name: t_dispositiffranchissement_dif; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE t_dispositiffranchissement_dif (
    dif_dis_identifiant integer NOT NULL,
    dif_ouv_identifiant integer NOT NULL,
    dif_code character varying(16) NOT NULL,
    dif_localisation text,
    dif_orientation character varying(20) NOT NULL,
    dif_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_dif_orientation CHECK (((upper((dif_orientation)::text) = 'DESCENTE'::text) OR (upper((dif_orientation)::text) = 'MONTEE'::text)))
);


ALTER TABLE t_dispositiffranchissement_dif OWNER TO postgres;

--
-- Name: t_lot_lot; Type: TABLE; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE TABLE t_lot_lot (
    lot_identifiant integer NOT NULL,
    lot_ope_identifiant integer NOT NULL,
    lot_tax_code character varying(6) NOT NULL,
    lot_std_code character varying(4) NOT NULL,
    lot_effectif double precision,
    lot_quantite double precision,
    lot_qte_code character varying(4),
    lot_methode_obtention character varying(10) NOT NULL,
    lot_lot_identifiant integer,
    lot_dev_code character varying(4),
    lot_commentaires text,
    lot_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_lot_lot_identifiant CHECK ((lot_lot_identifiant <> lot_identifiant)),
    CONSTRAINT c_ck_lot_methode_obtention CHECK (((((upper((lot_methode_obtention)::text) = 'MESURE'::text) OR (upper((lot_methode_obtention)::text) = 'CALCULE'::text)) OR (upper((lot_methode_obtention)::text) = 'EXPERT'::text)) OR (upper((lot_methode_obtention)::text) = 'PONCTUEL'::text)))
);


ALTER TABLE t_lot_lot OWNER TO iav;

--
-- Name: t_lot_lot_lot_identifiant_seq; Type: SEQUENCE; Schema: user_1; Owner: iav
--

CREATE SEQUENCE t_lot_lot_lot_identifiant_seq
    START WITH 175116
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_lot_lot_lot_identifiant_seq OWNER TO iav;

--
-- Name: t_lot_lot_lot_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: iav
--

ALTER SEQUENCE t_lot_lot_lot_identifiant_seq OWNED BY t_lot_lot.lot_identifiant;


--
-- Name: t_marque_mqe; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE t_marque_mqe (
    mqe_reference character varying(30) NOT NULL,
    mqe_loc_code character varying(4) NOT NULL,
    mqe_nmq_code character varying(4) NOT NULL,
    mqe_omq_reference character varying(30) NOT NULL,
    mqe_commentaires text,
    mqe_org_code character varying(30) NOT NULL
);


ALTER TABLE t_marque_mqe OWNER TO postgres;

--
-- Name: t_operation_ope; Type: TABLE; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE TABLE t_operation_ope (
    ope_identifiant integer NOT NULL,
    ope_dic_identifiant integer NOT NULL,
    ope_date_debut timestamp(0) without time zone NOT NULL,
    ope_date_fin timestamp(0) without time zone NOT NULL,
    ope_organisme character varying(35),
    ope_operateur character varying(35),
    ope_commentaires text,
    ope_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_ope_date_fin CHECK (((ope_date_fin >= ope_date_debut) AND (date_part('month'::text, age(ope_date_fin, ope_date_debut)) <= (1)::double precision)))
);


ALTER TABLE t_operation_ope OWNER TO iav;

--
-- Name: t_operation_ope_ope_identifiant_seq; Type: SEQUENCE; Schema: user_1; Owner: iav
--

CREATE SEQUENCE t_operation_ope_ope_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_operation_ope_ope_identifiant_seq OWNER TO iav;

--
-- Name: t_operation_ope_ope_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: iav
--

ALTER SEQUENCE t_operation_ope_ope_identifiant_seq OWNED BY t_operation_ope.ope_identifiant;


--
-- Name: t_operationmarquage_omq; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE t_operationmarquage_omq (
    omq_reference character varying(30) NOT NULL,
    omq_commentaires text,
    omq_org_code character varying(30) DEFAULT 'IAV'::character varying NOT NULL
);


ALTER TABLE t_operationmarquage_omq OWNER TO postgres;

--
-- Name: t_ouvrage_ouv; Type: TABLE; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE TABLE t_ouvrage_ouv (
    ouv_identifiant integer NOT NULL,
    ouv_sta_code character varying(8) NOT NULL,
    ouv_code character varying(12) NOT NULL,
    ouv_libelle character varying(40) NOT NULL,
    ouv_localisation text,
    ouv_coordonnee_x integer,
    ouv_coordonnee_y integer,
    ouv_altitude smallint,
    ouv_carte_localisation bytea,
    ouv_commentaires text,
    ouv_nov_code character varying(4) NOT NULL,
    ouv_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_ouv_altitude CHECK ((ouv_altitude >= 0)),
    CONSTRAINT c_ck_ouv_coordonnee_x CHECK (((ouv_coordonnee_x >= (-400000)) AND (ouv_coordonnee_x < 1300000))),
    CONSTRAINT c_ck_ouv_coordonnee_y CHECK (((ouv_coordonnee_y >= 6000000) AND (ouv_coordonnee_y < 7000000)))
);


ALTER TABLE t_ouvrage_ouv OWNER TO iav;

--
-- Name: COLUMN t_ouvrage_ouv.ouv_coordonnee_x; Type: COMMENT; Schema: user_1; Owner: iav
--

COMMENT ON COLUMN t_ouvrage_ouv.ouv_coordonnee_x IS 'Coordonnée x en Lambert 93';


--
-- Name: COLUMN t_ouvrage_ouv.ouv_coordonnee_y; Type: COMMENT; Schema: user_1; Owner: iav
--

COMMENT ON COLUMN t_ouvrage_ouv.ouv_coordonnee_y IS 'Coordonnée y en Lambert 93';


--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: SEQUENCE; Schema: user_1; Owner: iav
--

CREATE SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq
    START WITH 7
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_ouvrage_ouv_ouv_identifiant_seq OWNER TO iav;

--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: iav
--

ALTER SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq OWNED BY t_ouvrage_ouv.ouv_identifiant;


--
-- Name: t_periodefonctdispositif_per; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE t_periodefonctdispositif_per (
    per_dis_identifiant integer NOT NULL,
    per_date_debut timestamp(0) without time zone NOT NULL,
    per_date_fin timestamp(0) without time zone NOT NULL,
    per_commentaires text,
    per_etat_fonctionnement boolean NOT NULL,
    per_tar_code character varying(4),
    per_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_per_date_fin CHECK ((per_date_fin >= per_date_debut))
);


ALTER TABLE t_periodefonctdispositif_per OWNER TO postgres;

--
-- Name: t_station_sta; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE t_station_sta (
    sta_code character varying(8) NOT NULL,
    sta_nom character varying(40) NOT NULL,
    sta_localisation character varying(60),
    sta_coordonnee_x integer,
    sta_coordonnee_y integer,
    sta_altitude smallint,
    sta_carte_localisation bytea,
    sta_superficie integer,
    sta_distance_mer double precision,
    sta_date_creation date,
    sta_date_suppression date,
    sta_commentaires text,
    sta_dernier_import_conditions timestamp(0) without time zone,
    sta_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_sta_altitude CHECK ((sta_altitude >= 0)),
    CONSTRAINT c_ck_sta_coordonnee_x CHECK (((sta_coordonnee_x >= (-400000)) AND (sta_coordonnee_x < 1300000))),
    CONSTRAINT c_ck_sta_coordonnee_y CHECK (((sta_coordonnee_y >= 6000000) AND (sta_coordonnee_y < 7000000))),
    CONSTRAINT c_ck_sta_date_suppression CHECK ((sta_date_suppression >= sta_date_creation)),
    CONSTRAINT c_ck_sta_distance_mer CHECK ((sta_distance_mer >= (0)::double precision)),
    CONSTRAINT c_ck_sta_superficie CHECK ((sta_superficie >= 0))
);


ALTER TABLE t_station_sta OWNER TO postgres;

--
-- Name: COLUMN t_station_sta.sta_coordonnee_x; Type: COMMENT; Schema: user_1; Owner: postgres
--

COMMENT ON COLUMN t_station_sta.sta_coordonnee_x IS 'Coordonnée x en Lambert 93';


--
-- Name: COLUMN t_station_sta.sta_coordonnee_y; Type: COMMENT; Schema: user_1; Owner: postgres
--

COMMENT ON COLUMN t_station_sta.sta_coordonnee_y IS 'Coordonnée y en Lambert 93';


--
-- Name: tg_dispositif_dis; Type: TABLE; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE TABLE tg_dispositif_dis (
    dis_identifiant integer NOT NULL,
    dis_date_creation date,
    dis_date_suppression date,
    dis_commentaires text,
    dis_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_dis_date_suppression CHECK ((dis_date_suppression >= dis_date_creation))
);


ALTER TABLE tg_dispositif_dis OWNER TO iav;

--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: SEQUENCE; Schema: user_1; Owner: iav
--

CREATE SEQUENCE tg_dispositif_dis_dis_identifiant_seq
    START WITH 19
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE tg_dispositif_dis_dis_identifiant_seq OWNER TO iav;

--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: iav
--

ALTER SEQUENCE tg_dispositif_dis_dis_identifiant_seq OWNED BY tg_dispositif_dis.dis_identifiant;


--
-- Name: tj_actionmarquage_act; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_actionmarquage_act (
    act_lot_identifiant integer NOT NULL,
    act_mqe_reference character varying(30) NOT NULL,
    act_action character varying(20) NOT NULL,
    act_commentaires text,
    act_org_code character varying(30) NOT NULL,
    act_nmq_code character varying(4) NOT NULL,
    CONSTRAINT c_ck_act_action CHECK ((((upper((act_action)::text) = 'POSE'::text) OR (upper((act_action)::text) = 'LECTURE'::text)) OR (upper((act_action)::text) = 'RETRAIT'::text)))
);


ALTER TABLE tj_actionmarquage_act OWNER TO postgres;

--
-- Name: tj_caracteristiquelot_car; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_caracteristiquelot_car (
    car_lot_identifiant integer NOT NULL,
    car_par_code character varying(5) NOT NULL,
    car_methode_obtention character varying(10),
    car_val_identifiant integer,
    car_valeur_quantitatif real,
    car_precision real,
    car_commentaires text,
    car_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_car CHECK ((((car_val_identifiant IS NOT NULL) AND (car_valeur_quantitatif IS NULL)) OR ((car_val_identifiant IS NULL) AND (car_valeur_quantitatif IS NOT NULL)))),
    CONSTRAINT c_ck_car_methode_obtention CHECK ((((upper((car_methode_obtention)::text) = 'MESURE'::text) OR (upper((car_methode_obtention)::text) = 'CALCULE'::text)) OR (upper((car_methode_obtention)::text) = 'EXPERT'::text)))
);


ALTER TABLE tj_caracteristiquelot_car OWNER TO postgres;

--
-- Name: tj_coefficientconversion_coe; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_coefficientconversion_coe (
    coe_tax_code character varying(6) NOT NULL,
    coe_std_code character varying(4) NOT NULL,
    coe_qte_code character varying(4) NOT NULL,
    coe_date_debut date NOT NULL,
    coe_date_fin date NOT NULL,
    coe_valeur_coefficient real,
    coe_commentaires text,
    coe_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_coe_date_fin CHECK ((coe_date_fin >= coe_date_debut)),
    CONSTRAINT c_nn_coe_org_code CHECK ((coe_org_code IS NOT NULL))
);


ALTER TABLE tj_coefficientconversion_coe OWNER TO postgres;

--
-- Name: tj_conditionenvironnementale_env; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_conditionenvironnementale_env (
    env_date_debut timestamp(0) without time zone NOT NULL,
    env_date_fin timestamp(0) without time zone NOT NULL,
    env_methode_obtention character varying(10),
    env_val_identifiant integer,
    env_valeur_quantitatif real,
    env_stm_identifiant integer NOT NULL,
    env_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_env CHECK ((((env_val_identifiant IS NOT NULL) AND (env_valeur_quantitatif IS NULL)) OR ((env_val_identifiant IS NULL) AND (env_valeur_quantitatif IS NOT NULL)))),
    CONSTRAINT c_ck_env_date_fin CHECK ((env_date_fin >= env_date_debut)),
    CONSTRAINT c_ck_env_methode_obtention CHECK (((upper((env_methode_obtention)::text) = 'MESURE'::text) OR (upper((env_methode_obtention)::text) = 'CALCULE'::text)))
);


ALTER TABLE tj_conditionenvironnementale_env OWNER TO postgres;

--
-- Name: tj_dfestdestinea_dtx; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_dfestdestinea_dtx (
    dtx_dif_identifiant integer NOT NULL,
    dtx_tax_code character varying(6) NOT NULL,
    dtx_org_code character varying(30) NOT NULL
);


ALTER TABLE tj_dfestdestinea_dtx OWNER TO postgres;

--
-- Name: tj_dfesttype_dft; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_dfesttype_dft (
    dft_df_identifiant integer NOT NULL,
    dft_rang smallint NOT NULL,
    dft_org_code character varying(30) NOT NULL,
    dft_tdf_code character varying(2) NOT NULL,
    CONSTRAINT c_ck_dft_rang CHECK ((dft_rang >= 0))
);


ALTER TABLE tj_dfesttype_dft OWNER TO postgres;

--
-- Name: tj_pathologieconstatee_pco; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_pathologieconstatee_pco (
    pco_lot_identifiant integer NOT NULL,
    pco_pat_code character varying(4) NOT NULL,
    pco_loc_code character varying(4) NOT NULL,
    pco_commentaires text,
    pco_org_code character varying(30) NOT NULL,
    pco_imp_code integer
);


ALTER TABLE tj_pathologieconstatee_pco OWNER TO postgres;

SET default_with_oids = false;

--
-- Name: tj_prelevementlot_prl; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_prelevementlot_prl (
    prl_pre_typeprelevement character varying(15) NOT NULL,
    prl_lot_identifiant integer NOT NULL,
    prl_code character varying(12) NOT NULL,
    prl_operateur character varying(35),
    prl_loc_code character varying(4),
    prl_commentaires text,
    prl_org_code character varying(30) NOT NULL
);


ALTER TABLE tj_prelevementlot_prl OWNER TO postgres;

--
-- Name: tj_stationmesure_stm; Type: TABLE; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE TABLE tj_stationmesure_stm (
    stm_identifiant integer NOT NULL,
    stm_libelle character varying(12),
    stm_sta_code character varying(8) NOT NULL,
    stm_par_code character varying(5) NOT NULL,
    stm_description text,
    stm_org_code character varying(30) NOT NULL
);


ALTER TABLE tj_stationmesure_stm OWNER TO iav;

--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: SEQUENCE; Schema: user_1; Owner: iav
--

CREATE SEQUENCE tj_stationmesure_stm_stm_identifiant_seq
    START WITH 19
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE tj_stationmesure_stm_stm_identifiant_seq OWNER TO iav;

--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: iav
--

ALTER SEQUENCE tj_stationmesure_stm_stm_identifiant_seq OWNED BY tj_stationmesure_stm.stm_identifiant;


SET default_with_oids = true;

--
-- Name: tj_tauxechappement_txe; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_tauxechappement_txe (
    txe_tax_code character varying(6) NOT NULL,
    txe_std_code character varying(4) NOT NULL,
    txe_date_debut timestamp(0) without time zone NOT NULL,
    txe_date_fin timestamp(0) without time zone NOT NULL,
    txe_methode_estimation text,
    txe_ech_code character varying(4),
    txe_valeur_taux smallint,
    txe_commentaires text,
    txe_org_code character varying(30) NOT NULL,
    txe_sta_code character varying(8) NOT NULL,
    CONSTRAINT c_ck_txe_date_fin CHECK ((txe_date_fin >= txe_date_debut)),
    CONSTRAINT c_ck_txe_methode_estimation CHECK ((((upper(txe_methode_estimation) = 'MESURE'::text) OR (upper(txe_methode_estimation) = 'CALCULE'::text)) OR (upper(txe_methode_estimation) = 'EXPERT'::text))),
    CONSTRAINT c_ck_txe_valeur_taux CHECK ((((txe_valeur_taux >= 0) AND (txe_valeur_taux <= 100)) OR ((txe_valeur_taux IS NULL) AND (txe_ech_code IS NOT NULL))))
);


ALTER TABLE tj_tauxechappement_txe OWNER TO postgres;

SET default_with_oids = false;

--
-- Name: ts_maintenance_main; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_maintenance_main (
    main_identifiant integer NOT NULL,
    main_ticket integer,
    main_description text
);


ALTER TABLE ts_maintenance_main OWNER TO postgres;

--
-- Name: TABLE ts_maintenance_main; Type: COMMENT; Schema: user_1; Owner: postgres
--

COMMENT ON TABLE ts_maintenance_main IS 'Table de suivi des operations de maintenance de la base';


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE; Schema: user_1; Owner: postgres
--

CREATE SEQUENCE ts_maintenance_main_main_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_maintenance_main_main_identifiant_seq OWNER TO postgres;

--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: postgres
--

ALTER SEQUENCE ts_maintenance_main_main_identifiant_seq OWNED BY ts_maintenance_main.main_identifiant;


--
-- Name: ts_masque_mas; Type: TABLE; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE TABLE ts_masque_mas (
    mas_id integer NOT NULL,
    mas_code character varying(15) NOT NULL,
    mas_description text,
    mas_raccourci text,
    mas_type character varying(3),
    CONSTRAINT c_ck_mas_type CHECK ((((mas_type)::text = 'lot'::text) OR ((mas_type)::text = 'ope'::text)))
);


ALTER TABLE ts_masque_mas OWNER TO iav;

--
-- Name: ts_masque_mas_mas_id_seq; Type: SEQUENCE; Schema: user_1; Owner: iav
--

CREATE SEQUENCE ts_masque_mas_mas_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_masque_mas_mas_id_seq OWNER TO iav;

--
-- Name: ts_masque_mas_mas_id_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: iav
--

ALTER SEQUENCE ts_masque_mas_mas_id_seq OWNED BY ts_masque_mas.mas_id;


--
-- Name: ts_masquecaracteristiquelot_mac; Type: TABLE; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE TABLE ts_masquecaracteristiquelot_mac (
    mac_id integer NOT NULL,
    mac_mal_id integer NOT NULL,
    mac_par_code character varying(5) NOT NULL,
    mac_affichagevaleur boolean,
    mac_affichageprecision boolean,
    mac_affichagemethodeobtention boolean,
    mac_affichagecommentaire boolean,
    mac_valeurquantitatifdefaut numeric,
    mac_valeurqualitatifdefaut integer,
    mac_precisiondefaut numeric,
    mac_methodeobtentiondefaut character varying(10),
    mac_commentairedefaut text
);


ALTER TABLE ts_masquecaracteristiquelot_mac OWNER TO iav;

--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: SEQUENCE; Schema: user_1; Owner: iav
--

CREATE SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_masquecaracteristiquelot_mac_mac_id_seq OWNER TO iav;

--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: iav
--

ALTER SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq OWNED BY ts_masquecaracteristiquelot_mac.mac_id;


--
-- Name: ts_masqueconditionsenvironnementales_mae; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_masqueconditionsenvironnementales_mae (
    mae_mao_id integer NOT NULL,
    mae_stm_identifiant integer NOT NULL,
    mae_affichage boolean,
    mae_valeurdefaut text
);


ALTER TABLE ts_masqueconditionsenvironnementales_mae OWNER TO postgres;

--
-- Name: ts_masquelot_mal; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_masquelot_mal (
    mal_mas_id integer NOT NULL,
    mal_affichage boolean[],
    mal_valeurdefaut text[],
    CONSTRAINT c_ck_length_mal_affichage CHECK ((array_length(mal_affichage, 1) = 21)),
    CONSTRAINT c_ck_length_mal_valeurdefaut CHECK ((array_length(mal_valeurdefaut, 1) = 14))
);


ALTER TABLE ts_masquelot_mal OWNER TO postgres;

--
-- Name: ts_masqueope_mao; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_masqueope_mao (
    mao_mas_id integer NOT NULL,
    mao_affichage boolean[],
    mao_valeurdefaut text[],
    CONSTRAINT c_ck_length_mao_affichage CHECK ((array_length(mao_affichage, 1) = 16)),
    CONSTRAINT c_ck_length_mao_valeurdefaut CHECK ((array_length(mao_valeurdefaut, 1) = 9))
);


ALTER TABLE ts_masqueope_mao OWNER TO postgres;

--
-- Name: ts_masqueordreaffichage_maa; Type: TABLE; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE TABLE ts_masqueordreaffichage_maa (
    maa_id integer NOT NULL,
    maa_mal_id integer,
    maa_table character varying(40) NOT NULL,
    maa_valeur text NOT NULL,
    maa_champdumasque character varying(30),
    maa_rang integer
);


ALTER TABLE ts_masqueordreaffichage_maa OWNER TO iav;

--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: SEQUENCE; Schema: user_1; Owner: iav
--

CREATE SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_masqueordreaffichage_maa_maa_id_seq OWNER TO iav;

--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: SEQUENCE OWNED BY; Schema: user_1; Owner: iav
--

ALTER SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq OWNED BY ts_masqueordreaffichage_maa.maa_id;


SET default_with_oids = true;

--
-- Name: ts_taillevideo_tav; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_taillevideo_tav (
    tav_dic_identifiant integer NOT NULL,
    tav_coefconversion numeric NOT NULL,
    tav_distance character varying(3) NOT NULL,
    tav_org_code character varying(30) NOT NULL
);


ALTER TABLE ts_taillevideo_tav OWNER TO postgres;

--
-- Name: ts_taxonvideo_txv; Type: TABLE; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_taxonvideo_txv (
    txv_code character varying(3) NOT NULL,
    txv_tax_code character varying(6),
    txv_std_code character varying(4),
    txv_org_code character varying(30) NOT NULL
);


ALTER TABLE ts_taxonvideo_txv OWNER TO postgres;

SET search_path = user_2, pg_catalog;

SET default_with_oids = false;

--
-- Name: t_bilanmigrationjournalier_bjo; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE t_bilanmigrationjournalier_bjo (
    bjo_identifiant integer NOT NULL,
    bjo_dis_identifiant integer NOT NULL,
    bjo_tax_code character varying(6) NOT NULL,
    bjo_std_code character varying(4) NOT NULL,
    bjo_annee integer NOT NULL,
    bjo_jour timestamp without time zone NOT NULL,
    bjo_labelquantite character varying(30),
    bjo_valeur double precision,
    bjo_horodateexport timestamp without time zone,
    bjo_org_code character varying(30) NOT NULL
);


ALTER TABLE t_bilanmigrationjournalier_bjo OWNER TO postgres;

--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: SEQUENCE; Schema: user_2; Owner: postgres
--

CREATE SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq OWNER TO postgres;

--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: postgres
--

ALTER SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq OWNED BY t_bilanmigrationjournalier_bjo.bjo_identifiant;


--
-- Name: t_bilanmigrationmensuel_bme; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE t_bilanmigrationmensuel_bme (
    bme_identifiant integer NOT NULL,
    bme_dis_identifiant integer NOT NULL,
    bme_tax_code character varying(6) NOT NULL,
    bme_std_code character varying(4) NOT NULL,
    bme_annee integer NOT NULL,
    bme_mois integer NOT NULL,
    bme_labelquantite character varying(30),
    bme_valeur double precision,
    bme_horodateexport timestamp without time zone,
    bme_org_code character varying(30) NOT NULL
);


ALTER TABLE t_bilanmigrationmensuel_bme OWNER TO postgres;

--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: SEQUENCE; Schema: user_2; Owner: postgres
--

CREATE SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_bilanmigrationmensuel_bme_bme_identifiant_seq OWNER TO postgres;

--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: postgres
--

ALTER SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq OWNED BY t_bilanmigrationmensuel_bme.bme_identifiant;


SET default_with_oids = true;

--
-- Name: t_dispositifcomptage_dic; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE t_dispositifcomptage_dic (
    dic_dis_identifiant integer NOT NULL,
    dic_dif_identifiant integer NOT NULL,
    dic_code character varying(16) NOT NULL,
    dic_tdc_code integer NOT NULL,
    dic_org_code character varying(30) NOT NULL
);


ALTER TABLE t_dispositifcomptage_dic OWNER TO postgres;

--
-- Name: t_dispositiffranchissement_dif; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE t_dispositiffranchissement_dif (
    dif_dis_identifiant integer NOT NULL,
    dif_ouv_identifiant integer NOT NULL,
    dif_code character varying(16) NOT NULL,
    dif_localisation text,
    dif_orientation character varying(20) NOT NULL,
    dif_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_dif_orientation CHECK (((upper((dif_orientation)::text) = 'DESCENTE'::text) OR (upper((dif_orientation)::text) = 'MONTEE'::text)))
);


ALTER TABLE t_dispositiffranchissement_dif OWNER TO postgres;

--
-- Name: t_lot_lot; Type: TABLE; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE TABLE t_lot_lot (
    lot_identifiant integer NOT NULL,
    lot_ope_identifiant integer NOT NULL,
    lot_tax_code character varying(6) NOT NULL,
    lot_std_code character varying(4) NOT NULL,
    lot_effectif double precision,
    lot_quantite double precision,
    lot_qte_code character varying(4),
    lot_methode_obtention character varying(10) NOT NULL,
    lot_lot_identifiant integer,
    lot_dev_code character varying(4),
    lot_commentaires text,
    lot_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_lot_lot_identifiant CHECK ((lot_lot_identifiant <> lot_identifiant)),
    CONSTRAINT c_ck_lot_methode_obtention CHECK (((((upper((lot_methode_obtention)::text) = 'MESURE'::text) OR (upper((lot_methode_obtention)::text) = 'CALCULE'::text)) OR (upper((lot_methode_obtention)::text) = 'EXPERT'::text)) OR (upper((lot_methode_obtention)::text) = 'PONCTUEL'::text)))
);


ALTER TABLE t_lot_lot OWNER TO iav;

--
-- Name: t_lot_lot_lot_identifiant_seq; Type: SEQUENCE; Schema: user_2; Owner: iav
--

CREATE SEQUENCE t_lot_lot_lot_identifiant_seq
    START WITH 175116
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_lot_lot_lot_identifiant_seq OWNER TO iav;

--
-- Name: t_lot_lot_lot_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: iav
--

ALTER SEQUENCE t_lot_lot_lot_identifiant_seq OWNED BY t_lot_lot.lot_identifiant;


--
-- Name: t_marque_mqe; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE t_marque_mqe (
    mqe_reference character varying(30) NOT NULL,
    mqe_loc_code character varying(4) NOT NULL,
    mqe_nmq_code character varying(4) NOT NULL,
    mqe_omq_reference character varying(30) NOT NULL,
    mqe_commentaires text,
    mqe_org_code character varying(30) NOT NULL
);


ALTER TABLE t_marque_mqe OWNER TO postgres;

--
-- Name: t_operation_ope; Type: TABLE; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE TABLE t_operation_ope (
    ope_identifiant integer NOT NULL,
    ope_dic_identifiant integer NOT NULL,
    ope_date_debut timestamp(0) without time zone NOT NULL,
    ope_date_fin timestamp(0) without time zone NOT NULL,
    ope_organisme character varying(35),
    ope_operateur character varying(35),
    ope_commentaires text,
    ope_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_ope_date_fin CHECK (((ope_date_fin >= ope_date_debut) AND (date_part('month'::text, age(ope_date_fin, ope_date_debut)) <= (1)::double precision)))
);


ALTER TABLE t_operation_ope OWNER TO iav;

--
-- Name: t_operation_ope_ope_identifiant_seq; Type: SEQUENCE; Schema: user_2; Owner: iav
--

CREATE SEQUENCE t_operation_ope_ope_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_operation_ope_ope_identifiant_seq OWNER TO iav;

--
-- Name: t_operation_ope_ope_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: iav
--

ALTER SEQUENCE t_operation_ope_ope_identifiant_seq OWNED BY t_operation_ope.ope_identifiant;


--
-- Name: t_operationmarquage_omq; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE t_operationmarquage_omq (
    omq_reference character varying(30) NOT NULL,
    omq_commentaires text,
    omq_org_code character varying(30) DEFAULT 'IAV'::character varying NOT NULL
);


ALTER TABLE t_operationmarquage_omq OWNER TO postgres;

--
-- Name: t_ouvrage_ouv; Type: TABLE; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE TABLE t_ouvrage_ouv (
    ouv_identifiant integer NOT NULL,
    ouv_sta_code character varying(8) NOT NULL,
    ouv_code character varying(12) NOT NULL,
    ouv_libelle character varying(40) NOT NULL,
    ouv_localisation text,
    ouv_coordonnee_x integer,
    ouv_coordonnee_y integer,
    ouv_altitude smallint,
    ouv_carte_localisation bytea,
    ouv_commentaires text,
    ouv_nov_code character varying(4) NOT NULL,
    ouv_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_ouv_altitude CHECK ((ouv_altitude >= 0)),
    CONSTRAINT c_ck_ouv_coordonnee_x CHECK (((ouv_coordonnee_x >= (-400000)) AND (ouv_coordonnee_x < 1300000))),
    CONSTRAINT c_ck_ouv_coordonnee_y CHECK (((ouv_coordonnee_y >= 6000000) AND (ouv_coordonnee_y < 7000000)))
);


ALTER TABLE t_ouvrage_ouv OWNER TO iav;

--
-- Name: COLUMN t_ouvrage_ouv.ouv_coordonnee_x; Type: COMMENT; Schema: user_2; Owner: iav
--

COMMENT ON COLUMN t_ouvrage_ouv.ouv_coordonnee_x IS 'Coordonnée x en Lambert 93';


--
-- Name: COLUMN t_ouvrage_ouv.ouv_coordonnee_y; Type: COMMENT; Schema: user_2; Owner: iav
--

COMMENT ON COLUMN t_ouvrage_ouv.ouv_coordonnee_y IS 'Coordonnée y en Lambert 93';


--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: SEQUENCE; Schema: user_2; Owner: iav
--

CREATE SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq
    START WITH 7
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE t_ouvrage_ouv_ouv_identifiant_seq OWNER TO iav;

--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: iav
--

ALTER SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq OWNED BY t_ouvrage_ouv.ouv_identifiant;


--
-- Name: t_periodefonctdispositif_per; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE t_periodefonctdispositif_per (
    per_dis_identifiant integer NOT NULL,
    per_date_debut timestamp(0) without time zone NOT NULL,
    per_date_fin timestamp(0) without time zone NOT NULL,
    per_commentaires text,
    per_etat_fonctionnement boolean NOT NULL,
    per_tar_code character varying(4),
    per_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_per_date_fin CHECK ((per_date_fin >= per_date_debut))
);


ALTER TABLE t_periodefonctdispositif_per OWNER TO postgres;

--
-- Name: t_station_sta; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE t_station_sta (
    sta_code character varying(8) NOT NULL,
    sta_nom character varying(40) NOT NULL,
    sta_localisation character varying(60),
    sta_coordonnee_x integer,
    sta_coordonnee_y integer,
    sta_altitude smallint,
    sta_carte_localisation bytea,
    sta_superficie integer,
    sta_distance_mer double precision,
    sta_date_creation date,
    sta_date_suppression date,
    sta_commentaires text,
    sta_dernier_import_conditions timestamp(0) without time zone,
    sta_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_sta_altitude CHECK ((sta_altitude >= 0)),
    CONSTRAINT c_ck_sta_coordonnee_x CHECK (((sta_coordonnee_x >= (-400000)) AND (sta_coordonnee_x < 1300000))),
    CONSTRAINT c_ck_sta_coordonnee_y CHECK (((sta_coordonnee_y >= 6000000) AND (sta_coordonnee_y < 7000000))),
    CONSTRAINT c_ck_sta_date_suppression CHECK ((sta_date_suppression >= sta_date_creation)),
    CONSTRAINT c_ck_sta_distance_mer CHECK ((sta_distance_mer >= (0)::double precision)),
    CONSTRAINT c_ck_sta_superficie CHECK ((sta_superficie >= 0))
);


ALTER TABLE t_station_sta OWNER TO postgres;

--
-- Name: COLUMN t_station_sta.sta_coordonnee_x; Type: COMMENT; Schema: user_2; Owner: postgres
--

COMMENT ON COLUMN t_station_sta.sta_coordonnee_x IS 'Coordonnée x en Lambert 93';


--
-- Name: COLUMN t_station_sta.sta_coordonnee_y; Type: COMMENT; Schema: user_2; Owner: postgres
--

COMMENT ON COLUMN t_station_sta.sta_coordonnee_y IS 'Coordonnée y en Lambert 93';


--
-- Name: tg_dispositif_dis; Type: TABLE; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE TABLE tg_dispositif_dis (
    dis_identifiant integer NOT NULL,
    dis_date_creation date,
    dis_date_suppression date,
    dis_commentaires text,
    dis_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_dis_date_suppression CHECK ((dis_date_suppression >= dis_date_creation))
);


ALTER TABLE tg_dispositif_dis OWNER TO iav;

--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: SEQUENCE; Schema: user_2; Owner: iav
--

CREATE SEQUENCE tg_dispositif_dis_dis_identifiant_seq
    START WITH 19
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE tg_dispositif_dis_dis_identifiant_seq OWNER TO iav;

--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: iav
--

ALTER SEQUENCE tg_dispositif_dis_dis_identifiant_seq OWNED BY tg_dispositif_dis.dis_identifiant;


--
-- Name: tj_actionmarquage_act; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_actionmarquage_act (
    act_lot_identifiant integer NOT NULL,
    act_mqe_reference character varying(30) NOT NULL,
    act_action character varying(20) NOT NULL,
    act_commentaires text,
    act_org_code character varying(30) NOT NULL,
    act_nmq_code character varying(4) NOT NULL,
    CONSTRAINT c_ck_act_action CHECK ((((upper((act_action)::text) = 'POSE'::text) OR (upper((act_action)::text) = 'LECTURE'::text)) OR (upper((act_action)::text) = 'RETRAIT'::text)))
);


ALTER TABLE tj_actionmarquage_act OWNER TO postgres;

--
-- Name: tj_caracteristiquelot_car; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_caracteristiquelot_car (
    car_lot_identifiant integer NOT NULL,
    car_par_code character varying(5) NOT NULL,
    car_methode_obtention character varying(10),
    car_val_identifiant integer,
    car_valeur_quantitatif real,
    car_precision real,
    car_commentaires text,
    car_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_car CHECK ((((car_val_identifiant IS NOT NULL) AND (car_valeur_quantitatif IS NULL)) OR ((car_val_identifiant IS NULL) AND (car_valeur_quantitatif IS NOT NULL)))),
    CONSTRAINT c_ck_car_methode_obtention CHECK ((((upper((car_methode_obtention)::text) = 'MESURE'::text) OR (upper((car_methode_obtention)::text) = 'CALCULE'::text)) OR (upper((car_methode_obtention)::text) = 'EXPERT'::text)))
);


ALTER TABLE tj_caracteristiquelot_car OWNER TO postgres;

--
-- Name: tj_coefficientconversion_coe; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_coefficientconversion_coe (
    coe_tax_code character varying(6) NOT NULL,
    coe_std_code character varying(4) NOT NULL,
    coe_qte_code character varying(4) NOT NULL,
    coe_date_debut date NOT NULL,
    coe_date_fin date NOT NULL,
    coe_valeur_coefficient real,
    coe_commentaires text,
    coe_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_coe_date_fin CHECK ((coe_date_fin >= coe_date_debut)),
    CONSTRAINT c_nn_coe_org_code CHECK ((coe_org_code IS NOT NULL))
);


ALTER TABLE tj_coefficientconversion_coe OWNER TO postgres;

--
-- Name: tj_conditionenvironnementale_env; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_conditionenvironnementale_env (
    env_date_debut timestamp(0) without time zone NOT NULL,
    env_date_fin timestamp(0) without time zone NOT NULL,
    env_methode_obtention character varying(10),
    env_val_identifiant integer,
    env_valeur_quantitatif real,
    env_stm_identifiant integer NOT NULL,
    env_org_code character varying(30) NOT NULL,
    CONSTRAINT c_ck_env CHECK ((((env_val_identifiant IS NOT NULL) AND (env_valeur_quantitatif IS NULL)) OR ((env_val_identifiant IS NULL) AND (env_valeur_quantitatif IS NOT NULL)))),
    CONSTRAINT c_ck_env_date_fin CHECK ((env_date_fin >= env_date_debut)),
    CONSTRAINT c_ck_env_methode_obtention CHECK (((upper((env_methode_obtention)::text) = 'MESURE'::text) OR (upper((env_methode_obtention)::text) = 'CALCULE'::text)))
);


ALTER TABLE tj_conditionenvironnementale_env OWNER TO postgres;

--
-- Name: tj_dfestdestinea_dtx; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_dfestdestinea_dtx (
    dtx_dif_identifiant integer NOT NULL,
    dtx_tax_code character varying(6) NOT NULL,
    dtx_org_code character varying(30) NOT NULL
);


ALTER TABLE tj_dfestdestinea_dtx OWNER TO postgres;

--
-- Name: tj_dfesttype_dft; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_dfesttype_dft (
    dft_df_identifiant integer NOT NULL,
    dft_rang smallint NOT NULL,
    dft_org_code character varying(30) NOT NULL,
    dft_tdf_code character varying(2) NOT NULL,
    CONSTRAINT c_ck_dft_rang CHECK ((dft_rang >= 0))
);


ALTER TABLE tj_dfesttype_dft OWNER TO postgres;

--
-- Name: tj_pathologieconstatee_pco; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_pathologieconstatee_pco (
    pco_lot_identifiant integer NOT NULL,
    pco_pat_code character varying(4) NOT NULL,
    pco_loc_code character varying(4) NOT NULL,
    pco_commentaires text,
    pco_org_code character varying(30) NOT NULL,
    pco_imp_code integer
);


ALTER TABLE tj_pathologieconstatee_pco OWNER TO postgres;

SET default_with_oids = false;

--
-- Name: tj_prelevementlot_prl; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_prelevementlot_prl (
    prl_pre_typeprelevement character varying(15) NOT NULL,
    prl_lot_identifiant integer NOT NULL,
    prl_code character varying(12) NOT NULL,
    prl_operateur character varying(35),
    prl_loc_code character varying(4),
    prl_commentaires text,
    prl_org_code character varying(30) NOT NULL
);


ALTER TABLE tj_prelevementlot_prl OWNER TO postgres;

--
-- Name: tj_stationmesure_stm; Type: TABLE; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE TABLE tj_stationmesure_stm (
    stm_identifiant integer NOT NULL,
    stm_libelle character varying(12),
    stm_sta_code character varying(8) NOT NULL,
    stm_par_code character varying(5) NOT NULL,
    stm_description text,
    stm_org_code character varying(30) NOT NULL
);


ALTER TABLE tj_stationmesure_stm OWNER TO iav;

--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: SEQUENCE; Schema: user_2; Owner: iav
--

CREATE SEQUENCE tj_stationmesure_stm_stm_identifiant_seq
    START WITH 19
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE tj_stationmesure_stm_stm_identifiant_seq OWNER TO iav;

--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: iav
--

ALTER SEQUENCE tj_stationmesure_stm_stm_identifiant_seq OWNED BY tj_stationmesure_stm.stm_identifiant;


SET default_with_oids = true;

--
-- Name: tj_tauxechappement_txe; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE tj_tauxechappement_txe (
    txe_tax_code character varying(6) NOT NULL,
    txe_std_code character varying(4) NOT NULL,
    txe_date_debut timestamp(0) without time zone NOT NULL,
    txe_date_fin timestamp(0) without time zone NOT NULL,
    txe_methode_estimation text,
    txe_ech_code character varying(4),
    txe_valeur_taux smallint,
    txe_commentaires text,
    txe_org_code character varying(30) NOT NULL,
    txe_sta_code character varying(8) NOT NULL,
    CONSTRAINT c_ck_txe_date_fin CHECK ((txe_date_fin >= txe_date_debut)),
    CONSTRAINT c_ck_txe_methode_estimation CHECK ((((upper(txe_methode_estimation) = 'MESURE'::text) OR (upper(txe_methode_estimation) = 'CALCULE'::text)) OR (upper(txe_methode_estimation) = 'EXPERT'::text))),
    CONSTRAINT c_ck_txe_valeur_taux CHECK ((((txe_valeur_taux >= 0) AND (txe_valeur_taux <= 100)) OR ((txe_valeur_taux IS NULL) AND (txe_ech_code IS NOT NULL))))
);


ALTER TABLE tj_tauxechappement_txe OWNER TO postgres;

SET default_with_oids = false;

--
-- Name: ts_maintenance_main; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_maintenance_main (
    main_identifiant integer NOT NULL,
    main_ticket integer,
    main_description text
);


ALTER TABLE ts_maintenance_main OWNER TO postgres;

--
-- Name: TABLE ts_maintenance_main; Type: COMMENT; Schema: user_2; Owner: postgres
--

COMMENT ON TABLE ts_maintenance_main IS 'Table de suivi des operations de maintenance de la base';


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE; Schema: user_2; Owner: postgres
--

CREATE SEQUENCE ts_maintenance_main_main_identifiant_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_maintenance_main_main_identifiant_seq OWNER TO postgres;

--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: postgres
--

ALTER SEQUENCE ts_maintenance_main_main_identifiant_seq OWNED BY ts_maintenance_main.main_identifiant;


--
-- Name: ts_masque_mas; Type: TABLE; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE TABLE ts_masque_mas (
    mas_id integer NOT NULL,
    mas_code character varying(15) NOT NULL,
    mas_description text,
    mas_raccourci text,
    mas_type character varying(3),
    CONSTRAINT c_ck_mas_type CHECK ((((mas_type)::text = 'lot'::text) OR ((mas_type)::text = 'ope'::text)))
);


ALTER TABLE ts_masque_mas OWNER TO iav;

--
-- Name: ts_masque_mas_mas_id_seq; Type: SEQUENCE; Schema: user_2; Owner: iav
--

CREATE SEQUENCE ts_masque_mas_mas_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_masque_mas_mas_id_seq OWNER TO iav;

--
-- Name: ts_masque_mas_mas_id_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: iav
--

ALTER SEQUENCE ts_masque_mas_mas_id_seq OWNED BY ts_masque_mas.mas_id;


--
-- Name: ts_masquecaracteristiquelot_mac; Type: TABLE; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE TABLE ts_masquecaracteristiquelot_mac (
    mac_id integer NOT NULL,
    mac_mal_id integer NOT NULL,
    mac_par_code character varying(5) NOT NULL,
    mac_affichagevaleur boolean,
    mac_affichageprecision boolean,
    mac_affichagemethodeobtention boolean,
    mac_affichagecommentaire boolean,
    mac_valeurquantitatifdefaut numeric,
    mac_valeurqualitatifdefaut integer,
    mac_precisiondefaut numeric,
    mac_methodeobtentiondefaut character varying(10),
    mac_commentairedefaut text
);


ALTER TABLE ts_masquecaracteristiquelot_mac OWNER TO iav;

--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: SEQUENCE; Schema: user_2; Owner: iav
--

CREATE SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_masquecaracteristiquelot_mac_mac_id_seq OWNER TO iav;

--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: iav
--

ALTER SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq OWNED BY ts_masquecaracteristiquelot_mac.mac_id;


--
-- Name: ts_masqueconditionsenvironnementales_mae; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_masqueconditionsenvironnementales_mae (
    mae_mao_id integer NOT NULL,
    mae_stm_identifiant integer NOT NULL,
    mae_affichage boolean,
    mae_valeurdefaut text
);


ALTER TABLE ts_masqueconditionsenvironnementales_mae OWNER TO postgres;

--
-- Name: ts_masquelot_mal; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_masquelot_mal (
    mal_mas_id integer NOT NULL,
    mal_affichage boolean[],
    mal_valeurdefaut text[],
    CONSTRAINT c_ck_length_mal_affichage CHECK ((array_length(mal_affichage, 1) = 21)),
    CONSTRAINT c_ck_length_mal_valeurdefaut CHECK ((array_length(mal_valeurdefaut, 1) = 14))
);


ALTER TABLE ts_masquelot_mal OWNER TO postgres;

--
-- Name: ts_masqueope_mao; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_masqueope_mao (
    mao_mas_id integer NOT NULL,
    mao_affichage boolean[],
    mao_valeurdefaut text[],
    CONSTRAINT c_ck_length_mao_affichage CHECK ((array_length(mao_affichage, 1) = 16)),
    CONSTRAINT c_ck_length_mao_valeurdefaut CHECK ((array_length(mao_valeurdefaut, 1) = 9))
);


ALTER TABLE ts_masqueope_mao OWNER TO postgres;

--
-- Name: ts_masqueordreaffichage_maa; Type: TABLE; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE TABLE ts_masqueordreaffichage_maa (
    maa_id integer NOT NULL,
    maa_mal_id integer,
    maa_table character varying(40) NOT NULL,
    maa_valeur text NOT NULL,
    maa_champdumasque character varying(30),
    maa_rang integer
);


ALTER TABLE ts_masqueordreaffichage_maa OWNER TO iav;

--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: SEQUENCE; Schema: user_2; Owner: iav
--

CREATE SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE ts_masqueordreaffichage_maa_maa_id_seq OWNER TO iav;

--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: SEQUENCE OWNED BY; Schema: user_2; Owner: iav
--

ALTER SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq OWNED BY ts_masqueordreaffichage_maa.maa_id;


SET default_with_oids = true;

--
-- Name: ts_taillevideo_tav; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_taillevideo_tav (
    tav_dic_identifiant integer NOT NULL,
    tav_coefconversion numeric NOT NULL,
    tav_distance character varying(3) NOT NULL,
    tav_org_code character varying(30) NOT NULL
);


ALTER TABLE ts_taillevideo_tav OWNER TO postgres;

--
-- Name: ts_taxonvideo_txv; Type: TABLE; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE TABLE ts_taxonvideo_txv (
    txv_code character varying(3) NOT NULL,
    txv_tax_code character varying(6),
    txv_std_code character varying(4),
    txv_org_code character varying(30) NOT NULL
);


ALTER TABLE ts_taxonvideo_txv OWNER TO postgres;

SET search_path = nat, pg_catalog;

--
-- Name: bjo_identifiant; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY t_bilanmigrationjournalier_bjo ALTER COLUMN bjo_identifiant SET DEFAULT nextval('t_bilanmigrationjournalier_bjo_bjo_identifiant_seq'::regclass);


--
-- Name: bme_identifiant; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY t_bilanmigrationmensuel_bme ALTER COLUMN bme_identifiant SET DEFAULT nextval('t_bilanmigrationmensuel_bme_bme_identifiant_seq'::regclass);


--
-- Name: lot_identifiant; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY t_lot_lot ALTER COLUMN lot_identifiant SET DEFAULT nextval('t_lot_lot_lot_identifiant_seq'::regclass);


--
-- Name: ope_identifiant; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY t_operation_ope ALTER COLUMN ope_identifiant SET DEFAULT nextval('t_operation_ope_ope_identifiant_seq'::regclass);


--
-- Name: ouv_identifiant; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY t_ouvrage_ouv ALTER COLUMN ouv_identifiant SET DEFAULT nextval('t_ouvrage_ouv_ouv_identifiant_seq'::regclass);


--
-- Name: dis_identifiant; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY tg_dispositif_dis ALTER COLUMN dis_identifiant SET DEFAULT nextval('tg_dispositif_dis_dis_identifiant_seq'::regclass);


--
-- Name: stm_identifiant; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY tj_stationmesure_stm ALTER COLUMN stm_identifiant SET DEFAULT nextval('tj_stationmesure_stm_stm_identifiant_seq'::regclass);


--
-- Name: main_identifiant; Type: DEFAULT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY ts_maintenance_main ALTER COLUMN main_identifiant SET DEFAULT nextval('ts_maintenance_main_main_identifiant_seq'::regclass);


--
-- Name: mas_id; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY ts_masque_mas ALTER COLUMN mas_id SET DEFAULT nextval('ts_masque_mas_mas_id_seq'::regclass);


--
-- Name: mac_id; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY ts_masquecaracteristiquelot_mac ALTER COLUMN mac_id SET DEFAULT nextval('ts_masquecaracteristiquelot_mac_mac_id_seq'::regclass);


--
-- Name: maa_id; Type: DEFAULT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa ALTER COLUMN maa_id SET DEFAULT nextval('ts_masqueordreaffichage_maa_maa_id_seq'::regclass);


SET search_path = ref, pg_catalog;

--
-- Name: val_identifiant; Type: DEFAULT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY tr_valeurparametrequalitatif_val ALTER COLUMN val_identifiant SET DEFAULT nextval('tr_valeurparametrequalitatif_val_val_identifiant_seq'::regclass);


--
-- Name: main_identifiant; Type: DEFAULT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY ts_maintenance_main ALTER COLUMN main_identifiant SET DEFAULT nextval('ts_maintenance_main_main_identifiant_seq'::regclass);


--
-- Name: msr_id; Type: DEFAULT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY ts_messager_msr ALTER COLUMN msr_id SET DEFAULT nextval('ts_messager_msr_msr_id_seq'::regclass);


--
-- Name: mrl_id; Type: DEFAULT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY ts_messagerlang_mrl ALTER COLUMN mrl_id SET DEFAULT nextval('ts_messagerlang_mrl_mrl_id_seq'::regclass);


--
-- Name: nom_id; Type: DEFAULT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY ts_nomenclature_nom ALTER COLUMN nom_id SET DEFAULT nextval('ts_nomenclature_nom_nom_id_seq'::regclass);


SET search_path = user_1, pg_catalog;

--
-- Name: bjo_identifiant; Type: DEFAULT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY t_bilanmigrationjournalier_bjo ALTER COLUMN bjo_identifiant SET DEFAULT nextval('t_bilanmigrationjournalier_bjo_bjo_identifiant_seq'::regclass);


--
-- Name: bme_identifiant; Type: DEFAULT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY t_bilanmigrationmensuel_bme ALTER COLUMN bme_identifiant SET DEFAULT nextval('t_bilanmigrationmensuel_bme_bme_identifiant_seq'::regclass);


--
-- Name: lot_identifiant; Type: DEFAULT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY t_lot_lot ALTER COLUMN lot_identifiant SET DEFAULT nextval('t_lot_lot_lot_identifiant_seq'::regclass);


--
-- Name: ope_identifiant; Type: DEFAULT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY t_operation_ope ALTER COLUMN ope_identifiant SET DEFAULT nextval('t_operation_ope_ope_identifiant_seq'::regclass);


--
-- Name: ouv_identifiant; Type: DEFAULT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY t_ouvrage_ouv ALTER COLUMN ouv_identifiant SET DEFAULT nextval('t_ouvrage_ouv_ouv_identifiant_seq'::regclass);


--
-- Name: dis_identifiant; Type: DEFAULT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY tg_dispositif_dis ALTER COLUMN dis_identifiant SET DEFAULT nextval('tg_dispositif_dis_dis_identifiant_seq'::regclass);


--
-- Name: stm_identifiant; Type: DEFAULT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY tj_stationmesure_stm ALTER COLUMN stm_identifiant SET DEFAULT nextval('tj_stationmesure_stm_stm_identifiant_seq'::regclass);


--
-- Name: main_identifiant; Type: DEFAULT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY ts_maintenance_main ALTER COLUMN main_identifiant SET DEFAULT nextval('ts_maintenance_main_main_identifiant_seq'::regclass);


--
-- Name: mas_id; Type: DEFAULT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY ts_masque_mas ALTER COLUMN mas_id SET DEFAULT nextval('ts_masque_mas_mas_id_seq'::regclass);


--
-- Name: mac_id; Type: DEFAULT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY ts_masquecaracteristiquelot_mac ALTER COLUMN mac_id SET DEFAULT nextval('ts_masquecaracteristiquelot_mac_mac_id_seq'::regclass);


--
-- Name: maa_id; Type: DEFAULT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa ALTER COLUMN maa_id SET DEFAULT nextval('ts_masqueordreaffichage_maa_maa_id_seq'::regclass);


SET search_path = user_2, pg_catalog;

--
-- Name: bjo_identifiant; Type: DEFAULT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY t_bilanmigrationjournalier_bjo ALTER COLUMN bjo_identifiant SET DEFAULT nextval('t_bilanmigrationjournalier_bjo_bjo_identifiant_seq'::regclass);


--
-- Name: bme_identifiant; Type: DEFAULT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY t_bilanmigrationmensuel_bme ALTER COLUMN bme_identifiant SET DEFAULT nextval('t_bilanmigrationmensuel_bme_bme_identifiant_seq'::regclass);


--
-- Name: lot_identifiant; Type: DEFAULT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY t_lot_lot ALTER COLUMN lot_identifiant SET DEFAULT nextval('t_lot_lot_lot_identifiant_seq'::regclass);


--
-- Name: ope_identifiant; Type: DEFAULT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY t_operation_ope ALTER COLUMN ope_identifiant SET DEFAULT nextval('t_operation_ope_ope_identifiant_seq'::regclass);


--
-- Name: ouv_identifiant; Type: DEFAULT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY t_ouvrage_ouv ALTER COLUMN ouv_identifiant SET DEFAULT nextval('t_ouvrage_ouv_ouv_identifiant_seq'::regclass);


--
-- Name: dis_identifiant; Type: DEFAULT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY tg_dispositif_dis ALTER COLUMN dis_identifiant SET DEFAULT nextval('tg_dispositif_dis_dis_identifiant_seq'::regclass);


--
-- Name: stm_identifiant; Type: DEFAULT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY tj_stationmesure_stm ALTER COLUMN stm_identifiant SET DEFAULT nextval('tj_stationmesure_stm_stm_identifiant_seq'::regclass);


--
-- Name: main_identifiant; Type: DEFAULT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY ts_maintenance_main ALTER COLUMN main_identifiant SET DEFAULT nextval('ts_maintenance_main_main_identifiant_seq'::regclass);


--
-- Name: mas_id; Type: DEFAULT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY ts_masque_mas ALTER COLUMN mas_id SET DEFAULT nextval('ts_masque_mas_mas_id_seq'::regclass);


--
-- Name: mac_id; Type: DEFAULT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY ts_masquecaracteristiquelot_mac ALTER COLUMN mac_id SET DEFAULT nextval('ts_masquecaracteristiquelot_mac_mac_id_seq'::regclass);


--
-- Name: maa_id; Type: DEFAULT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa ALTER COLUMN maa_id SET DEFAULT nextval('ts_masqueordreaffichage_maa_maa_id_seq'::regclass);


SET search_path = nat, pg_catalog;

--
-- Name: serial_ann; Type: SEQUENCE SET; Schema: nat; Owner: postgres
--

SELECT pg_catalog.setval('serial_ann', 1997, false);


--
-- Name: serial_sem; Type: SEQUENCE SET; Schema: nat; Owner: postgres
--

SELECT pg_catalog.setval('serial_sem', 1, false);


--
-- Data for Name: t_bilanmigrationjournalier_bjo; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY t_bilanmigrationjournalier_bjo (bjo_identifiant, bjo_dis_identifiant, bjo_tax_code, bjo_std_code, bjo_annee, bjo_jour, bjo_labelquantite, bjo_valeur, bjo_horodateexport, bjo_org_code) FROM stdin;
\.


--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('t_bilanmigrationjournalier_bjo_bjo_identifiant_seq', 1, false);


--
-- Data for Name: t_bilanmigrationmensuel_bme; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY t_bilanmigrationmensuel_bme (bme_identifiant, bme_dis_identifiant, bme_tax_code, bme_std_code, bme_annee, bme_mois, bme_labelquantite, bme_valeur, bme_horodateexport, bme_org_code) FROM stdin;
\.


--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('t_bilanmigrationmensuel_bme_bme_identifiant_seq', 1, false);


--
-- Data for Name: t_dispositifcomptage_dic; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY t_dispositifcomptage_dic (dic_dis_identifiant, dic_dif_identifiant, dic_code, dic_tdc_code, dic_org_code) FROM stdin;
\.


--
-- Data for Name: t_dispositiffranchissement_dif; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY t_dispositiffranchissement_dif (dif_dis_identifiant, dif_ouv_identifiant, dif_code, dif_localisation, dif_orientation, dif_org_code) FROM stdin;
\.


--
-- Data for Name: t_lot_lot; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY t_lot_lot (lot_identifiant, lot_ope_identifiant, lot_tax_code, lot_std_code, lot_effectif, lot_quantite, lot_qte_code, lot_methode_obtention, lot_lot_identifiant, lot_dev_code, lot_commentaires, lot_org_code) FROM stdin;
\.


--
-- Name: t_lot_lot_lot_identifiant_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('t_lot_lot_lot_identifiant_seq', 1, false);


--
-- Data for Name: t_marque_mqe; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY t_marque_mqe (mqe_reference, mqe_loc_code, mqe_nmq_code, mqe_omq_reference, mqe_commentaires, mqe_org_code) FROM stdin;
\.


--
-- Data for Name: t_operation_ope; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY t_operation_ope (ope_identifiant, ope_dic_identifiant, ope_date_debut, ope_date_fin, ope_organisme, ope_operateur, ope_commentaires, ope_org_code) FROM stdin;
\.


--
-- Name: t_operation_ope_ope_identifiant_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('t_operation_ope_ope_identifiant_seq', 1, false);


--
-- Data for Name: t_operationmarquage_omq; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY t_operationmarquage_omq (omq_reference, omq_commentaires, omq_org_code) FROM stdin;
\.


--
-- Data for Name: t_ouvrage_ouv; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY t_ouvrage_ouv (ouv_identifiant, ouv_sta_code, ouv_code, ouv_libelle, ouv_localisation, ouv_coordonnee_x, ouv_coordonnee_y, ouv_altitude, ouv_carte_localisation, ouv_commentaires, ouv_nov_code, ouv_org_code) FROM stdin;
\.


--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('t_ouvrage_ouv_ouv_identifiant_seq', 7, false);


--
-- Data for Name: t_periodefonctdispositif_per; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY t_periodefonctdispositif_per (per_dis_identifiant, per_date_debut, per_date_fin, per_commentaires, per_etat_fonctionnement, per_tar_code, per_org_code) FROM stdin;
\.


--
-- Data for Name: t_station_sta; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY t_station_sta (sta_code, sta_nom, sta_localisation, sta_coordonnee_x, sta_coordonnee_y, sta_altitude, sta_carte_localisation, sta_superficie, sta_distance_mer, sta_date_creation, sta_date_suppression, sta_commentaires, sta_dernier_import_conditions, sta_org_code, sta_geom) FROM stdin;
\.


--
-- Data for Name: tg_dispositif_dis; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY tg_dispositif_dis (dis_identifiant, dis_date_creation, dis_date_suppression, dis_commentaires, dis_org_code) FROM stdin;
\.


--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('tg_dispositif_dis_dis_identifiant_seq', 19, false);


--
-- Data for Name: tj_actionmarquage_act; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY tj_actionmarquage_act (act_lot_identifiant, act_mqe_reference, act_action, act_commentaires, act_org_code, act_nmq_code) FROM stdin;
\.


--
-- Data for Name: tj_caracteristiquelot_car; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY tj_caracteristiquelot_car (car_lot_identifiant, car_par_code, car_methode_obtention, car_val_identifiant, car_valeur_quantitatif, car_precision, car_commentaires, car_org_code) FROM stdin;
\.


--
-- Data for Name: tj_coefficientconversion_coe; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY tj_coefficientconversion_coe (coe_tax_code, coe_std_code, coe_qte_code, coe_date_debut, coe_date_fin, coe_valeur_coefficient, coe_commentaires, coe_org_code) FROM stdin;
\.


--
-- Data for Name: tj_conditionenvironnementale_env; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY tj_conditionenvironnementale_env (env_date_debut, env_date_fin, env_methode_obtention, env_val_identifiant, env_valeur_quantitatif, env_stm_identifiant, env_org_code) FROM stdin;
\.


--
-- Data for Name: tj_dfestdestinea_dtx; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY tj_dfestdestinea_dtx (dtx_dif_identifiant, dtx_tax_code, dtx_org_code) FROM stdin;
\.


--
-- Data for Name: tj_dfesttype_dft; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY tj_dfesttype_dft (dft_df_identifiant, dft_rang, dft_org_code, dft_tdf_code) FROM stdin;
\.


--
-- Data for Name: tj_pathologieconstatee_pco; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY tj_pathologieconstatee_pco (pco_lot_identifiant, pco_pat_code, pco_loc_code, pco_commentaires, pco_org_code, pco_imp_code) FROM stdin;
\.


--
-- Data for Name: tj_prelevementlot_prl; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY tj_prelevementlot_prl (prl_pre_typeprelevement, prl_lot_identifiant, prl_code, prl_operateur, prl_loc_code, prl_commentaires, prl_org_code) FROM stdin;
\.


--
-- Data for Name: tj_stationmesure_stm; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY tj_stationmesure_stm (stm_identifiant, stm_libelle, stm_sta_code, stm_par_code, stm_description, stm_org_code) FROM stdin;
\.


--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('tj_stationmesure_stm_stm_identifiant_seq', 1, false);


--
-- Data for Name: tj_tauxechappement_txe; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY tj_tauxechappement_txe (txe_tax_code, txe_std_code, txe_date_debut, txe_date_fin, txe_methode_estimation, txe_ech_code, txe_valeur_taux, txe_commentaires, txe_org_code, txe_sta_code) FROM stdin;
\.


--
-- Data for Name: ts_maintenance_main; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY ts_maintenance_main (main_identifiant, main_ticket, main_description) FROM stdin;
\.


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE SET; Schema: nat; Owner: postgres
--

SELECT pg_catalog.setval('ts_maintenance_main_main_identifiant_seq', 1, false);


--
-- Data for Name: ts_masque_mas; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY ts_masque_mas (mas_id, mas_code, mas_description, mas_raccourci, mas_type) FROM stdin;
\.


--
-- Name: ts_masque_mas_mas_id_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('ts_masque_mas_mas_id_seq', 1, false);


--
-- Data for Name: ts_masquecaracteristiquelot_mac; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY ts_masquecaracteristiquelot_mac (mac_id, mac_mal_id, mac_par_code, mac_affichagevaleur, mac_affichageprecision, mac_affichagemethodeobtention, mac_affichagecommentaire, mac_valeurquantitatifdefaut, mac_valeurqualitatifdefaut, mac_precisiondefaut, mac_methodeobtentiondefaut, mac_commentairedefaut) FROM stdin;
\.


--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('ts_masquecaracteristiquelot_mac_mac_id_seq', 1, false);


--
-- Data for Name: ts_masqueconditionsenvironnementales_mae; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY ts_masqueconditionsenvironnementales_mae (mae_mao_id, mae_stm_identifiant, mae_affichage, mae_valeurdefaut) FROM stdin;
\.


--
-- Data for Name: ts_masquelot_mal; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY ts_masquelot_mal (mal_mas_id, mal_affichage, mal_valeurdefaut) FROM stdin;
\.


--
-- Data for Name: ts_masqueope_mao; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY ts_masqueope_mao (mao_mas_id, mao_affichage, mao_valeurdefaut) FROM stdin;
\.


--
-- Data for Name: ts_masqueordreaffichage_maa; Type: TABLE DATA; Schema: nat; Owner: nat
--

COPY ts_masqueordreaffichage_maa (maa_id, maa_mal_id, maa_table, maa_valeur, maa_champdumasque, maa_rang) FROM stdin;
\.


--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: SEQUENCE SET; Schema: nat; Owner: nat
--

SELECT pg_catalog.setval('ts_masqueordreaffichage_maa_maa_id_seq', 1, false);


--
-- Data for Name: ts_taillevideo_tav; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY ts_taillevideo_tav (tav_dic_identifiant, tav_coefconversion, tav_distance, tav_org_code) FROM stdin;
\.


--
-- Data for Name: ts_taxonvideo_txv; Type: TABLE DATA; Schema: nat; Owner: postgres
--

COPY ts_taxonvideo_txv (txv_code, txv_tax_code, txv_std_code, txv_org_code) FROM stdin;
\.


SET search_path = public, pg_catalog;

--
-- Data for Name: chevauchement; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY chevauchement (ope1, ope2, ope_dic_identifiant) FROM stdin;
\.


--
-- Data for Name: pointcloud_formats; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY pointcloud_formats  FROM stdin;
\.


--
-- Data for Name: spatial_ref_sys; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY spatial_ref_sys  FROM stdin;
\.


SET search_path = ref, pg_catalog;

--
-- Data for Name: tg_parametre_par; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tg_parametre_par (par_code, par_nom, par_unite, par_nature, par_definition) FROM stdin;
A123	Durée du premier séjour en eau douce	Dénombrement	BIOLOGIQUE	Chez les salmonidés migrateurs, nombre d’hivers successifs passés en eau douce avant leur premier départ en mer.
A122	Hivers	Dénombrement	BIOLOGIQUE	Variable définissant chronologiquement la succession des hivers vécus par le poisson. R  : Hiver en eau douce sans fraie;  M  : Hiver en mer sans fraie;  F  : Hiver en eau douce avec fraie;  .  : Définit une phase en eau douce ou en mer ininterprétable ;  Le nombre de caractères correspond à l’age total du poisson, sauf lorsque la variable contient un « . ».
A124	Durée du premier séjour en mer	Dénombrement	BIOLOGIQUE	Chez les salmonidés migrateurs, nombre d’hivers successifs passés en mer avant leur premier retour en eau douce. Si une reprise de croissance printanière ou estivale est présente en bordure d’écaille, ce nombre est suivi du caractère « + ».
A127	Age pit tag	Dénombrement	BIOLOGIQUE	Âge donné par la lecture d'un pit tag permettant de classer le saumon dans la classification des âges de l'INRA (définition SANDRE à fournir par l'INRA)
A128	Age (écaille)	Dénombrement	BIOLOGIQUE	Âge donné par la lecture des écailles (définition SANDRE à fournir par l'INRA)
A111	Poids du lot	g	BIOLOGIQUE	Poids en grammes d’un poisson individuel, ou d’un groupe de poissons pour les lots.
1785	Longueur à la fourche	mm	BIOLOGIQUE	Longueur mesurée, poisson à plat, allant de l'extrémité du museau du poisson au fond de l'échancrure de sa nageoire caudale. Certaines espèces (chabot, anguille, ...) n'ont pas d'échancrure caudale. La notion de longueur à la fourche n'a pas de sens et ne s'applique donc pas.
1786	Longueur totale	mm	BIOLOGIQUE	Longueur mesurée, poisson à plat, nageoire caudale en position normale, allant de l'extrémité du museau à l'extrémité de la queue.
1787	Longueur de la mâchoire supérieure (maxillaire)	mm	BIOLOGIQUE	Longueur mesurée entre l'extrémité du museau et l'extrémité postérieure externe du maxillaire. Le rapport de la longueur de la mâchoire à la longueur totale permet de déterminer le sexe des salmonidés en cours de mâturation sexuelle (méthode utilisée notamment chez le saumon).
1788	Indice oculaire	mm	BIOLOGIQUE	Rapport entre la surface de l'oeil (approximé par un cercle) et la longueur de l'anguille, obtenu par l'équation suivante : I = ((((A + B) / 4) ² * Pi) / L) * 100, avec : I : index oculaire A : diamètre horizontal de l'oeil (en mm) B : diamètre vertical de l'oeil (en mm) L : longueur totale de l'animal (en mm). Cet indice est bien corrélé au degré de maturation des gonades et à la prise d'argenture mais les limites sont encores à préciser.
1783	Sexe de l'individu	Sans objet	BIOLOGIQUE	Description du sexe d'un animal. Le sexe indéterminé indique que la recherche du sexe n'a pas été effectuée. Le sexe indifférencié indique que la recherche du sexe a été effectuée mais qu'aucun sexe n'a pu être identifié. 
1784	Maturité sexuelle	Sans objet	BIOLOGIQUE	Stade du développement de la capacité du poisson à se reproduire.
1790	Chronologie des hivers et des étés (salmonidés)	Sans objet	BIOLOGIQUE	Succession chronologique codée, pour chaque saison estivale et hivernale vécue par un poisson (généralement un salmonidé migrateur), des événements dominants connus survenus dans son cycle biologique. Par exemple : RrRrMrRmMrR signifie que le poisson a passé ses deux premières années en rivière puis les hivers suivants en alternance entre la mer et la rivière."
1791	Pigmentation des civelles	Sans objet	BIOLOGIQUE	Classification de la fin de la métamorphose du leptocephale en civelle basé sur l'évolution de la pigmentation superficielle. 
B001	Classe de taille (10 mm)	Sans objet	BIOLOGIQUE	Classes pour l'approximation des tailles en vidéo comptage
B002	Classe de taille (anguilles Arzal)	Sans objet	BIOLOGIQUE	Classes basées sur des valeurs de sélectivité de filets servant au tri
1409	Température de l'air	°C	ENVIRONNEMENTAL	Température de l'air à un moment donné
A014	Température de l’eau	°C	ENVIRONNEMENTAL	Température mesurée au niveau de la station à un instant donné
1798	Conductivité	µS.cm-1	ENVIRONNEMENTAL	Conductivité électrique de l'eau non ramenée à une température conventionnelle
1331	Salinité en g.l-1	g.l-1 NaCl	ENVIRONNEMENTAL	Code gelé, l'unité de mesure 'g.l-1 NaCl' est obsolète. Il est remplacé par le paramètre 1842 Salinité auquel aucune unité n'est associée.
1408	Pression atmosphérique	hPa	ENVIRONNEMENTAL	 Information: (1 bar = 10^5 Pascal = 10.33m CE)  1mm de Hg = Ro x g x h = 13595 x 9.80 x 0.001 = 133 Pa = 1.33 hPa
1429	Cote à l'échelle	m	ENVIRONNEMENTAL	Cote à l'échelle relevée à un instant donné.
1689	Profondeur du niveau piézométrique	m	ENVIRONNEMENTAL	Distance mesurée entre le niveau d'eau présent dans un piézomètre et la surface.
1419	Vitesse moyenne d'écoulement	m.s-1	ENVIRONNEMENTAL	Vitesse moyenne d'écoulement
1552	Volume moyen journalier	m3.j-1	ENVIRONNEMENTAL	Volume d'eau écoulé pendant une unité de temps au droit d'une section de cours d'eau à un instant donné.
1420	Débit instantané	m3.s-1	ENVIRONNEMENTAL	Volume d'eau écoulé pendant une unité de temps au droit d'une section de cours d'eau.
1421	Débit moyen journalier (QMJ)	m3.s-1	ENVIRONNEMENTAL	Volume d'eau écoulé pendant 24 heures calendaires données au droit d'une section de cours d'eau donnée.
A013	Dureté de l’eau	mg.l-1	ENVIRONNEMENTAL	Somme des teneurs en calcium et magnésium de l’eau exprimé en mg.l-1 de Ca++.
1302	Potentiel en Hydrogène (pH)	Ph	ENVIRONNEMENTAL	pH de l'eau (activité des ions H+ selon la loi de Nernst).
1425	Conditions météorologiques	Sans objet	ENVIRONNEMENTAL	Constat de la combinaison locale et passagère de la température, de l'état du ciel, des précipitations et du vent pendant la réalisation du prélèvement.
A005	Turbidité	Sans objet	ENVIRONNEMENTAL	Classes d'opacité de l'eau définies par l'appréciation de la difficulté de réaliser l'opération de contrôle des migrations
A126	Age (livrée -taille)	Dénombrement	BIOLOGIQUE	Classification des âges chez le saumon par l'INRA (définition SANDRE à fournir) et utilisation pour spécifier une cohorte (logrami)
1842	Salinité	Sans objet	ENVIRONNEMENTAL	La grandeur ''Salinité'' représente la proportion de sels minéraux dissous dans l'eau de mer. L'emploi du paramètre "Salinité" concerne exclusivement les mesures réalisées dans les eaux marines ou saumâtres (pour une salinité de 2 à 42). Définie comme un rapport de conductivité par l'echelle pratique de salinité (EPS 78), elle n'a ni dimension ni unité.
BBBB	Diamètre vertical de l`oeil	mm	BIOLOGIQUE	Diamètre vertical de l`oeil
CCCC	Diamètre horizontal de l`oeil	mm	BIOLOGIQUE	Diamètre horizontal de l`oeil
CONT	Contraste dos-ventre	Logique	BIOLOGIQUE	Contraste entre le dos et le ventre d’une anguille, utilisé pour déterminer l’argenture
PECT	Longueur pectorale	mm	BIOLOGIQUE	Longueur de la nageoire pectorale gauche
LINP	Présence ponctuation	Logique	BIOLOGIQUE	Présence de ponctuation sur la ligne latérale d’une anguille, utilisé pour déterminer l’argenture
AAAA	Phases lunaires	Sans objet	ENVIRONNEMENTAL	Phases de la Lune
C001	Longueur vidéo	mm	BIOLOGIQUE	Calcul de la longueur à partir de la taille observée sur la vitre de vidéo comptage
1553	hauteur de précipitations	mm	ENVIRONNEMENTAL	Hauteur des précipitations
1295	Turbidité Formazine Néphélométrique	NTU	ENVIRONNEMENTAL	Réduction de la transparence d’un liquide due à la présence de matières non dissoutes, \nmesurée à un angle de 90° par rapport à la lumière incidente (néphélomètre),\nLes unités J.T.U. (Jackson Turbidity Units) et N.T.U. (Nephelometric Turbidity Units) sont équivalentes.\nLa norme française demande l’expression des mesures sous un angle de 90° (la turbidité Néphélométrique) et sous un angle de 0° (Turbidité Formazine). \nDans les deux cas l’unité utilisée est l’unité formazine.
B003	Classe de taille (anguilles Marais Poitevin)	Sans objet	BIOLOGIQUE	Classe pour la séparation des petites et grandes anguilles lors du piégeage
1789	Age de l'individu	année	BIOLOGIQUE	Nombre d'années au semestre près qui sépare la naissance d'un animal de la date à laquelle il est examiné, lors d'une capture notamment. Exemple : Un gardon né en juin 1994, est âgé de : - 3 ans en juin 1997 - 3,5 ans en janvier 1998"
COHO	Cohorte de reproduction	Sans objet	BIOLOGIQUE	Indication de la cohorte de reproduction dont dépend le poisson (expertise dépendant de la distance à l'estuaire de la station, de l'état du poisson et de sa date d'arrivée)
LNAR	Longueur narine	mm	BIOLOGIQUE	Longueur de la narine (code MIGRADOUR)
\.


--
-- Data for Name: tr_devenirlot_dev; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_devenirlot_dev (dev_code, dev_libelle, dev_rang) FROM stdin;
1	Relâché au droit de la station	1
0	Devenir inconnu	2
2	Trépassé	3
3	Transporté dans le milieu naturel 	4
4	Mis en élevage	6
6	Relâché avant l'entrée de la station, susceptible d'être recapturé dans la même station	5
5	Prélevé pour étude	7
\.


--
-- Data for Name: tr_importancepatho_imp; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_importancepatho_imp (imp_code, imp_libelle, imp_definition) FROM stdin;
0	Nul (0)	Nombre (N) 0, Surface atteinte (S) 0%, Degré d'altération (D) nul.
1	Faible (1)	Nombre (N) <3, Surface atteinte (S) <5%, Degré d'altération (D) faible.
2	Moyen (2)	Nombre (N) 4 à 6, Surface atteinte (S) 5 à 10 %, Degré d'altération (D) moyen.
3	Fort (3)	Nombre (N) 7 à 10, Surface atteinte (S) 10 à 20 %, Degré d'altération (D) important.
4	Très fort (4)	Nombre (N) >10, Surface atteinte (S) >20%, Degré d'altération (D) très important.
\.


--
-- Data for Name: tr_localisationanatomique_loc; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_localisationanatomique_loc (loc_code, loc_libelle) FROM stdin;
C	Tout le corps
H	Dos
A	Abdomen
T	Tête
G	Bouche
M	Mâchoire
L	Lèvre
Y	Oeil
O	Opercule
B	Branchie
Q	Nageoire caudale
D	Nageoire dorsale
U	Orifice urogénital
E	Ecaille
PVD	Pelvienne droite
PVG	Pelvienne gauche
PCD	Pectorale droite
PCG	Pectorale gauche
ANA	Anale
CAH	Caudale haute
CAB	Caudale basse
D1	Première dorsale
D2	Deuxième dorsale
ADP	Adipeuse
OED	Oeil droit
OEG	Oeil gauche
NAS	Région nasale
GAV	Flanc gauche en avant de la dorsale
GAR	Flanc gauche en arrière de la dorsale
DAV	Flanc droit en avant de la dorsale
DAR	Flanc droit en arrière de la dorsale
MDG	Musculature à gauche de la dorsale
MDD	Musculature à droite de la dorsale
ABD	Cavité abdominale
TDI	Tube digestif
OPD	Opercule droit
OPG	Opercule gauche
MSG	Mâchoire supérieure gauche
MSD	Mâchoire supérieure droite
MID	Mâchoire inférieure droite
MIG	Mâchoire inférieure gauche
EC	Ensemble du corps
FL	Flancs
IND	Indéterminé
W	Tronc
K	Pédoncule caudal
X	Orifice anal
N	Nageoire principale
P	Nageoire pectorale
V	Colonne vertébrale
PV	Pelvienne
\.


--
-- Data for Name: tr_naturemarque_nmq; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_naturemarque_nmq (nmq_code, nmq_libelle) FROM stdin;
ABLA	Ablation de nageoire en général
ABLD	Ablation de nageoire par découpage
ABLB	Ablation de nageoire par brûlure
CRYO	Cryomarquage
CARL	Etiquette CARLIN
FTAG	Spaghetti FLOY TAG
COLO	Coloration
META	Agrafes métalliques
MMNA	Micromarque magnétique binaire
PITS	Marque transpondeur PIT
IMPV	Implant visible
TELE	Emetteur télémétrique radio ou sonique
\.


--
-- Data for Name: tr_natureouvrage_nov; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_natureouvrage_nov (nov_code, nov_nom) FROM stdin;
0	Nature de l'ouvrage inconnue
1.1	Barrage
1.2	Seuil en rivière
X.2	Filet barrage
X.1	barrière électrique
X.0	pas d’ouvrage
\.


--
-- Data for Name: tr_niveauechappement_ech; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_niveauechappement_ech (ech_code, ech_libelle, ech_equivalence_tx) FROM stdin;
1	Non renseigné	\N
2	Inconnu	\N
3	nul	0%
4	Faible	<33%
5	Moyen	34 à 66 %
6	Fort	>66 %
\.


--
-- Data for Name: tr_niveautaxonomique_ntx; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_niveautaxonomique_ntx (ntx_code, ntx_libelle, ntx_mnemonique) FROM stdin;
13	Genre	G
15	Espèce	Sp
0	Inconnu	0
\.


--
-- Data for Name: tr_parametrequalitatif_qal; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_parametrequalitatif_qal (qal_par_code, qal_valeurs_possibles) FROM stdin;
1784	Non recherché / A frayé / Boutons de noces / A commencé de frayer / …
1791	VI/VA/VB/VIA0/…
1425	Temps sec ensoleillé…
A005	0 : Non renseigné 1 : Nulle (fond visible) 2 : Faible (fond perceptible) 3 : Appréciable (fond non visible)
B001	[0-10[;[10-20[…
B002	<98;[98-145[,[145,300[,>300
1783	Indéterminé / mâle / femelle / Indifférencié
AAAA	nouvelle lune/premier quartier/pleine lune,dernier quartier
CONT	0/1
LINP	0/1
B003	<150;>150
COHO	cohorte année reproduction n / cohorte année reproduction n+1 / cohorte année reproduction n-1
A126	Cohorte année reproduction n/ cohorte année reproduction n+1 / cohorte année reproduction n-1
\.


--
-- Data for Name: tr_parametrequantitatif_qan; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_parametrequantitatif_qan (qan_par_code) FROM stdin;
1789
A123
A122
A124
A111
1785
1786
1787
1788
1409
A014
1798
1331
1408
1429
1689
1419
1552
1420
1421
A013
1302
1842
C001
BBBB
CCCC
PECT
1790
A126
1553
A128
A127
1295
\.


--
-- Data for Name: tr_pathologie_pat; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_pathologie_pat (pat_code, pat_libelle, pat_definition) FROM stdin;
AA	Altération de l'aspect	Le corps du poisson examiné présente des altérations morphologiques caractérisées, pouvant éventuellement être détaillées ou non.
AC	Altération de la couleur	La pigmentation présente des altérations entrainant une coloration anormale de tout ou partie du corps du poisson.
AD	Difforme	Le corps du poisson présente des déformations anormales se traduisant par des acures ou des bosselures,extériorisation possible d'une atteinte interne, virale par exemple (ex : nécrose pancréatique infectieuse de la truite arc-en-ciel)
AH	Aspect hérissé (écailles)	Les écailles du poisson ont tendance à se relever perpendiculairement au corps, à la suite généralement d'une infection au niveau des téguments
AM	Maigreur	Le corps du poisson présente une minceur marquée par rapport à la normalité
AO	Absence d'organes	L'altération morphologique observée sur le poisson se traduit par l'absence d'un organe (nageoire, opercule, oeil, machoire)
BG	Bulle de gaz	Présence de bulle de gaz pouvant être observées sous la peau, au bord des nageoires, au niveau des yeux, des branchies ou de la bouche.
CA	Coloration anormale	L'altération de la pigmentation entraîne la différenciation de zones diversement colorées, avec en particulier des zones sombres.
CC	Copépodes (Ergasilus, Lerna,...)	Présence visible, à la surface du corps ou des branchies du poisson, de crustacés parasites, à un stade donné de leur cycle de développement.
CO	Coloration opaque (oeil)	L'altération de la coloration se traduit par une opacification de l'un ou des deux yeux.
CS	Coloration sombre	L'altération de la coloration du corps du poisson se traduit par un assombrissement de tout ou partie de celui-ci (noircissement).
CT	Coloration terne (pâle)	L'altération de la coloration du corps du poisson se traduit par une absence de reflets lui conférant un aspect terne, pâle, voire une décoloration.
HA	Acanthocéphales	Présence visible, à la surface du corps ou des branchies du poisson, d' acanthocéphales à un stade donné de leur cycle de développement.
HC	Cestodes (Ligula,  Bothriocephalus, ...)	Présence visible, à la surface du corps ou des branchies du poisson, de cestodes à un stade donné de leur cycle de développement.
HE	Hémorragie	Ecoulement de sang pouvant être observé à la surface du corps ou au niveau des branchies.
HH	Hirudinés (Piscicola)	Présence visible sur le poisson de sangsue(s)
HN	Nématodes (Philometra, Philimena...)	Présence visible, à la surface du corps ou des branchies du poisson, de  nématodes à un stade donné de leur cycle de développement.
HS	Stade pre-mortem	Le poisson présente un état pathologique tel qu'il n'est plus capable de se mouvoir normalement dans son milieu et qu'il est voué à une mort certaine à brève échéance.
HT	Trématodes (Bucephalus, ...)	Présence visible, à la surface du corps ou des branchies du poisson, de trématodes parasites à un stade donné de leur cycle de développement.
IS	Individu sain	Après examen du poisson, aucun signe externe, caractéristique d'une pathologie quelconque, n'est décelable à l'oeil nu
LD	Lésions diverses	Les téguments présentent une altération quelconque de leur intégrité.
NN	Non renseigné	L'aspect pathologique du poisson n'a fait l'objet d'aucun examen et aucune information n'est fournie à ce sujet
PA	Parasitisme	Présence visible, à la surface du corps ou des branchies du poisson, d'organismes parasites vivant à ses dépens.
PB	Points blancs	Présence de points blancs consécutive à la prolifération de certains protozoaires parasites comme Ichtyopthtirius (ne pas confondre avec les boutons de noces, formations kératinisées apparaissant  lors de la période de reproduction)
PL	Plaie - blessure	Présence d'une ou plusieurs lésions à la surface du tégument généralement due à un prédateur (poisson, oiseau,.)
PN	Points noirs	Présence de tâches noires bien individualisées sur la surface du tégument du poisson
VL	Vésicule contenant un liquide	Présence d'un oedème constituant une excroissance.
ZO	Etat pathologie multiforme	Le poisson présente plus de deux caractéristiques pathologiques différentes
11	- < 10 poux ; sans flagelles	Le poisson présente moins de 10 poux de mer, mais ces derniers, en raison d'un présence prolongée en eau douce, ont déjà perdu leur flagelle
21	- > 10 poux ; sans flagelles	Le poisson présente moins de 10 poux de mer, mais ces derniers, compte-tenu de l'arrivée récente de leur hôte en eau douce, n'ont pas encore perdu leur flagelle
31	- < 10 poux ; avec flagelles	Le poisson présente plus de 10 poux de mer, mais ces derniers, en raison d'un présence prolongée en eau douce, ont déjà perdu leur flagelle
42	- > 10 poux, avec flagelles	Le poisson présente plus de 10 poux de mer, mais ces derniers, compte-tenu de l'arrivée récente de leur hôte en eau douce, n'ont pas encore perdu leur flagelle
01	Trace de poux	Le poisson ne porte aucun pou mais présente des lésions cutanées consécutives à une colonisation par le pou de mer. La présence du poisson en eau douce a été suffisante pour obliger les poux à quitter leur hôte.
US	Anus Rouge ou Saillant	inflammation de l'anus avec, éventuellement, prolapsus anal. Zone hémorragique ou inflammatoire ou péri anale, comprendant une extériorisation partielle ou totale de l'anus
EX	Exophtalmie	Saillie ou protrusion anormale du globe oculaire de son orbite. Les exophtalmies peuvent être uni ou bilatérales et plus ou moins prononcées
ER	Erosion	Lésion de la peau ou des muqueuses caractérisées par la destruction généralement lente et progressive des tissus superficiels suite à une lésion pathologique ou traumatique avec un risque éventuel de surinfection. L'érosion peut être profonde s'il y a destruction dela couche basale des épithéliums de revêtement. La couche superficelle du tégument est endommagée ou manquante, laissant apparaître le tissus sous cutané
NE	Nécrose	Mort,  gangrène,  mortification de cellules ou d’un tissu organique se produisant  du vivant  de l’animal  par\nsuppression de l’irrigation sanguine. La nécrose, à la différence de l’ulcère, est une lésion irréversible. \nPeau : les premiers stades commencent par des lésions pâles, blanc, grisâtres, qui tendent à devenir noires par\nla suite.  Puis la peau se racornit  et  se dessèche,  restant  séparée des zones  irriguées par  un sillon qui  la\ndélimite de façon précise. Le stade final évolue vers une perte de substance, c’est à dire une ulcération, de la\nzone atteinte. Nageoire : elle apparaît déchirée, en lambeaux et ne subsiste, finalement, que sous la forme d’un moignon de\ncouleur blanchâtre.
AG	Grosseur, excroissance	Toutes « bosses » anormales constatées sur le poisson. \nUne masse peut être une excroissance, un néoplasme (ou tumeur), un abcès (rarissime chez les poissons),\nun  kyste,  un  papillome,  un  granulome,  un  nodule,   une  plaque  ou  le  site  d’une  inflammation  sévère\n(granulome).  A ces «masses»,  sont  parfois associées des hémorragies superficielles qui  témoignent  de la\nréponse de l’hôte.
CR	Crustacé	Les crustacés appartiennent au phylum des arthropodes. Ils se caractérisent par une respiration branchiale. \nIls affectent  la plupart  des espèces piscicoles,  dont  l’anguille,  et  sont  notamment  responsables d’affections\ncirculatoires (anémies) et cutanéobranchiales délabrantes. Ils sont également impliqués dans la transmission\nd’autres agents pathogènes, telle la virémie printanière de la carpe (VPC). Certains d’entre eux (Argulus) sont\ncapables de sécréter des substances irritantes stressant\ntrès fortement l’animal.
PC	Mycose (mousse)	Présence d'un développement à la surface du corps, d'un mycélium formant une sorte de plaque rappelant l'aspect de la mousse et appartenant à une espèce de champignon colonisant les tissus du poisson.
SM	Hypersécrétion de mucus	Présence anormale de mucus sur le corps ou au niveau de la chambre branchiale.
00	Ni poux, ni traces de poux	Le poisson, généralement un salmonidé migrateur venu de la mer, n'héberge aucun pou de mer et ne présente aucune lésion visible consécutive à une colonisation par le pou de mer (qui est en fait un crustacé parasite des salmonidés migrateurs)
CB	Branchiures (Argulus...)\n	Présence visible, à la surface du corps ou des branchies du poisson, de crustacés branchiures à un stade donné de leur cycle de développement.\n
51	Présence de Poux de mer	des poux de mer ont été observés sur l individus sans avoir été dénombrés\n
PX	Autres parasites (que CR, HH, PB, PC)	
OO	Absence de lésions ou de parasites	
NC	Signe pathologique d`origine inconnue	
UH	Ulcère (dont hémorragique)	Lésion  cutanée  inflammatoire,  aiguë  ou  chronique,  caractérisée  par  la  perte  localisée  et  complète  de\nl’épiderme (peau) ou de l’épithélium, exposant ainsi les muscles sous-jacents. La guérison laisse une cicatrice\navec ou sans perte de matière. \nUn ulcère présente une zone centrale généralement hémorragique avec mise à nu du derme et ou du tissu\nmusculaire sous-jacent.  La lésion est  délimitée et  entourée d’un anneau d’épiderme nécrotique blanc ou\njaunâtre.  Associée  ou  non  à  une   congestion  diffuse  (zone  inflammatoire  et  hémorragique)  des  tissus\nenvironnants, elle est souvent accompagnée d’une dépigmentation de la peau.
PT	Parasites (PB ou PC ou CR ou HH ou PX)	
\.


--
-- Data for Name: tr_prelevement_pre; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_prelevement_pre (pre_typeprelevement, pre_definition) FROM stdin;
Ecaille	Identifiant du prélèvement d'écaille chez un saumon en vue d'une analyse de l'âge et de l'histoire de vie (définition SANDRE à fournir).
Isotopique CN	Identifiant d'un prélèvement pour analyse isotopique (définition SANDRE à fournir).
Génétique	Un prélèvement génétique consiste en un prélèvement individuel de tissu (nageoire ou écaille pour les poissons) dans le but d’extraire l’ADN de l’échantillon collecté. L’échantillon est généralement conservé dans l’alcool avant l’analyse pour les tissus pouvant subir une décomposition ou dans un endroit sec pour les les écailles.Une fois l’ADN extrait de l’échantillon, des analyses basées sur des marqueurs moléculaire (microsatellites généralement) permettent de déterminer le génotype d’un individu, et ensuite de faire des analyses de génétique des populations (diversité génétique et différentiation des populations) ou de comportement (assignation de paternité...)\n
\.


--
-- Data for Name: tr_stadedeveloppement_std; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_stadedeveloppement_std (std_code, std_libelle, std_definition, std_statut) FROM stdin;
BEC	Ravalé (bécard)	\N	\N
PRS	Présmolt	Juvénile en cours de transformation physiologique pour passer de l'eau douce à l'eau de mer (il devient argenté). Ce stade peut être divisé en fonction de la maturation du smolt en 1/4, 1/2, 3/4 de smolt	\N
SML	Smolt	Juvénile ayant terminé sa transformation physiologique pour passer de l'eau douce à l'eau de mer (argenté).	\N
IND	Indéterminé	Stade indéterminé (note: ce stade peut correspondre à inconnu (0) ou indeterminé (1) dans la nomenclature du SANDRE)	\N
3	Larvaire	Stade de sa vie distinct du stade juvénile qui suit l'éclosion jusqu'à la métamorphose. La phase larvaire intervient avant la première prise de nourriture et la nage libre chez les poissons,  mais correspond à une phase plus longue (plusieurs années) chez les lamproies.	\N
4	Juvénile	Phase entre la phase larvaire et la phase adulte, \n correspondant à la principale phase de croissance.	\N
5	Adulte	Stade correspondant à la maturité sexuelle.	\N
11	Géniteur	Stade mature (adulte) ou en cours de maturation qui effectue la migration vers les zones de reproduction.	\N
7	Alevins vésiculés	Alevins qui après l'éclosion, n'ont pas encore résorbé leur vésicule vittéline. Chez les salmonidés, ce stade correspond à l'émergence du gravier avant la phase de nage/alimentation libre.	\N
12	Alevins à vésicule résorbée	Alevins ayant résorbé leur vésicule vittéline mais n'ayant pas repris leur alimentation	\N
13	Alevins nourrit	Alevins ayant repris leur alimentation.	\N
AGP	Argentée pré-migrante	Anguille ayant débuté le processus de métamorphose, rencontré en rivière généralement entre la fin du printemps et l'automne, avant sa migration de dévalaison.	\N
LEP	Leptocéphale	Phase larvaire marine de l'anguille qui se métamorphose en civelle à partir du talus continental.	\N
AGG	Anguille argentée	Phase migrante suivant la phase anguille jaune. Les anguilles argentées sont caractérisées par un dos assombri, un ventre argenté avec une ligne latérale clairement marquée, et une augmentation du diamètre des yeux. Les anguilles argentées effectuent la migration vers la mer (catadrome) et la migration marine vers l'ouest.	\N
AGJ	Anguille jaune	Anguille entièrement pigmentée, qui réalise la principale partie du processus de colonisation continentale et constitue la phase de croissance jusqu'au stade argenté. Cette phase est souvent nommée sédentaire mais peut effectuer des migrations dans la rivière, entre rivières, et effectuer des déplacements entre les estuaires et les eaux douces dans les deux sens.	\N
CIV	civelle	Jeune anguille non pigmentée ou en cours de pigmentation, qui marque la fin de la métamorphose depuis le stade marin leptocéphale et la migration des eaux continentales depuis le talus continental. C'est au cours du stade civelle que se fait la reprise de l'alimentation.	\N
AMM	Ammocète	Stade larvaire de la Lamproie vivant enfouie dans les sédiments ou les fond vaseux des rivières jusqu'à la métamorphose.	\N
PAR	Parr (tacon)	Juvénile de saumon durant sa phase en eau douce et avant sa transformation en smolt.	\N
BER	Bécard reconditionné (smolt)	Adulte de saumon ou truite après la reproduction,  avant leur nouveau départ vers la mer et ayant smoltifié	\N
PSP	Petit saumon de printemps	\N	gelé
PSE	Petit saumon d'été	\N	gelé
POS	Post smolt	\N	gelé
TUH	Truite à 1 hiver	\N	gelé
TDH	Truite à deux hivers	\N	gelé
PANG	Petite anguille de moins de 150mm	\N	gelé
GANG	Grande anguille de plus de 150mm	\N	gelé
ALS	Aloson	Juvénile d'alose qui effectue au cours de sa première année la migration depuis les zones de fraie en rivière vers l'estuaire puis les zones côtières.	gelé
SAB	Sabre	Alose ayant effectué sa reproduction, ces individus amaigris sont parfois rencontrés en dévalaison dans les fleuves.	gelé
MAD	Castillon	Madeleineau ou Castillon, géniteur saumon de retour après un hiver de mer.	gelé
GST	Grand saumon	Grand géniteur de saumon de retour après plusieurs hiver de mer	gelé
FIN	Finnock	Truite immature souvent dans leur primière année après la migration au stade smolt, trouvés dans les estuaires ou les parties aval des rivières. 	gelé
TRFV	Truite Fario Brillante	Truite fario légèrement brillante mais considéré comme non amphihaline. 	gelé
\.


--
-- Data for Name: tr_taxon_tax; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_taxon_tax (tax_code, tax_nom_latin, tax_nom_commun, tax_ntx_code, tax_tax_code, tax_rang) FROM stdin;
2085	Abramis	Brème	13	\N	10
2099	Abramis bjoerkna	Brème bordelière	15	\N	11
2086	Abramis brama	Brême	15	\N	12
2031	Acipenser	Esturgeon	13	\N	13
3218	Acipenser baerii baerii	Baeri	15	\N	14
2032	Acipenser sturio	Esturgeon commun	15	\N	15
2088	Alburnoides bipunctatus	Spirlin	15	\N	16
2090	Alburnus alburnus	Ablette	15	\N	17
2055	Alosa	\N	13	\N	18
2056	Alosa alosa	Grande alose	15	\N	19
2057	Alosa fallax fallax	Alose feinte	15	\N	20
2058	Alosa fallax rhodanensis	Alose feinte du Rhône	15	\N	21
2177	Ameiurus melas	Poisson chat	15	\N	22
2094	Aspius aspius	Aspe	15	\N	25
2095	Barbus	\N	13	\N	26
2096	Barbus barbus	Barbeau fluviatile	15	\N	27
2097	Barbus meridionalis	Barbeau méridional	15	\N	28
2098	Blicca	Brème	13	\N	29
2100	Carassius	\N	13	\N	30
2101	Carassius auratus	Carassin doré	15	\N	31
2102	Carassius carassius	Carassin	15	\N	32
2179	Chelon	\N	13	\N	33
2180	Chelon labrosus	Mulet à grosses levres	15	\N	34
2104	Chondrostoma nasus	Hotu	15	\N	35
2105	Chondrostoma toxostoma	Toxostome	15	\N	36
2080	Cottus gobio	Chabot	15	\N	37
2107	Ctenopharyngodon idellus	Amour blanc	15	\N	38
2108	Cyprinus	Carpe	13	\N	39
2109	Cyprinus carpio	Carpe miroir	15	\N	40
2234	Dicentrarchus labrax	Bar	15	\N	41
2151	Esox lucius	Brochet	15	\N	42
2113	Gobio gobio	Goujon	15	\N	43
2191	Gymnocephalus cernuus	Gremille	15	\N	44
2010	Lampetra	\N	13	\N	45
2012	Lampetra planeri	Lamproie de planer	15	\N	47
2117	Leucaspius delineatus	Able de Heckel	15	\N	49
2050	Lepomis gibbosus	Perche soleil	15	\N	48
2118	Leuciscus	\N	13	\N	50
2123	Leuciscus burdigalensis	Vandoise rostrée	15	\N	51
2120	Leuciscus cephalus	Chevaine	15	\N	52
2121	Leuciscus idus	Ide melanote	15	\N	53
2122	Leuciscus leuciscus	Vandoise	15	\N	54
2119	Leuciscus souffia	Blageon	15	\N	55
2181	Liza	\N	13	\N	56
2182	Liza aurata	Mulet doré	15	\N	57
2183	Liza ramada	Mulet porc	15	\N	58
3267	Liza saliens	Mulet sauteur	15	\N	59
2156	Lota lota	Lote de rivière	15	\N	60
2051	Micropterus	\N	13	\N	61
2052	Micropterus dolomieui	Black bass à petite bouche	15	\N	62
2053	Micropterus salmoides	Black bass à grande bouche	15	\N	63
2185	Mugil cephalus	Mulet cabot	15	\N	64
2216	Oncorhynchus mykiss	Truite arc-en-ciel	15	\N	65
2193	Perca fluviatilis	Perche	15	\N	66
2014	Petromyzon marinus	Lamproie marine	15	\N	67
2125	Phoxinus phoxinus	Vairon	15	\N	68
2203	Platichthys flesus	Flet commun	15	\N	69
2205	Pleuronectes platessa	Plie commune	15	\N	70
2133	Rutilus rutilus	Gardon	15	\N	71
2219	Salmo	\N	13	\N	72
2220	Salmo salar	Saumon atlantique	15	\N	73
2221	Salmo trutta fario	Truite de riviere	15	\N	74
2222	Salmo trutta lacustris	Truite de lac	15	\N	75
2224	Salmo trutta trutta	Truite de mer	15	\N	76
2227	Salvelinus fontinalis	Saumon de fontaine	15	\N	77
2195	Sander lucioperca	Sandre	15	\N	78
2135	Scardinius erythrophthalmus	Rotengle	15	\N	79
2238	Silurus glanis	Silure glane	15	\N	80
2247	Thymallus thymallus	Ombre commun	15	\N	81
2137	Tinca tinca	Tanche	15	\N	82
2197	Zingel asper	Apron	15	\N	83
2038	Anguilla anguilla	Anguille d'Europe	15	\N	1
2037	Anguilla	Anguille	13	\N	2
0	Taxon inconnu	Taxon inconnu	0	\N	82
872	Pacifastacus	Ecrevisse de Californie	13	\N	87
2115	Hypophthalmichthys molitrix	Carpe argentÃ©e\n	15	\N	85
2071	Barbatula barbatula	Loche franche	15	\N	84
2106	Ctenopharyngodon	Carpe chinoise	13	\N	86
2011	Lampetra fluviatilis	Lamproie de rivière	15	2010	46
2138	Vimba	Vimbe (genre)	13	\N	90
2139	Vimba vimba	Vimbe	15	2138	91
2067	cabilis taenia	loche de rivière	15	\N	100
2127	pimephales promelas	tête de boule	15	\N	100
2111	Cyprinus carpio	Carpe cuir	15	\N	40
9814	Cyprinus carpio	carpe koï	15	\N	100
5204	Abramis sapa	Brême du Danube	15	\N	101
\.


--
-- Data for Name: tr_typearretdisp_tar; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_typearretdisp_tar (tar_code, tar_libelle, tar_commentaires) FROM stdin;
1	Fonc normal	Fonctionnement normal
2	Arr ponctuel	Arret ponctuel lie au fonctionnement
3	Arr maint	Arret pour maintenance
4	Dysfonc	Dysfonctionnement (notamment dysfonctionnement hydraulique et dysfonctionnement video)
5	Non connu	Pas de suivi de donnees permettant de déterminer le fonctionnement du dispositif
\.


--
-- Data for Name: tr_typedc_tdc; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_typedc_tdc (tdc_code, tdc_libelle, tdc_definition) FROM stdin;
2	Compteur à résistivité	Tube vers lequel le poisson est guidé à l'aide de grilles et qui utilise les variations de résistivité au moment du passage du poisson pour compter un passage. Ces dispositifs nécessitent une calibration et ne permettent pas d'identifier l'espèce.
3	Analyse visuelle d'image	Analyse visuelle d'image, reconnaissance par bande vidéo, assitée par un ordinateur, inclue les echos générés par des radars multifaisceaux permettant l'interpretation d'échos accoustiques.
5	Comptage radio	Les comptages utilisent la technologie RFID, qui utilise des champs électromagnétiques générés par des antennes, des boucles ou des cables pour déclencher la réponse d'une marque (pit tag, cables NEDAP...).
4	Comptage acoustique	Comptage du passage sur la base de balises accoustiques qui détectent les signaux envoyés par les balises implantées sur les poissons.
6	Engin de pêche	Utilisation d'engins de pêche comme des verveux, filets, tézelle, dideau pour intercepter les poissons en migration, ces piégeages peuvent être partiels ou complets
1	Piégeage	Dispositif permettant de pièger le poisson, soit à l'aide d'une chute, soit à l'aide de grilles ou filets empêchant son retour
\.


--
-- Data for Name: tr_typedf_tdf; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_typedf_tdf (tdf_libelle, tdf_mnemonique, tdf_definition, tdf_code) FROM stdin;
Passe à ralentisseurs	PASSERALENTI	La passe à ralentisseurs est un canal rectiligne à pente relativement forte (entre 1/10 et 1/5 suivant le type de passe et l'espèce considérée), de section rectangulaire, dans lequel sont installés sur le fond uniquement (passes à ralentisseurs de fond suractifs, passes à ralentisseurs à chevrons épais) ou à la fois sur le fond et les parois latérales (passes à ralentisseurs plans) des déflecteurs destinés à réduire les vitesses moyennes de l'écoulement. Ces déflecteurs, de formes plus ou moins complexes, donnent naissance à des courants hélicoïdaux qui assurent une forte dissipation d'énergie au sein de l'écoulement.	1
Passe à bassins successifs	PASSEBASSIN	Dispositif très commun et de conception relativement ancienne, consistant à diviser la hauteur à franchir en plusieurs petites chutes formant une série de bassins. Il existe plusieurs types de communications entre bassins, le passage de l'eau pouvant s'effectuer soit par déversement de surface, soit par écoulement à travers un ou plusieurs orifices ménagés dans la cloison, soit encore par une ou plusieurs fentes ou échancrures. On rencontre également des passes de type mixte.	2
Ecluse à poissons	ECLUSEPOISS	L’écluse à poissons est un dispositif au fonctionnement voisin de celui observé pour une écluse de navigation. Les poissons sont attirés dans une chambre puis éclusés comme on écluserait un bateau.  On incite le poisson à sortir de l'écluse en créant à l'intérieur de celle- ci un courant descendant grâce à l'ouverture d'un by-pass situé dans la partie inférieure du dispositif. 	3
Exutoire de dévalaison	EXUTOIREDEVAL	Dispositif facilitant le franchissement d'un barrage par les poissons lors de leur migration vers l'aval. Ce dispositifif peut être.	4
Passe à anguille	PASANG	Rampe équipée d'un matériau facilitant la progression des jeunes anguilles à la montaison. Les matériaux employés peuvent être d'origine naturelle (cailloux, branchages, bruyère, paille) ou artificielle (brosses , plots en béton...). Ce sont essentiellement des substrats de type brosse qui sont utilisés aujourd'hui en France. L'espacement entre chaque faisceau de soies dépend de la taille des individus à faire passer.	5
Tapis Brosse	TAPBROSSE	\N	5a
Substrat rugueux	SUBRUGU	\N	5b
Passe piège	PASSPIEGE	\N	5c
Ascenseur à poissons	ASCPOISS	L'ascenseur à poissons permet de remonter les poissons, piégés dans une cuve, et de les déverser en amont de l'obstacle.	6
Pré-barrage	PREBAR	Dispositifs formés de plusieurs petits seuils, le plus souvent en béton ou enrochements jointoyés, créant à l'aval de l'obstacle des grands bassins qui fractionnent la chute à franchir. Ces prébarrages sont généralement implantés sur une partie de la largeur de l'obstacle, à proximité de l'une des deux rives pour en faciliter l'entretien.	7
Rampe	RAMPE	\N	8
Rivière de contournement	RIVIERE	Dispositif consistant à relier biefs amont et aval par un chenal dans lequel l'énergie est dissipée et les vitesses réduites par la rugosité du fond et celle des parois ainsi que par une succession d'obstacles (blocs, épis, seuils) plus ou moins régulièrement répartis, reproduisant en quelque sorte l'écoulement dans un cours d'eau naturel.	9
Autre type de dispositif	AUTRES	\N	10
Guidage par grille, filet, électricité	GUIDE	Le dispositif consiste en une grille ou un guidage par une barrière électrique permettant d'orienter les poissons vers le piège.	11
\.


--
-- Data for Name: tr_typequantitelot_qte; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_typequantitelot_qte (qte_code, qte_libelle) FROM stdin;
2	Volume
3	Résistivité
1	Poids (g)
\.


--
-- Data for Name: tr_valeurparametrequalitatif_val; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY tr_valeurparametrequalitatif_val (val_identifiant, val_qal_code, val_rang, val_libelle) FROM stdin;
2	1783	2	Mâle
3	1783	3	Femelle
4	1783	4	Indifférencié
5	1784	1	Spermiant
6	1784	2	Femelle pleine
7	1784	3	A commencé de frayer
8	1784	4	Pertes d'Ovules
9	1784	5	A frayé
10	1784	6	Marques de Frai
11	1784	7	Bouton de noces
12	1784	8	Non mature
13	1784	9	Non recherchée
14	1791	1	VA
15	1791	2	VB
16	1791	3	VIA0
17	1791	4	VIA1
18	1791	5	VIA2
19	1791	6	VIA3
20	1791	7	VIA4
21	1791	8	VIB
22	1425	1	Temps sec dégagé
23	1425	2	Temps sec couvert
24	1425	3	Temps humide
25	1425	4	Pluie
26	1425	5	Orage
27	1425	6	Neige
28	1425	7	Gel
29	A005	1	0 : Non renseigné
30	A005	2	1 : Nulle (fond visible)
31	A005	3	2 : Faible (Poissons identifiables)
32	A005	4	3 : Appréciable (difficultés d'identification)
33	B001	1	[0-10[
34	B001	2	[10-20[
35	B001	3	[20-30[
36	B001	4	[30-40[
37	B001	5	[40-50[
38	B001	6	[50-60[
39	B001	7	[60-70[
40	B001	8	[70-80[
41	B001	9	[80-90[
42	B001	10	[90-100[
43	B001	11	[100-110[
44	B001	12	[110-120[
45	B001	13	[120-130[
46	B001	14	[130-140[
47	B001	15	[140-150[
50	B002	3	Grandes anguillettes [145-300[
51	B002	4	Anguilles >300
49	B002	2	Moyennes anguillettes ]98-145[
48	B002	1	Petites anguillettes <=98
52	AAAA	1	premier quartier
53	AAAA	2	pleine lune
54	AAAA	3	dernier quartier
55	AAAA	4	nouvelle lune
56	CONT	1	0 pas de contraste
57	CONT	2	1 contraste
58	LINP	1	0 pas de ponctuation
59	LINP	2	1 présence de ponctuation
62	COHO	1	Cohorte de reproduction de l'année n
63	COHO	2	Cohorte de reproduction de l'année n+1
64	COHO	3	Cohorte de reproduction de l'année n-1
60	B003	1	<150
61	B003	2	>150
65	1783	5	Recherché mais non indentifié
1	1783	1	Inconnu
\.


--
-- Name: tr_valeurparametrequalitatif_val_val_identifiant_seq; Type: SEQUENCE SET; Schema: ref; Owner: postgres
--

SELECT pg_catalog.setval('tr_valeurparametrequalitatif_val_val_identifiant_seq', 64, true);


--
-- Data for Name: ts_maintenance_main; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY ts_maintenance_main (main_identifiant, main_ticket, main_description) FROM stdin;
1	59	creation de la table de maintenance
2	40	ajout des clé étrangères manquantes
3	42	modification des propriétaires sur les tables à séquence et grant select sur ref.tr_typedf_tdf oublié
5	72	creation d'une tableref.ts_messager_msr  pour l'internationalisation
6	72	creation d'une ref.ts_messagerlang_mrl  pour l'internationalisation
7	104	modification des messages dans ref.ts_messagerlang_mrl
\.


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE SET; Schema: ref; Owner: postgres
--

SELECT pg_catalog.setval('ts_maintenance_main_main_identifiant_seq', 7, true);


--
-- Data for Name: ts_messager_msr; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY ts_messager_msr (msr_id, msr_element, msr_number, msr_type, msr_endofline, msr_comment) FROM stdin;
109	BilanMigrationInterannuelle	1	class	t	\N
333	BilanMigration	10	class	t	\N
334	BilanMigrationInterannuelle	11	class	f	\N
336	fungraph	8	class	t	pasted output for numbers
230	interface_Bilan_poids_moyen	1	interface	f	glabel of title bilan poids moyen
343	funtraitement_poids	3	function	t	warning for incomplete coefficient input
18	Bilan_poids_moyen	2	class	t	 context nrows found for conversion coefficients
20	Bilan_poids_moyen	4	class	f	 at the start of a warning
21	Bilan_poids_moyen	5	class	f	 context weight of undrained glass eels
23	Bilan_poids_moyen	7	class	f	 all weights both cases selected
24	Bilan_poids_moyen	8	class	f	graphs
25	Bilan_poids_moyen	9	class	f	graphs
26	Bilan_poids_moyen	10	class	f	graphs, within a paste
27	Bilan_poids_moyen	11	class	f	graphs
28	Bilan_poids_moyen	12	class	f	graphs
29	Bilan_poids_moyen	13	class	f	graphs
32	Bilan_poids_moyen	16	class	f	frame question
33	Bilan_poids_moyen	17	class	f	frame title
34	Bilan_poids_moyen	18	class	f	button message for graphes
35	Bilan_poids_moyen	19	class	f	button message for coefficients
36	Bilan_poids_moyen	20	class	f	button message for size (bubbles of different size according to the numbers
37	Bilan_poids_moyen	21	class	f	button message for regression
38	Bilan_poids_moyen	22	class	f	button message to export the data
39	Bilan_poids_moyen	23	class	f	button message button to quit
42	Bilan_stades_pigm	3	class	f	 glablel
68	BilanFonctionnementDC	3	class	f	graph xlabel
69	BilanFonctionnementDC	4	class	f	graph ylabel
70	BilanFonctionnementDC	5	class	f	graph title
71	BilanFonctionnementDC	6	class	f	graph legend
72	BilanFonctionnementDC	7	class	f	graph legend
74	BilanFonctionnementDC	9	class	f	graph boxes ylab
75	BilanFonctionnementDC	10	class	f	graph boxes legend
76	BilanFonctionnementDC	11	class	f	graph boxes legend
77	BilanFonctionnementDC	12	class	f	graph boxes upper box title
78	BilanFonctionnementDC	13	class	f	graph boxes lower box title
79	BilanFonctionnementDC	14	class	f	… path …
84	BilanFonctionnementDF	4	class	f	 progressbar
85	BilanFonctionnementDF	5	class	f	 progressbar
110	BilanMigrationInterannuelle	2	class	f	 part of a message
112	BilanMigrationInterannuelle	4	class	f	graph : pasted within the title
114	BilanMigrationInterannuelle	6	class	f	graph cum group
115	BilanMigrationInterannuelle	7	class	f	graph cum X
116	BilanMigrationInterannuelle	8	class	f	graph cum Y
1	ref	1	referential	t	\N
117	BilanMigrationInterannuelle	9	class	f	graph cum within title
342	interface_graphique	21	interface	t	message de lancement du handler des migrations multiples
125	BilanMigrationPar	7	class	f	 path
150	RefDC	4	class	f	 button
151	RefDC	5	class	f	 title
163	Refparquan	1	class	f	TODO check this class
164	Refperiode	1	class	f	TODO check this class
165	RefpoidsMoyenPeche	1	class	f	TODO check this class
168	RefStades	3	class	f	 this is a frame label
169	RefStades	4	class	f	 this is a frame label
171	RefStades	6	class	f	 this is a frame label
174	RefStationMesure	3	class	f	 frame label
177	RefTaxon	2	class	f	 frame label
191	fungraph_civelle	1	function	f	 followed by dis_commentaire
192	fungraph_civelle	2	function	f	 title
215	fungraph	7	function	f	 column names
222	interface_Bilan_lot	2	interface	f	 glablel
227	interface_Bilan_lot	7	interface	f	 tooltip
228	interface_Bilan_lot	8	interface	f	 tooltip
229	interface_Bilan_lot	9	interface	f	 tootip and name
237	interface_Bilan_taille	2	interface	f	 tooltip
239	interface_BilanConditionEnv	2	interface	f	 tootip
243	interface_BilanEspeces	2	interface	f	 glabel title
248	interface_BilanEspeces	7	interface	f	 label de la boite liste
250	interface_BilanMigration	2	interface	f	 tooltip
254	interface_BilanMigrationConditionEnv	1	interface	f	 
261	interface_BilanMigrationInterannuelle	6	interface	f	 tooltip
262	interface_BilanMigrationInterannuelle	7	interface	f	 tooltip
263	interface_BilanMigrationInterannuelle	8	interface	f	 tooltip
264	interface_BilanMigrationInterannuelle	9	interface	f	 tooltip
281	interface_BilanFonctionnementDC	2	interface	f	 tootip
300	interface_graphique	16	interface	f	 TITRE PRINCIPAL !!!
2	ref	2	referential	t	\N
3	ref	3	referential	t	\N
4	ref	4	referential	t	\N
5	ref	5	referential	t	\N
6	ref	6	referential	t	\N
7	ref	7	referential	t	\N
8	ref	8	referential	t	\N
9	ref	9	referential	t	\N
10	ref	10	referential	t	\N
11	ref	11	referential	t	\N
12	ref	12	referential	t	\N
17	Bilan_poids_moyen	1	class	t	\N
19	Bilan_poids_moyen	3	class	t	\N
22	Bilan_poids_moyen	6	class	f	\N
30	Bilan_poids_moyen	14	class	t	\N
31	Bilan_poids_moyen	15	class	f	\N
40	Bilan_stades_pigm	1	class	t	\N
41	Bilan_stades_pigm	2	class	t	\N
43	Bilan_stades_pigm	4	class	f	\N
44	Bilan_stades_pigm	5	class	f	\N
45	Bilan_stades_pigm	6	class	f	\N
46	Bilan_taille	1	class	t	\N
47	Bilan_taille	2	class	t	\N
48	Bilan_taille	3	class	t	\N
49	Bilan_taille	4	class	t	\N
50	Bilan_taille	5	class	t	\N
51	Bilan_taille	6	class	t	\N
52	Bilan_taille	7	class	t	\N
53	Bilan_taille	8	class	t	\N
54	BilanCondtionEnv	1	class	t	\N
55	BilanCondtionEnv	2	class	t	\N
56	BilanCondtionEnv	3	class	t	\N
57	BilanCondtionEnv	4	class	t	\N
58	BilanCondtionEnv	5	class	t	\N
59	BilanEspeces	1	class	t	\N
60	BilanEspeces	2	class	t	\N
61	BilanEspeces	3	class	t	\N
62	BilanEspeces	4	class	t	\N
63	BilanEspeces	5	class	t	\N
64	BilanEspeces	6	class	t	\N
65	BilanEspeces	7	class	t	\N
66	BilanFonctionnementDC	1	class	t	\N
67	BilanFonctionnementDC	2	class	t	\N
73	BilanFonctionnementDC	8	class	t	\N
80	BilanFonctionnementDC	15	class	t	\N
81	BilanFonctionnementDF	1	class	t	\N
82	BilanFonctionnementDF	2	class	t	\N
83	BilanFonctionnementDF	3	class	t	\N
86	BilanFonctionnementDF	6	class	f	\N
87	BilanFonctionnementDF	7	class	f	\N
88	BilanFonctionnementDF	8	class	t	\N
89	BilanFonctionnementDF	9	class	f	\N
90	BilanFonctionnementDF	10	class	f	\N
91	BilanMigration	1	class	t	\N
92	BilanMigration	2	class	t	\N
93	BilanMigration	3	class	t	\N
94	BilanMigration	4	class	t	\N
95	BilanMigration	5	class	t	\N
96	BilanMigration	6	class	f	\N
97	BilanMigration	7	class	f	\N
98	BilanMigration	8	class	t	\N
99	BilanMigration	9	class	t	\N
100	BilanMigrationConditionEnv	1	class	t	\N
101	BilanMigrationConditionEnv	2	class	t	\N
102	BilanMigrationConditionEnv	3	class	t	\N
103	BilanMigrationConditionEnv	4	class	t	\N
104	BilanMigrationConditionEnv	5	class	f	\N
105	BilanMigrationConditionEnv	6	class	f	\N
106	BilanMigrationConditionEnv	7	class	f	\N
107	BilanMigrationConditionEnv	8	class	f	\N
108	BilanMigrationConditionEnv	9	class	t	\N
111	BilanMigrationInterannuelle	3	class	f	\N
113	BilanMigrationInterannuelle	5	class	f	\N
118	BilanMigrationInterannuelle	10	class	f	\N
119	BilanMigrationPar	1	class	t	\N
120	BilanMigrationPar	2	class	t	\N
121	BilanMigrationPar	3	class	t	\N
122	BilanMigrationPar	4	class	t	\N
123	BilanMigrationPar	5	class	t	\N
124	BilanMigrationPar	6	class	t	\N
132	PasdeTemps	1	class	f	\N
133	PasdeTemps	2	class	f	\N
134	PasdeTemps	3	class	t	\N
135	PasdeTemps	4	class	f	\N
136	PasdeTempsJournalier	1	class	f	\N
137	PasdeTempsJournalier	2	class	f	\N
138	PasdeTempsJournalier	3	class	f	\N
139	PasdeTempsJournalier	4	class	f	\N
140	PasdeTempsJournalier	5	class	f	\N
141	PasdeTempsJournalier	6	class	f	\N
142	PasdeTempsJournalier	7	class	f	\N
143	PasdeTempsJournalier	8	class	t	\N
144	RefAnnee	1	class	f	\N
145	RefAnnee	2	class	t	\N
146	RefAnnee	3	class	f	\N
147	RefDC	1	class	t	\N
148	RefDC	2	class	t	\N
149	RefDC	3	class	f	\N
152	RefDC	6	class	f	\N
153	RefDC	7	class	t	\N
154	RefDF	1	class	t	\N
155	RefDF	2	class	f	\N
156	RefDF	3	class	f	\N
157	RefDF	4	class	t	\N
158	Refpar	1	class	t	\N
159	Refpar	2	class	t	\N
160	Refpar	3	class	t	\N
161	Refpar	4	class	t	\N
162	Refparqual	1	class	f	\N
166	RefStades	1	class	t	\N
167	RefStades	2	class	t	\N
170	RefStades	5	class	t	\N
172	RefStationMesure	1	class	t	\N
173	RefStationMesure	2	class	t	\N
175	RefStationMesure	4	class	t	\N
176	RefTaxon	1	class	t	\N
178	RefTaxon	3	class	t	\N
179	RequeteODBC	1	class	t	\N
180	RequeteODBC	2	class	f	\N
181	RequeteODBC	3	class	f	\N
182	RequeteODBC	4	class	t	\N
183	RequeteODBC	5	class	t	\N
184	RequeteODBC	6	class	t	\N
185	fn_EcritBilanJournalier	1	function	f	\N
186	fn_EcritBilanJournalier	2	function	f	\N
187	fn_EcritBilanJournalier	3	function	f	\N
188	fn_EcritBilanJournalier	4	function	f	\N
189	fn_EcritBilanJournalier	5	function	f	\N
190	fn_EcritBilanMensuel	1	function	f	\N
193	fungraph_civelle	3	function	f	\N
194	fungraph_civelle	4	function	f	\N
195	fungraph_civelle	5	function	f	\N
196	fungraph_civelle	6	function	f	\N
197	fungraph_civelle	7	function	f	\N
198	fungraph_civelle	8	function	f	\N
199	fungraph_civelle	9	function	f	\N
200	fungraph_civelle	10	function	f	\N
201	fungraph_civelle	11	function	f	\N
202	fungraph_civelle	12	function	f	\N
203	fungraph_civelle	13	function	f	\N
204	fungraph_civelle	14	function	f	\N
205	fungraph_civelle	15	function	f	\N
206	fungraph_civelle	16	function	f	\N
207	fungraph_env	1	function	f	\N
208	fungraph_env	2	function	t	\N
209	fungraph	1	function	f	\N
210	fungraph	2	function	f	\N
211	fungraph	3	function	f	\N
212	fungraph	4	function	f	\N
213	fungraph	5	function	f	\N
214	fungraph	6	function	f	\N
216	funstat	1	function	t	\N
217	funstat	2	function	f	\N
218	funtable	1	function	f	\N
219	funtraitement_poids	1	function	t	\N
220	funtraitement_poids	2	function	t	\N
221	interface_Bilan_lot	1	interface	t	\N
223	interface_Bilan_lot	3	interface	f	\N
224	interface_Bilan_lot	4	interface	f	\N
225	interface_Bilan_lot	5	interface	t	\N
226	interface_Bilan_lot	6	interface	t	\N
235	interface_Bilan_poids_moyen	6	interface	f	\N
236	interface_Bilan_taille	1	interface	f	\N
238	interface_BilanConditionEnv	1	interface	t	\N
240	interface_BilanConditionEnv	3	interface	f	\N
241	interface_BilanConditionEnv	4	interface	f	\N
242	interface_BilanEspeces	1	interface	t	\N
244	interface_BilanEspeces	3	interface	f	\N
245	interface_BilanEspeces	4	interface	f	\N
246	interface_Bilanespeces	5	interface	f	\N
247	interface_BilanEspeces	6	interface	f	\N
249	interface_BilanMigration	1	interface	t	\N
251	interface_BilanMigration	3	interface	f	\N
252	interface_BilanMigration	4	interface	f	\N
253	interface_BilanMigration	5	interface	f	\N
255	interface_BilanMigrationConditionEnv	2	interface	t	\N
256	interface_BilanMigrationInterannuelle	1	interface	t	\N
257	interface_BilanMigrationInterannuelle	2	interface	t	\N
258	interface_BilanMigrationInterannuelle	3	interface	f	\N
259	interface_BilanMigrationInterannuelle	4	interface	t	\N
260	interface_BilanMigrationInterannuelle	5	interface	f	\N
265	interface_BilanMigrationInterannuelle	10	interface	f	\N
266	interface_BilanMigrationInterannuelle	11	interface	f	\N
267	interface_BilanMigrationInterannuelle	12	interface	f	\N
268	interface_BilanMigrationInterannuelle	13	interface	f	\N
269	interface_BilanMigrationInterannuelle	14	interface	f	\N
270	interface_BilanMigrationInterannuelle	15	interface	f	\N
271	interface_BilanMigrationInterannuelle	16	interface	f	\N
272	interface_BilanMigrationInterannuelle	17	interface	f	\N
273	interface_BilanMigrationPar	1	interface	t	\N
274	interface_BilanMigrationPar	2	interface	f	\N
275	interface_BilanMigrationPar	3	interface	f	\N
276	interface_BilanMigrationPar	4	interface	f	\N
277	interface_BilanMigrationPar	5	interface	f	\N
278	interface_BilanMigrationPar	6	interface	f	\N
279	interface_BilanMigrationPar	7	interface	f	\N
280	interface_BilanFonctionnementDC	1	interface	t	\N
282	interface_BilanFonctionnementDC	3	interface	f	\N
283	interface_BilanFonctionnementDC	4	interface	f	\N
284	interface_BilanFonctionnementDC	5	interface	f	\N
285	interface_graphique	1	interface	t	\N
286	interface_graphique	2	interface	t	\N
287	interface_graphique	3	interface	t	\N
288	interface_graphique	4	interface	t	\N
289	interface_graphique	5	interface	t	\N
290	interface_graphique	6	interface	t	\N
291	interface_graphique	7	interface	t	\N
292	interface_graphique	8	interface	t	\N
293	interface_graphique	9	interface	t	\N
294	interface_graphique	10	interface	t	\N
295	interface_graphique	11	interface	t	\N
296	interface_graphique	12	interface	t	\N
297	interface_graphique	13	interface	t	\N
298	interface_graphique	14	interface	t	\N
301	interface_graphique	17	interface	f	\N
302	interface_graphique	18	interface	t	\N
303	interface_graphique	19	interface	t	\N
304	interface_graphique_menu	1	interface	f	\N
305	interface_graphique_menu	1.1	interface	f	\N
306	interface_graphique_menu	1.2	interface	f	\N
307	interface_graphique_menu	1.3	interface	f	\N
308	interface_graphique_menu	1.4	interface	f	\N
309	interface_graphique_menu	2	interface	f	\N
310	interface_graphique_menu	2.1	interface	f	\N
311	interface_graphique_menu	2.2	interface	f	\N
312	interface_graphique_menu	2.3	interface	f	\N
313	interface_graphique_menu	2.4	interface	f	\N
314	interface_graphique_menu	2.5	interface	f	\N
315	interface_graphique_menu	2.6	interface	f	\N
316	interface_graphique_menu	2.7	interface	f	\N
317	interface_graphique_menu	2.8	interface	f	\N
318	interface_graphique_menu	2.9	interface	f	\N
319	interface_graphique_menu	2.10	interface	f	\N
320	interface_graphique_menu	2.11	interface	f	\N
321	interface_graphique_menu	2.12	interface	f	\N
322	interface_graphique_menu	2.13	interface	f	\N
323	interface_graphique_menu	3	interface	f	\N
324	interface_graphique_log	1	interface	f	\N
325	interface_graphique_log	2	interface	f	\N
326	interface_graphique_log	3	interface	f	\N
327	interface_graphique_log	4	interface	f	\N
328	interface_graphique_log	5	interface	f	\N
329	interface_graphique_log	6	interface	f	\N
231	interface_Bilan_poids_moyen	2	interface	f	gbutton
232	interface_Bilan_poids_moyen	3	interface	f	gbutton
233	interface_Bilan_poids_moyen	4	interface	f	gbutton
234	interface_Bilan_poids_moyen	5	interface	f	frame
330	interface_graphique_log	7	interface	f	\N
331	interface_graphique_log	8	interface	f	\N
332	interface_graphique_log	9	interface	f	\N
344	interface_Bilan_lot	10	interface	f	tooltip and name
345	interface_Bilan_lot	11	interface	f	tooltip and name
126	ConnectionODBC	1	class	t	note messages here are not printed in the console, sono use for this, kept for record
127	ConnectionODBC	2	class	t	note messages here are not printed in the console, sono use for this, kept for record
128	ConnectionODBC	3	class	t	note messages here are not printed in the console, sono use for this, kept for record
129	ConnectionODBC	4	class	t	note messages here are not printed in the console, sono use for this, kept for record
130	ConnectionODBC	5	class	f	note messages here are not printed in the console, sono use for this, kept for record
131	ConnectionODBC	6	class	f	\N
346	interface_graphique_menu	2.14	interface	f	\N
347	RefDC	8	class	t	\N
348	RefTaxon	4	class	t	\N
349	RefTaxon	5	class	t	\N
350	RefTaxon	6	class	t	\N
351	RefStades	7	class	t	\N
352	RefStades	8	class	t	\N
353	BilanMigrationMult	1	class	f	Bouton d'action pour imprimer la commande générée par l'interface graphique
354	BilanMigrationMult	2	class	t	Message pour l'affichage de la commande out, quand le bilan n'est pas complet
355	BilanMigrationMult	3	class	t	Récupération des données
356	BilanMigrationMult	4	class	t	Récupération des données
357	BilanFonctionnementDF	11	class	f	per_tar_code
358	BilanFonctionnementDF	12	class	f	per_tar_code
359	BilanFonctionnementDF	13	class	f	per_tar_code
360	BilanOperation	1	class	f	connect
13	Bilan_carlot	1	class	t	\N
14	Bilan_carlot	2	class	t	\N
15	Bilan_carlot	3	class	t	\N
16	Bilan_carlot	4	class	t	\N
361	BilanAnnuels	1	class	f	methode charge, récupération de l'objet
362	BilanArgentee	1	class	f	methode connect, message de réussite
363	BilanArgentee	2	class	f	methode calcul, error for no data
\.


--
-- Name: ts_messager_msr_msr_id_seq; Type: SEQUENCE SET; Schema: ref; Owner: postgres
--

SELECT pg_catalog.setval('ts_messager_msr_msr_id_seq', 1, true);


--
-- Data for Name: ts_messagerlang_mrl; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY ts_messagerlang_mrl (mrl_id, mrl_msr_id, mrl_text, mrl_lang) FROM stdin;
1	1	"Il faut choisir un dispositif de comptage,cliquez sur valider "	French
2	2	"Il faut choisir un taxon,cliquez sur valider "	French
3	3	"Il faut choisir un stade,cliquez sur valider "	French
4	4	"Il faut choisir un parametre,cliquez sur valider "	French
5	5	"Il faut choisir la date de debut"	French
6	6	"Il faut choisir la date de fin"	French
7	7	"Il faut choisir un paramètre quantitatif"	French
8	8	"Il faut choisir un parametre qualitatif"	French
9	9	"Il faut choisir une categorie d'effectif"	French
10	10	"Il faut choisir l'annee de debut"	French
11	11	"Il faut choisir l'annee de fin"	French
12	12	"il faut choisir un dispositif de franchissement,cliquez sur valider "	French
13	13	"La requete est effectuee pour charger les caracteristiques de lot "	French
14	14	"Aucune donnee pour ces lots dans la periode selectionnee"	French
17	17	"La requete est effectuee pour charger les coefficients de conversion "	French
18	18	"lignes trouvees pour les coefficients de conversion"	French
20	20	"effectif non renseigne, lots" 	French
21	21	"humides" 	French
22	22	"secs"	French
23	23	"humides et secs" 	French
24	24	"date"	French
25	25	"poids moyens"	French
26	26	"Tendance saisonniere des poids" 	French
27	27	", anneedebut="	French
28	28	", anneefin="	French
29	29	"modele sinusoidal,a.cos.2pi.(jour-T)/365)+b "	French
31	31	"Repertoire d'ecriture des donnees :"	French
32	32	"Voulez vous ecrire les donnees dans la base ?"	French
33	33	"attention"	French
34	34	"Gra" 	French
35	35	"Coe" 	French
36	36	"Tail" 	French
37	37	"Reg" 	French
38	38	"export" 	French
39	39	"quitter" 	French
40	40	"La requete a été effectuée pour charger les stades pigmentaires"	French
41	41	"Bilan des stades pigmentaires"	French
42	42	"BILAN STADES PIGMENTAIRES" 	French
43	43	"Stades pigmentaires"	French
44	44	"et dates d'arrivees en estuaire"	French
45	45	"Choix du titre"	French
46	46	"Il faut choisir au moins une caracteristique quantitative ou qualitative"	French
47	47	"Attention cette requete (croisement de deux vues) peut prendre plusieurs minutes, soyez patient(e)..."	French
49	49	"La requete : requete=get("bilan_taille",envir_stacomi)@requete@sql "	French
51	51	"Donnees directement issues de la requete : don=get("bilan_taille",envir_stacomi)@requete@query"	French
52	52	"Il faut d'abord faire le calcul, appuyez sur calcul"	French
53	53	"Requete effectuee"	French
54	54	"La requete est effectuee pour charger les conditions environnementales "	French
55	55	"Il faut choisir une station de mesure puis cliquer sur valider "	French
56	56	"Certaines stations de mesure n'ont pas de valeurs associees"	French
57	57	"Aucune valeur de conditions environnementales pour les stations de mesure selectionnees (BilanConditionEnv.r)"	French
58	58	"Statistiques :"	French
60	60	"Il faut faire tourner les calculs avant, cliquer sur calc "	French
61	61	"Echec de la requete vers la vue vue_ope_lot_car "	French
62	62	"Il faut lancer les calculs avant cliquez sur calcul"	French
63	63	"Il n'y a aucun poisson dans la base sur cette periode"	French
64	64	"Attention certains effectifs négatifs sont transformés en positifs"	French
65	65	"Verifications des objets et lancement de la requete"	French
66	66	"La requete est effectuee pour charger les pas de temps du DC "	French
67	67	"Il n'y a pas de donnees sur ce DC "	French
68	68	"mois" 	French
69	69	"temps en heures" 	French
70	70	"Fonctionnement du dispositif de comptage" 	French
71	71	"suivi video" 	French
72	72	"arret video" 	French
74	74	"DC" 	French
75	75	c("Fonc","Arr","Fonc normal")	French
76	76	c("Fonc","Arr")	French
77	77	"Fonctionnement DC" 	French
78	78	"Types d'arrets du DC" 	French
79	79	"Ecriture de" 	French
80	80	"peut prendre un peu de temps, soyez patient...."	French
81	81	"La requete est effectuee pour charger les pas de temps du DF "	French
82	82	"Il n'y a pas de donnees sur ce DF "	French
83	83	"Construction du graphe, patientez "	French
84	84	"calcul..." 	French
85	85	"progression %" 	French
86	86	c("duree","type_fonct.","fonctionnement")	French
87	87	"Fonctionnement DF"	French
89	89	"DF"	French
73	73	"Ecriture de tableau dans l'environnement envir_stacomi : ecrire periodeDC=get('periodeDC',envir_stacomi) "	French
19	19	"Pour recuperer le tableau, tapper : bilan_poids_moyen=get('bilan_poids_moyen',envir_stacomi)@data"	French
30	30	"Pour recuperer le tableau, tapper : import_coe=get('import_coe',envir_stacomi)"	French
48	48	"Pour recuperer le tableau, tapper : bilan_taille=get('bilan_taille',envir_stacomi)"	French
50	50	"Les donnees : donnees_taille=get('bilan_taille',envir_stacomi)@data"	French
59	59	"L'objet Bilan est stocke dans l'environnement envir_stacomi, ecrire  ecrire bilanEspeces=get('bilanEspeces',envir_stacomi), "	French
90	90	"Types d'arrets du DF"	French
290	290	"Bilan migration inter-annuel"	French
91	91	"Attention le choix du pas de temps n'a pas ete effectue, calcul avec la valeur par defaut "	French
92	92	"Debut du bilan migration... patientez "	French
95	95	"Il faut faire tourner les calculs avant, cliquer sur calc "	French
96	96	"Migration cumulee"	French
97	97	"Effectif cumule, "	French
98	98	"Attention cette fonction est pour les bilans annuels "	French
99	99	"Statistiques concernant la migration : "	French
100	100	"L'objet Bilan est stocke dans l'environnement envir_stacomi"	French
101	101	"Il faut faire tourner les calculs avant, cliquer sur calc"	French
102	102	"Vous n'avez pas de conditions environnementales sur la periode de temps"	French
103	103	"pas station selectionnee => graphe simple"	French
104	104	"le nombre de lignes du tableau des conditions environnentales ("	French
105	105	") ne correspond pas a la duree du bilan Migration ("	French
106	106	"Attention : sur une des stations :"	French
107	107	"il y a plusieurs enregistrements pour la même journée : "	French
108	108	"seule la première valeur sera intégrée dans le bilan "	French
109	109	"Attention il n'existe pas de bilan migration pour l'annee "	French
110	110	", ce taxon et ce stade (BilanMigrationInterAnnuelle.r)" 	French
111	111	"La requete est effectuee pour charger les migrations sur les annees"	French
112	112	"Effectifs" 	French
113	113	"ATTENTION : Veuillez effectuer un Bilan Migration pour au moins une des annees selectionnees avant de lancer un bilan inter-annuel"	French
114	114	"annee" 	French
115	115	"date" 	French
116	116	"Pourcentage de la migration annuelle" 	French
119	119	"Debut du bilan migration avec parametres... patientez "	French
120	120	"Il faut choisir au moins une caracteristique quantitative ou qualitative"	French
121	121	"Attention, ce traitement ne s'effectue pas sur les quantites de lots "	French
123	123	"Il faut faire tourner les calculs avant"	French
125	125	"Ecriture de"	French
126	126	"Il faut definir un vecteur baseODBC avec le lien ODBC, l'utilistateur et le mot de passe"	French
127	127	"La librairie RODBC est necessaire, chargez le package ! "	French
128	128	"Essai de connexion, attention cette classe ne doit être utilisée que pour les tests : "	French
129	129	"Connexion impossible :"	French
130	130	"Connexion établie"	French
131	131	"Connexion en cours"	French
132	132	"Choix des Pas de Temps"	French
133	133	"Choix du nombre de pas de temps"	French
134	134	"Erreur interne : le tableau des pas de temps ne contient aucune ligne"	French
135	135	"Date de fin"	French
136	136	"la duree du pas devrait etre journaliere"	French
137	137	"le pas de temps ne doit pas etre a cheval sur plusieurs annnees"	French
138	138	"Choix des Pas de Temps (duree 1 an)"	French
139	139	"Date de debut"	French
140	140	"Pas de temps"	French
141	141	"Nb jour"	French
142	142	"Date de fin"	French
143	143	"Les pas de temps ont ete charges"	French
144	144	"Choix de l'annee"	French
145	145	"Annee selectionnee"	French
146	146	"probleme lors du chargement des donnees ou pas de donnees dans la base (lien ODBC ?)"	French
147	147	"Le DC a ete selectionne "	French
148	148	"selection des taxons du DC (pour l'instant sur toutes les periodes) "	French
149	149	"Donnees sur les Dispositifs de Comptage"	French
150	150	"fermer" 	French
151	151	"Choix du Dispositif de Comptage"	French
152	152	"Tableau"	French
153	153	"Erreur : Aucun DC n'est rentre dans la base (aucune ligne de retour de la requete)"	French
154	154	"Le DF a ete selectionne "	French
155	155	"Donnees sur les Dispositifs de Franchissement"	French
156	156	"Choix du Dispositif de Franchissement"	French
157	157	"Aucun DF n'est rentre dans la base (aucune ligne de retour de la requete)"	French
158	158	"La requete est effectuee pour charger les parametres "	French
159	159	"Pas de donnees pour ce DC, ce taxon et ce stade "	French
160	160	"La caracteristique a ete selectionnee "	French
161	161	"erreur interne, aucune caracteristique n'a pu être chargée pour faire le choix "	French
162	162	"Erreur interne : il devrait y avoir une ligne dans Refparqual@data, or nbligne="	French
163	163	0	French
164	164	0	French
165	165	0	French
166	166	"Pas de donnees pour ce DC, et ce taxon "	French
167	167	"Le stade a ete selectionne "	French
168	168	"Caracteristique qualitative" 	French
169	169	"Caracteristique quantitative" 	French
170	170	"Stop erreur interne : charger les donnees pour faire le choix "	French
171	171	"Choix du Stade" 	French
172	172	"selectionnez au moins une valeur"	French
173	173	"Les stations de mesure ont ete selectionnees "	French
174	174	"Choix des stations de mesure" 	French
175	175	"Stop il n'y  a aucune donnee de station de mesure (pb lien ODBC ?) "	French
118	118	"Ecriture de l'objet graphique dans l'environnement envir_stacomi : ecrire g=get('gi',envir_stacomi) avec "	French
122	122	"Ecriture de data dans l'environnement envir_stacomi : ecrire data=get('data',envir_stacomi) "	French
124	124	"Ecriture de l'objet graphique dans l'environnement envir_stacomi : ecrire g=get('g',envir_stacomi) "	French
176	176	"Le taxon a ete selectionne "	French
177	177	"Choix du Taxon" 	French
178	178	"Stop il n'y  a aucune ligne dans la table des taxons (pb lien ODBC ?) "	French
179	179	"Erreur ODBC =>Il faut definir un vecteur baseODBC avec le lien ODBC, l'utilistateur et le mot de passe "	French
180	180	"Essai de connexion :"	French
181	181	"Connexion impossible :"	French
182	182	"Connexion reussie "	French
183	183	"Essai de la requete "	French
184	184	"Requete reussie "	French
185	185	"Un Bilan a deja ete ecrit dans la base le :"	French
186	186	"voulez vous le remplacer ?"	French
187	187	"ecriture du bilan journalier dans la base"	French
188	188	"progression %"	French
189	189	"ecriture du bilan journalier dans la base"	French
190	190	"ecriture du bilan mensuel dans la base"	French
191	191	"graph civelle :" 	French
192	192	"Effectif de civelles (x1000)" 	French
193	193	"Effectif estimes,"	French
194	194	c("eff. journ. poids","eff. journ. compt.")	French
195	195	"nombre d'operations="	French
196	196	"duree moyenne du piegeage="	French
197	197	"duree max="	French
198	198	"duree min="	French
199	199	c("Fonc","Arr","Fonc normal")	French
200	200	c("Fonc","Arr")	French
201	201	"DF"	French
202	202	"DC"	French
203	203	"OP"	French
204	204	"Mois"	French
205	205	"Effectif (x1000)"	French
206	206	c("eff. mens. poids","eff. mens. compt.")	French
207	207	"Effectif"	French
210	210	"effectif"	French
211	211	"Date"	French
212	212	"Effectif estime, "	French
214	214	"somme effectifs ="	French
215	215	c("Effectifs","type","duree","mois","quinzaine","semaine","jour_365") 	French
216	216	"calcul des bilans mensuels"	French
217	217		French
218	218	"ecriture de"	French
219	219	"Conversion poids effectif "	French
220	220	"Attention somme =0,vous n'avez pas encore rentre les coef de conversion"	French
221	221	"chargement de la vue (vue_ope_lot) et choix du dc et des pas de temps"	French
222	222	"BILAN LOTS" 	French
223	223	"horodate de debut"	French
224	224	"horodate de fin"	French
225	225	"La date de debut a ete choisie"	French
226	226	"La date de fin a ete choisie"	French
228	228	"tableau" 	French
229	229	"Quitter" 	French
235	235		French
236	236		French
237	237	"requete croisee taille/ caract qualitative"	French
238	238	"Chargement des stations de mesure "	French
239	239	"graphe bilan" 	French
240	240	"tables bilan en .csv"	French
241	241	"Quitter"	French
242	242	"Bilan des especes presentes sur le DC"	French
243	243	"Bilan Especes" 	French
244	244	"Chargement"	French
245	245	"Graphe PieChart"	French
246	246	"Histogramme"	French
247	247	"Tables bilan en .csv et XML"	French
248	248	"Choix du decoupage" 	French
249	249	"Chargement des listes taxons et stades et dc"	French
250	250	"Calcul des effectifs par pas de temps" 	French
251	251	"Graphe bilan"	French
252	252	"Graphe cumul"	French
253	253	"Tables bilan en .csv"	French
254	254	"calcul des condition environnementales par pas de temps" 	French
255	255	"Chargement des listes taxons et stades et dc et stations de mesure "	French
256	256	"Chargement des bilanJournaliers existants"	French
257	257	"L'annee de debut a ete choisie"	French
258	258	"Annee de debut"	French
259	259	"L'annee de fin a ete choisie"	French
260	260	"Annee de fin"	French
261	261	"Migration de toutes les annees dans le meme graphique" 	French
262	262	"cumul migratoires en %" 	French
263	263	"Tableau" 	French
264	264	"Quitter" 	French
265	265	"jour"	French
266	266	"Migration journalière"	French
267	267	"sem"	French
268	268	"Migration hebdomadaire"	French
269	269	"quin"	French
270	270	"Migration par quinzaine"	French
271	271	"mois"	French
272	272	"Migration mensuelle"	French
273	273	"Chargement des listes taxons,stades,dc, parametres qualitatifs et quantitatifs  "	French
274	274	"Choix du type de lot, inclusion des echantillons ?"	French
275	275	"Calcul des effectifs par pas de temps"	French
276	276	"graphe mensuel"	French
277	277	"graphe journalier"	French
278	278	"tables bilan en .csv"	French
279	279	"quitter"	French
280	280	"Chargement des listes dc et choix pas de temps"	French
281	281	"Graphe mensuel"	French
282	282	"Diagramme en boites"	French
283	283	"tableau"	French
284	284	"Quitte"	French
285	285	"Calculs du fonctionnement du df"	French
286	286	"Calculs du fonctionnement du dc"	French
230	230	"Bilan Poids Moyen"	French
232	232	"table"	French
231	231	"charge"	French
233	233	"quitter"	French
234	234	"choix de la catégorie d'effectif"	French
287	287	"Bilan des operations d'un dispositif ...a developper"	French
288	288	"Bilan croises du fonctionnement du DF et du DC,a developper "	French
289	289	"Bilan migration (pour une espèce et un stade)"	French
291	291	"Bilan migration conditions environnementales"	French
292	292	"Bilan migration avec parametres"	French
293	293	"Bilan des conditions environnementales"	French
294	294	"Bilan lots par appel de la vue vue lot ope"	French
295	295	"Bilan tailles "	French
297	297	"Calcul des stades pigmentaires "	French
300	300	"Traitement migrateur" 	French
301	301	"TODO à développer" 	French
302	302	"sorties du programme"	French
303	303	"Bilan des espèces du DC"	French
304	304	"Station"	French
305	305	"DF"	French
306	306	"DC"	French
307	307	"Operation (TODO)"	French
308	308	"DF sans DC (TODO)"	French
309	309	"Bilan"	French
310	310	"Migration"	French
311	311	"Cond. Env."	French
312	312	"Migr.~Cond. Env."	French
313	313	"Migr./ parm. quant / parm. qual"	French
314	314	"Migr. interannuel"	French
315	315	"Parm. de lot"	French
316	316	"Poids Moyen civelle"	French
317	317	"Tailles"	French
318	318	"Stades pigmentaires"	French
322	322	"Especes"	French
323	323	"Aide"	French
324	324	"Connexion"	French
325	325	"Utilisateur"	French
326	326	"Mot de passe"	French
327	327	"Login"	French
328	328	"Erreur"  title of the frame	French
329	329	"Probleme lors du test de la connexion ODBC" 	French
330	330	"Erreur dans l'utilisation de la methode connect de la classe ConnexionODBC" 	French
332	332	"Lien ODBC"	French
93	93	"L'objet Bilan est stocke dans l'environnement envir_stacomi, ecrire  ecrire bilanMigration=get('bilanMigration',envir_stacomi)"	French
88	88	"ecriture de tableau dans l'environnement envir_stacomi : ecrire periodeDF=get('periodeDF',envir_stacomi) "	French
208	208	"Ecriture de l'objet graphique dans l'environnement envir_stacomi : ecrire g=get('g',envir_stacomi) "	French
333	1	"You need to choose a counting device, clic on validate"	English
334	2	"You need to choose a taxa, clic on validate"	English
335	3	"You need to choose a stage, clic on validate"	English
336	4	"You need to choose a parameter, clic on validate"	English
337	5	"You need to choose the starting date"	English
338	6	"You need to choose the ending date"	English
339	7	"You need to choose a quantitative parameter"	English
340	8	"You need to choose a qualitative parameter"	English
341	9	"You need to choose a size class"	English
342	10	"You need to choose the starting year"	English
343	11	"You need to choose the ending year"	English
344	12	"You need to choose a crossing device, clic on validate"	English
346	14	"No information for this lots into the selected period"	English
349	17	"The query to load the coefficients of conversion is finished"	English
350	18	"lines founded for the coefficients of conversion"	English
351	19	"To obtain the table, type : bilan_poids_moyen=get('bilan_poids_moyen',envir_stacomi)@data"	English
352	20	"size is missing, lots"	English
353	21	"wet"	English
354	22	"dry"	English
355	23	"wet and dry"	English
356	24	"date"	English
357	25	"mean weights"	English
358	26	"Seasonnal trend of weights"	English
359	27	", beginningyear="	English
360	28	", endingyear="	English
361	29	"sinusoidal model, a.cos.2pi.(jour-T)/365)+b "	English
362	30	"To obtain the table, type : import_coe=get(import_coe",envir_stacomi)"	English
363	31	"data directory :"	English
364	32	"Do you want to write data in the database ?"	English
365	33	"attention"	English
366	34	"Gra"	English
367	35	"Coe"	English
368	36	"Leng"	English
369	37	"Reg"	English
370	38	"export"	English
371	39	"exit"	English
372	40	"Pigmentation stages loading query completed"	English
373	41	"Summary of pigmentation stages"	English
374	42	"SUMMARY OF PIGMENTATION STAGES"	English
375	43	"Pigmentation stages"	English
376	44	"and incoming dates in estuary"	English
377	45	"Title choice"	English
378	46	"You need to choose at least one quantitative or qualitative feature"	English
296	296	"Bilan des poids moyens en vue du calcul des relations poids effectif."	French
379	47	"Attention, this query might take a while, be patient ..."	English
380	48	"To get the table, type : bilan_taille=get('bilan_taille',envir_stacomi)"	English
381	49	"The query : requete=get('bilan_taille',envir_stacomi)@requete@sql "	English
382	50	"The data : donnees_taille=get('bilan_taille',envir_stacomi)@data"	English
383	51	"Data outcome from the query : don=get('bilan_taille',envir_stacomi)@requete@query"	English
384	52	"You need to launch computation first, clic on calc"	English
385	53	"Query completed"	English
386	54	"Environmental conditions loading query completed"	English
387	55	"You need to choose a monitoring station, clic on validate"	English
388	56	"Some monitoring stations lack associated values"	English
331	331	"Probleme lors du test, le lien ODBC fonctionne mais ne pointe pas vers la base version 0.5, verifiez le lien ODBC"	French
321	321	Annuels	French
345	13	"Sample characteristics have been loaded from the database"	English
347	15	"To obtain te table, type : bilan_lot=get('bilan_lot',envir_stacomi)"	English
320	320	Traitement taille-âge salmonidés	French
319	319	Anguilles argentées	French
389	57	"No environmental conditions values for selected monitoring stations (BilanConditionEnv.r)"	English
390	58	"Statistics :"	English
391	59	"Summary object is stocked into envir_stacomi environment : write bilanEspeces=get('bilanEspeces',envir_stacomi)"	English
392	60	"You need to launch computation first, clic on calc"	English
393	61	"Query failed for the view vue_ope_lot_car "	English
394	62	"You need to launch computation first, clic on calc"	English
395	63	"No fish in the database for this period"	English
396	64	"Attention, some negative counts are transformed into positive ones"	English
397	65	"Checking objects and launching query"	English
398	66	"Time steps loaded fot this counting device"	English
399	67	"No data for this counting device"	English
400	68	"month"	English
401	69	"time in hours"	English
402	70	"Working of the counting device"	English
403	71	matting ON 	English
404	72	matting OFF 	English
405	73	"Writing the table into envir_stacomi environment : write periodeDC=get('periodeDC',envir_stacomi)"	English
406	74	"Counting device"	English
407	75	c("Fonc","Arr","Fonc normal")	English
409	77	"Working of the counting device"	English
410	78	"Shutdowns types for this counting device"	English
411	79	"Writing of "	English
412	80	"this might take a while, please be patient …"	English
413	81	"Time steps of the fishway loaded"	English
414	82	"Shutdowns types for this counting device"	English
415	83	"No data for this fishway"	English
416	84	"computing ..."	English
417	85	"Progress %"	English
418	86	c("duree","type_fonct.","fonctionnement")	English
420	88	"Writing the table into envir_stacomi environment : write periodeDF=get('periodeDF',envir_stacomi)"	English
421	89	"Fishway"	English
422	90	"Shutdowns types for this fishway "	English
423	91	"Attention, no time step selected, compunting with default value"	English
424	92	"Starting migration summary ... be patient"	English
425	93	"Summary object is stocked into envir_stacomi environment : write bilanMigration=get(bilanMigration",envir_stacomi)"	English
427	95	"You need to launch computation first, clic on calc"	English
428	96	"Cumulative migration"	English
429	97	"Cumulative count"	English
430	98	"Attention, this function applies for annual summaries"	English
431	99	"Statistics about migration :"	English
432	100	"Summary object is stocked into envir_stacomi environment"	English
433	101	"You need to launch computation first, clic on calc"	English
434	102	"You don't have any environmental conditions within the time period"	English
435	103	"no selected station => simple graph"	English
436	104	"The number of lines of the environmental conditions table ("	English
437	105	") doesn't fit the duration of the migration summary  ("	English
438	106	"Attention, on one station :"	English
439	107	"there are several entries for the same day : "	English
440	108	"only the first value will be incuded in the summary"	English
441	109	"Attention, there is no migration summary for this year"	English
442	110	", this taxon and this stage (BilanMigrationInterAnnuelle.r)"	English
443	111	"Annual migrations query completed"	English
444	112	"Counts"	English
445	113	"Attention : you have to complete a migration summary for at least one of the selected year before launching a inter-annual summary"	English
446	114	"year"	English
447	115	"date"	English
448	116	"Annual migration percentage"	English
450	118	"Writing the graphical object into envir_stacomi environment : write g=get(gi",envir_stacomi) with "	English
451	119	"Starting migration summary with parameters, be patient …"	English
452	120	"You need to choose at least one quantitative or qualitative attribute"	English
453	121	"Be Attention, the processing doesnt take lot"s quantities into account"	English
454	122	"Writing data into envir_stacomi environment : write data=get(data",envir_stacomi)"	English
455	123	"You need to launch computation first, clic on calc"	English
456	124	"Writing the graphical object into envir_stacomi environment : write g=get(g",envir_stacomi)"	English
457	125	"Writing of"	English
458	126	"You need to define a baseODBC vector with the ODBC link, the user and the password"	English
459	127	"RODBC library is necessary, load the package !"	English
460	128	"Testing the connection, be Attention this class must only be used for testing :"	English
461	129	"Connection failed :"	English
462	130	"Connection established"	English
463	131	"Connection in progress"	English
464	132	"Time steps choice"	English
465	133	"Number of time steps choice"	English
466	134	"Internal error : no entry in time steps table"	English
467	135	"End date"	English
468	136	"Time step duration should be daily"	English
469	137	"Time step can't include more than one year"	English
470	138	"Time steps choice (1 year duration)"	English
471	139	"Start date"	English
472	140	"Time step"	English
473	141	"Number of days"	English
474	142	"End date"	English
475	143	"Time steps loaded"	English
476	144	"Year choice"	English
477	145	"Year selected"	English
478	146	"Problem when loading data or no data in the database (ODBC link ?)"	English
419	87	"Fishway operation"	English
408	76	c("Func.","Stop")	English
479	147	"Counting device selected"	English
480	148	"Select taxa for this counting device (for all periods for now)"	English
481	149	"Counting devices data"	English
482	150	"close"	English
483	151	"Counting devices choice"	English
484	152	"Table"	English
485	153	"Error : no counting device in the database (the query returns 0 entry)"	English
486	154	"Fishway selected"	English
487	155	"Fishways data"	English
488	156	"Fishway choice"	English
489	157	"No fishway in the database (the query returns 0 entry)"	English
490	158	"Loading parameters query completed"	English
491	159	"No data for selected device, taxon and stage"	English
492	160	"Feature has been selected"	English
493	161	"Internal error : unable to load any feature to make the choice"	English
494	162	"Internal error : there must have one line in Refparqual@data, or nbligne="	English
495	163	0	English
496	164	0	English
497	165	0	English
498	166	No data for this counting device and this taxon	English
499	167	Stage selected	English
500	168	Qualitative feature	English
501	169	Quantitative feature	English
502	170	Stop internal error : load data to make a choice	English
503	171	Stage selection	English
504	172	Select at least one value	English
505	173	The monitoring stations have been selected	English
506	174	Monitoring stations selection	English
507	175	Stop : no data for selected monitoring station (problem with the ODBC link ?)	English
508	176	Taxon selected	English
509	177	Taxon selection	English
510	178	Stop there is no line in the taxons table (problem with the ODBC link ?)	English
511	179	Error ODBC => you must define a vector baseODBC with an ODBC link, a user and a password	English
512	180	Testing connection	English
513	181	Connection failed	English
514	182	Connection successful	English
515	183	Testing query	English
516	184	Query successful	English
517	185	A summary has already been written in the database the :	English
518	186	Overwrite ?	English
519	187	Writing daily balance sheet in the database	English
520	188	Progression %	English
521	189	Writing daily summary in the database	English
522	190	Writing monthly summary in the database	English
523	191	Glass eels graph	English
524	192	Number of glass eels (x1000)	English
525	193	Estimated numbers	English
526	194	c("weight of the daily number","daily number counted")	English
527	195	number of operations =	English
528	196	average trapping time = 	English
529	197	maximum term =	English
530	198	minimum term =	English
531	199	c("work","stop","work normaly")	English
532	200	c("work","stop")	English
533	201	Fishway	English
536	204	Month	English
537	205	Number (x1000)	English
538	206	c("weight of monthly number","monthly number counted")	English
539	207	Number	English
540	208	Writing of the graphical object in the environment envir_stacomi : write g=get(g",envir_stacomi)"	English
542	210	Number	English
543	211	Date	English
544	212	estimated number,	English
546	214	Sum of numbers =	English
547	215	c("Numbers","type","period","month","fortnight","week","day_365")	English
548	216	Calculation of the monthly balance sheet	English
549	217		English
550	218	writing of	English
551	219	Conversion weight / number	English
552	220	Be Attention sum=0, you didn't enter the coefficient of conversion	English
553	221	Loading of the view vue_ope_lot, and choice of the counting device and of the time steps	English
554	222	LOTS SUMMARY	English
555	223	Start of timestamp	English
556	224	End of timestamp	English
557	225	Beginning date has been chosen	English
558	226	Ending date has been chosen	English
560	228	Table	English
561	229	Exit	English
567	235		English
568	236		English
569	237	crossed query length / qualitative feature	English
570	238	Loading of the monitoring stations	English
564	232	"table"	English
563	231	"load"	English
565	233	"exit"	English
571	239	summary graphic	English
572	240	Summary tables in .csv	English
573	241	Exit	English
574	242	Summary of encountered species for the counting device	English
575	243	Species summary	English
576	244	Loading	English
577	245	Pie chart graphic	English
578	246	Histogram	English
579	247	Summary tables in .csv and XML	English
580	248	Choice of cutting	English
581	249	Loading of the lists for taxons, stages and counting devices	English
582	250	Calculation of numbers by time step	English
583	251	Balance graphic	English
584	252	Cumulative graphic	English
585	253	Balance sheet in .csv	English
586	254	Calculation of environnemental conditions by time step	English
587	255	Loading of the lists for taxons, stages, counting devices and monitoring stations	English
535	203	Op	English
534	202	CD	English
588	256	Loading of the existing daily summaries	English
589	257	The year of beginning has been chosen	English
590	258	Beginning year	English
591	259	The year of end has been chosen	English
592	260	Ending year	English
593	261	Migration of all the years in the same graphic	English
594	262	cumulated migrations %	English
595	263	Table	English
596	264	Exit	English
597	265	day	English
598	266	Daily migration	English
599	267	week	English
600	268	weekly migration	English
601	269	fortnight	English
602	270	Fortnight Migration	English
603	271	month	English
604	272	Monthly migration	English
605	273	Loading of the lists for taxons, stages, counting devices, qualitative and quantitative parameters	English
606	274	Choice of batch type, inclusion of samples ?	English
607	275	Calculation of numbers by time step	English
608	276	Monthly graphic	English
609	277	Daily graphic	English
610	278	Summary in .csv	English
611	279	Exit	English
612	280	Loading of the list for fishways and choice of the time step	English
613	281	Mensual graphic	English
614	282	Boxplot	English
615	283	Table	English
616	284	Exit	English
617	285	Calculation of the operating fishway	English
618	286	Calculation of the operating counting device	English
619	287	Summary of the operations of a device ... to do	English
620	288	Summary between the operating fishway and the counting device … to do	English
621	289	Migration summary (for a species and a stage)	English
622	290	Summary of interannual migration	English
623	291	Summary of migration environnemental conditions	English
624	292	Summary of migration with parameters	English
625	293	Summary of the environnemental conditions	English
626	294	Summary of batch by calling the application vue lot ope	English
627	295	Lengths summary	English
629	297	Calculation of the pigmentary stages	English
630	298	Paste (For help, contact Cédric Briand - 0033 29 99 08 844 - cedric.briand@lavilaine.com")"	English
632	300	Migratory treatment	English
633	301	TODO to develop	English
634	302	Output of the program	English
635	303	Species summary of the counting device	English
636	304	Station	English
637	305	Fishway	English
638	306	Counting Device	English
639	307	Operation (TODO)	English
640	308	Fishway without counting device (TODO)	English
641	309	Summary	English
642	310	Migration	English
643	311	Environnemental conditions	English
644	312	Migration. ~Environnemental conditions	English
645	313	Migration / quant. param. / qual. param.	English
646	314	Inter annual migration	English
647	315		English
648	316	Average weight glass eel	English
649	317	Lenghts	English
650	318	Pigmentary stages	English
654	322	Species	English
655	323	Help	English
656	324	Connection	English
657	325	User	English
658	326	Password	English
659	327	Login	English
660	328	"Error" title of the frame	English
661	329	Problem when testing the ODBC connection	English
662	330	Error when using the method connect of the ConnectionODBC class	English
664	332	ODBC link	English
665	1	"Elige un dispositivo de cuenta, haga click en validar"	Spanish
666	2	"Elige un taxon, haga click en validar"	Spanish
667	3	"Elige un estadio, haga click en validar"	Spanish
668	4	"Elige la fecha de inicio"	Spanish
669	5	"Elige la fecha de inicio"	Spanish
670	6	"Elige la fecha final"	Spanish
671	7	"Elige un parámetro cuantitativo"	Spanish
672	8	"Elige un parámetro cualitativo"	Spanish
673	9	"Elige una categoria de cantidad"	Spanish
674	10	"Elige el año de inicio"	Spanish
675	11	"Elige el año final"	Spanish
676	12	"Elige un disposotivo de paso, haga click en validar"	Spanish
677	13	"Se ha efectuado la  petición para cargar las características del lote (grupo)"	Spanish
678	14	"No hay ningún dato de estos lotes en el período seleccionado"	Spanish
681	17	"Se ha efectuado la petición para cargar los coeficientes de conversión"	Spanish
682	18	"Filas encontradas para los coeficientes de conversión"	Spanish
683	19	"Para recuperar la tabla, escribir: bilan_poids_moyen=get('bilan_poids_moyen',envir_stacomi)@data"	Spanish
684	20	"cantidad no conocida, lotes"	Spanish
685	21	"Húmedos"	Spanish
686	22	"Secos"	Spanish
687	23	"Húmedos y secos"	Spanish
688	24	"Fecha"	Spanish
689	25	"Pesos medios"	Spanish
690	26	"Tendencia estacional de los pesos"	Spanish
691	27	",añoinicio="	Spanish
692	28	",añofin="	Spanish
693	29	"modelo sinusoidal,a.cos.2pi.(jour-T)/365)+b "	Spanish
694	30	"Para recuperar la tabla, escribir: import_coe=get('import_coe',envir_stacomi)"	Spanish
663	331	""Problem during the test, the ODBC link works but doesn't point to the database 0.5, check the ODBC link""	English
679	15	"Para recuperar la tabla, escribir: bilan_lot=get('bilan_lot',envir_stacomi)"	Spanish
653	321	Yearly	English
651	319	Silver eel	English
695	31	"Directorio de escritura de los datos"	Spanish
696	32	"¿Quiere escribir los datos en la base?"	Spanish
697	33	"atención"	Spanish
698	34	"Gra"	Spanish
699	35	"Coe"	Spanish
700	36	"Tail"	Spanish
701	37	"Reg"	Spanish
702	38	"exportar"	Spanish
703	39	"salir"	Spanish
704	40	"Se ha efectuado la petición para cargar los estadíos pigmentarios"	Spanish
705	41	" Conclusión de los estadíos pigmentarios"	Spanish
706	42	"CONCLUSIÓN DE ESTADIOS PIGMENTARIOS"	Spanish
707	43	"Estadíos pigmentarios"	Spanish
708	44	"y fechas de llegada al estuario"	Spanish
709	45	"Elegir el  título"	Spanish
710	46	"Elige al menos una característica cuantitativa o cualitativa"	Spanish
711	47	"Atención esta petición (cruce de dos vistas) puede necesitar unos minutos, sea paciente"	Spanish
712	48	"Para recuperar la tabla, escribir: bilan_taille=get('bilan_taille',envir_stacomi)"	Spanish
713	49	"La peteción:  requete=get('bilan_taille',envir_stacomi)@requete@sql "	Spanish
714	50	"Los datos: donnees_taille=get('bilan_taille',envir_stacomi)@data"	Spanish
715	51	"Datos resultantes de la petición: don=get('bilan_taille',envir_stacomi)@requete@query"	Spanish
716	52	"Tienes que hacer los cálculos en primer lugar,hacer click en calcular"	Spanish
717	53	"Petición efectuada"	Spanish
718	54	"Se ha efectuado la petición para cargar las condicones ambientales"	Spanish
719	55	"Elige una estación de medida, después haz click sobre validar"	Spanish
720	56	"Algunas estaciones de medida no tienen valores asociados"	Spanish
721	57	"Las estaciónes de medidas selecionadas no tienen valores de condiciones ambientales (BilanConditionEnv.r)"	Spanish
722	58	"Estadísticas:"	Spanish
723	59	"El objeto Conclusión está almacenado en el entorno envir_stacomi, escribir bilanEspeces=get('bilanEspeces',envir_stacomi), "	Spanish
724	60	"Se deben de realizar los cálculos antes, haga click sobre calc"	Spanish
725	61	"Error en la petición sobre la vista vue_ope_lot_car "	Spanish
726	62	"Debemos lanzar los cálulos antes de hacer click sobre calc"	Spanish
727	63	"No hay ningún pez en la base de datos para este periodo "	Spanish
728	64	"Atención algunos cantidades negativas son transformados en positivas"	Spanish
729	65	"Verificación de los objetos y lanzamiento de la petición"	Spanish
730	66	"Se ha efectuado la petición para cargar los pasos de los tiempos de DC"	Spanish
731	67	"Se ha efectuado la petición para cargar los pasos de los tiempos de DC"	Spanish
732	68	"mes"	Spanish
733	69	"tiempo en horas"	Spanish
734	70	"Funcionamiento del dispositivo de recuento"	Spanish
735	71	"ver video"	Spanish
736	72	"parar video"	Spanish
737	73	"Escritura de tabla en el entorno  envir_stacomi : escribir periodeDC=get('periodeDC',envir_stacomi) "	Spanish
738	74	"DC"	Spanish
739	75	c("Fonc","Arr","Fonc normal")	Spanish
740	76	c("Fonc","Arr")	Spanish
741	77	"Funcionamiento DC"	Spanish
742	78	"Tipos de paradas de DC"	Spanish
743	79	"Escritura de"	Spanish
744	80	"puede tardar un poco, sea paciente..."	Spanish
745	81	"Se efectua la petición para cargar los pasos de los tiempos de DF"	Spanish
746	82	"No hay datos en este DF"	Spanish
747	83	"Construcción de gráfica, espere"	Spanish
748	84	"cálculo..."	Spanish
749	85	"progresión% "	Spanish
750	86	c("duree","type_fonct.","fonctionnement")	Spanish
751	87	"Funcionamiento DF"	Spanish
752	88	"escritura de tablas con el entorno envir_stacomi : escribir periodeDF=get('periodeDF',envir_stacomi) "	Spanish
753	89	"DF"	Spanish
754	90	"Tipos de paradas de DF"	Spanish
755	91	"Atención la elección de periodo no ha sido efectuada, cálculo con el valor por defecto"	Spanish
756	92	"Comienzo del Informe de migración… espere"	Spanish
757	93	"El objeto Conclusión está almacenado en el entorno envir_stacomi, escribir  ecrire bilanMigration=get('bilanMigration',envir_stacomi)"	Spanish
759	95	"Se deben de realizar los cálculos antes, haga click sobre calc"	Spanish
760	96	"Migración acumulada"	Spanish
761	97	"Cantidad acumulada"	Spanish
762	98	"Atención esta función es para los Conclusiones anuales"	Spanish
763	99	"Estadisticas concernientes a la migración"	Spanish
764	100	"El objeto Conclusión está almacenado por el entorno  envir_stacomi"	Spanish
765	101	"Hay que realizar los cálculos antes, haga click sobre calc"	Spanish
766	102	"No tiene condiciones medio ambientales en el periodo de tiempo"	Spanish
767	103	"no se ha seleccionado estación => gráfico simple"	Spanish
768	104	"el número de líneas de la tabla de condiciones ambientales ("	Spanish
769	105	")no corresponde a la duración de la Conclusión de la migración"	Spanish
770	106	"Atención: en una de las estaciones:"	Spanish
771	107	"Hay varios registros para el mismo día"	Spanish
772	108	"solo el primer valor será integrado en el informe"	Spanish
773	109	"Atención no existe Conclusión de migración para este año"	Spanish
774	110	",este taxón y este estadío (BilanMigrationInterAnnuelle.r)" 	Spanish
775	111	"Se ha efectuado la petición para cargar las migraciones de los años""	Spanish
776	112	"Cantidad"	Spanish
777	113	"ATENCIÓN:  extraiga una conclusión de Migración para al menos uno de los años seleccionados antes de lanzar un balance interanual" 	Spanish
778	114	"año"	Spanish
779	115	"fecha"	Spanish
780	116	"Porcentaje de la migración anual"	Spanish
782	118	"Escribiendo el objeto gráfico en el entorno envir_stacomi : escribir g=get("gi",envir_stacomi) avec "	Spanish
783	119	"Inicio del Informe de migración con parámetros… espere"	Spanish
784	120	"Tienes que elegir al menos una característica cuantitativa o cualitativa"	Spanish
785	121	"Atención, este tratamiento no ha sido efectuado sobre las cantidades de los lotes"	Spanish
786	122	"Escritura de los datos en el entorno envir_stacomi : escribir data=get('data',envir_stacomi) "	Spanish
787	123	"Tiene que realizar los cálculos antes"	Spanish
788	124	"Escribir el objeto gráfico en el entorno envir_stacomi : escribir g=get('g',envir_stacomi) "	Spanish
789	125	"Escritura de"	Spanish
790	126	"Se debe definir un vector baseODBC con el enlace ODBC, usuario y contraseña"	Spanish
791	127	"La biblioteca RODBC es necesaria, cargue el paquete!"	Spanish
792	128	"Conexión de prueba, esta clase solo debe de ser utilizada para las pruebas:"	Spanish
793	129	"No se puede conectar:"	Spanish
794	130	"Conexión establecida"	Spanish
795	131	"Conexión en curso"	Spanish
796	132	"Elección sin tiempo"	Spanish
797	133	"Seleccionar el número de tiempos"	Spanish
798	134	"Error interno: la tabla de paso del tiempo no contiene ninguna fila de tiempo"	Spanish
799	135	"Fecha de finalización"	Spanish
800	136	"La  duración del paso debe de ser diaria"	Spanish
801	137	"el paso del tiempo no debe comprender varios años"	Spanish
802	138	"Elección del periodo (duración 1 año)"	Spanish
803	139	"Fecha de inicio"	Spanish
804	140	"Paso del tiempo"	Spanish
805	141	"día Nb"	Spanish
806	142	"Fecha de finalización"	Spanish
807	143	" El paso  del tiempo ha sido cargado"	Spanish
808	144	"Selección del año"	Spanish
809	145	"Año seleccionado"	Spanish
810	146	"problema durante la carga de datos o no hay datos en la base (enlace ODBC?)"	Spanish
811	147	"El DC ha sido seleccionado"	Spanish
812	148	"selección de los taxones de DC (por el momento de todos los períodos)"	Spanish
813	149	"Datos en los Dispositivos de Contaje"	Spanish
814	150	"cerrar"	Spanish
815	151	"Selección de Dispositivo de Contaje"	Spanish
816	152	"Tabla"	Spanish
817	153	"Error: ningún DC se ejecuta en la base de datos (no hay ninguna línea de retorno de la solicitud)	Spanish
818	154	"El DF ha sido seleccionado"	Spanish
819	155	"Datos de los Dispositivos de Franqueo"	Spanish
820	156	"Elección del Dispositivo de Franqueo"	Spanish
821	157	"Ningún DF se ejecuta en la base  (no hay ninguna línea de retorno de la solicitud)"	Spanish
822	158	"La consulta se ha realizado para cargar los parámetros"	Spanish
823	159	"No hay datos para este DC, este taxón y esta etapa"	Spanish
824	160	"La característica ha sido seleccionada"	Spanish
825	161	"error interno, ninguna característica ha podido ser seleccionada para realizar la elección"	Spanish
826	162	"error interno, no puede haber una fila en Refparqual@data, or nbligne="	Spanish
827	163	0	Spanish
828	164	0	Spanish
829	165	0	Spanish
830	166	"No hay datos para este DC, y este taxón"	Spanish
831	167	"El estadío ha sido seleccionado"	Spanish
832	168	"Característica cualitativa"	Spanish
833	169	"Característica cuantitativa"	Spanish
834	170	"stop error interno: cargar los datos para hacer la elección"	Spanish
835	171	"Elección del estadío"	Spanish
836	172	"seleccione al menos un valor"	Spanish
837	173	"La estaciones de muestreo han sido seleccionadas"	Spanish
838	174	"Elección de estaciones de muestreo"	Spanish
839	175	"Stop no hay ningún dato de estación de muestreo (pb enlace ODBC?)"	Spanish
840	176	"El taxón ha sido seleccionado"	Spanish
841	177	"Selección de Taxón"	Spanish
842	178	"Stop no hay ninguna fila en la tabla de taxones (pb enlace ODBC?)"	Spanish
843	179	"Error ODBC=>hay que definir un vector base ODBC con el enlace ODBC, el usuario y la contraseña"	Spanish
844	180	"Prueba de conexión:"	Spanish
845	181	"Conexión imposible:"	Spanish
846	182	"Conectado con éxito"	Spanish
847	183	"Prueba de peticion"	Spanish
848	184	"Petición exitosa"	Spanish
849	185	"Una Conclusión ha sido escrita ya en la base:"	Spanish
850	186	"¿desea reemplazarlo?"	Spanish
958	294	"Conclusión lotes llamando a la vista vue lot ope"	Spanish
851	187	"escritura de la conclusión diaria en la base de datos"	Spanish
852	188	"% progresión"	Spanish
853	189	"escritura de la conclusión diaria en la base de datos"	Spanish
854	190	"escritura de la conclusión mensual en la base de datos"	Spanish
855	191	"gráfica de la angula:"	Spanish
856	192	"Cantidad angulas (x1000)"	Spanish
857	193	"Cantidad estimada,"	Spanish
858	194	c("eff. journ. poids","eff. journ. Compt.")	Spanish
859	195	"número de operaciones="	Spanish
971	307	"Operación (TODO)"	Spanish
860	196	"duración media de la captura="	Spanish
861	197	"duración max="	Spanish
862	198	"duración min="	Spanish
863	199	c("Fonc","Arr","Fonc normal")	Spanish
864	200	c("Fonc","Arr")	Spanish
865	201	"DF"	Spanish
866	202	"DC"	Spanish
867	203	"OP"	Spanish
868	204	"Mes"	Spanish
869	205	"Cantidad (x1000)	Spanish
870	206	c("eff. mens. poids","eff. mens. Compt.")	Spanish
871	207	"Cantidad"	Spanish
872	208	"Escritura el objeto gráfico en el entorno envir_stacomi : escribir g=get("g",envir_stacomi) "	Spanish
873	209	"ATENCIÓN, hay cantidad de entradas de lotes de un taxón diferente a angulas, comprobar"	Spanish
874	210	"cantidad"	Spanish
875	211	"fecha"	Spanish
876	212	"Cantidad estimada"	Spanish
878	214	"suma cantidades:"	Spanish
879	215	c("Effectifs","type","duree","mois","quinzaine","semaine","jour_365")	Spanish
880	216	"calculo de las conclusiones mensuales"	Spanish
881	217		Spanish
882	218	"escritura de"	Spanish
883	219	"Conversión de la cantidad de peso"	Spanish
884	220	"Atención suma==0, no ha introducido el coeficiente de conversión"	Spanish
885	221	"carga de la vista (vue_ope_lot) y elección del dc y del periodo de tiempo"	Spanish
886	222	"CONCLUSIÓN LOTES"	Spanish
887	223	"fecha y hora de inicio"	Spanish
888	224	"fecha y hora de fin"	Spanish
889	225	"La fecha de inicio ha sido elegida"	Spanish
890	226	"La fecha de fin ha sido elegida"	Spanish
892	228	"Tabla"	Spanish
893	229	"Salir"	Spanish
899	235		Spanish
900	236		Spanish
901	237	"Petición cruzada talla/caract cualitativa"	Spanish
902	238	"Carga de las estaciones de muestreo"	Spanish
903	239	"gráfica conclusiones"	Spanish
904	240	"tabla conclusiones en .csv"	Spanish
905	241	"Salir"	Spanish
906	242	"Conclusión de las especies presentes en el DC"	Spanish
907	243	"Conclusión de las especies"	Spanish
908	244	"Cargando"	Spanish
909	245	"Gráfico de PieChart"	Spanish
910	246	"Histograma"	Spanish
911	247	"Tablas de Conclusión en .csv y XML"	Spanish
912	248	"Elección de recorte"	Spanish
913	249	"Carga de listas de taxones, estadíos y dc"	Spanish
914	250	"Cálculo de la cantidad por periodos"	Spanish
915	251	"Gráfica conclusiones"	Spanish
916	252	"Gráfico de acumulación"	Spanish
917	253	"Tabla Conclusiones en .csv"	Spanish
918	254	"cálculo de las condiciones ambientales por  periodos"	Spanish
919	255	"Carga de los taxones y los estadíos y estaciones dc y de muestreo"	Spanish
920	256	"carga de conclusionesDiarias existentes"	Spanish
921	257	"El año de inicio ha sido elegido"	Spanish
922	258	"Año de inicio"	Spanish
923	259	"El año final ha sido elegido"	Spanish
924	260	"Año final"	Spanish
925	261	"Migración de todos los años en el mismo gráfico"	Spanish
926	262	"Acumulación de migradores en %"	Spanish
927	263	"Tabla"	Spanish
928	264	"Salir"	Spanish
929	265	"día"	Spanish
930	266	"Migración diaria"	Spanish
931	267	"sem"	Spanish
932	268	"Migración semanal"	Spanish
933	269	"quin"	Spanish
934	270	"Migración quincenal"	Spanish
935	271	"mes"	Spanish
936	272	"Migración mensual"	Spanish
937	273	"Carga de listas de taxones, estadíos, DC, parámetros cualitativos y cuantitativos"	Spanish
938	274	"Elegir tipo de lote, incluir las muestras?"	Spanish
939	275	"Cálculo del número por periodo"	Spanish
940	276	"gráfica mensual"	Spanish
941	277	"gráfica diaria"	Spanish
942	278	"tabla informe en .csv"	Spanish
943	279	"Salir"	Spanish
944	280	"Carga de listas dc y elección de periodo"	Spanish
945	281	"gráfica mensual"	Spanish
946	282	"Diagrama de barras"	Spanish
947	283	"Tabla"	Spanish
948	284	"Salir"	Spanish
949	285	"Cálculo del funcionamiento de dc"	Spanish
950	286	"Cálculo del funcionamiento de df"	Spanish
951	287	"Conclusión de las operaciones de un dispositivo… a desarrollar"	Spanish
952	288	"Conclusión de cruces de funcionamiento de DF y DC, a desarrollar"	Spanish
953	289	"Conclusión migración (para una especie y un estadío)	Spanish
954	290	"Conclusión migración interanual"	Spanish
955	291	"Conclusión migración condiciones ambientales"	Spanish
956	292	"Conclusión migración con los parámetros"	Spanish
957	293	"Conclusión de condiciones ambientales"	Spanish
896	232	"table"	Spanish
895	231	"load"	Spanish
897	233	"salir"	Spanish
959	295	"Conclusión tallas"	Spanish
961	297	"Calcular estadíos pigmentarios"	Spanish
962	298	pegar ("para ayuda cedric Briand - 02 99 90 88 44 - cedric.briand@lavilaine.com"	Spanish
964	300	"Tratamiento migratorio"	Spanish
965	301	"TODO a desarrollar"	Spanish
966	302	"salidas del programa"	Spanish
967	303	"conclusión de especies de DC"	Spanish
968	304	"Estación"	Spanish
969	305	"DF"	Spanish
970	306	"DC"	Spanish
972	308	"DF sin DC (TODO)"	Spanish
973	309	"Conclusión"	Spanish
974	310	"Migración"	Spanish
975	311	"Cond. Env."	Spanish
976	312	"Migr.~Cond. Env."	Spanish
977	313	"Migr./ parm. quant / parm. qual"	Spanish
978	314	"Migr. interanual"	Spanish
979	315	"Parm. de lot"	Spanish
980	316	"Pesos Medios angula"	Spanish
981	317	"Tallas"	Spanish
982	318	"Estadíos pigmentarios"	Spanish
983	319	Anguilas plateadas	Spanish
986	322	"Especies"	Spanish
987	323	"Ayuda"	Spanish
988	324	"Conexión"	Spanish
989	325	"Usuario"	Spanish
990	326	"Contraseña"	Spanish
991	327	"Registro"	Spanish
992	328	"Error" título del marco	Spanish
993	329	"Problema al comprobar la conexión ODBC"	Spanish
994	330	"Error en el uso del método de conexión de la clase ConexiónODBC"	Spanish
996	332	"Enlace ODBC"	Spanish
997	333	"There are no values for the taxa, stage and selected period"	English
998	333	"Il n'y a pas d'effectif pour le taxon, le stade, et la période sélectionnée"	French
999	333	"please translate this : Il n'y a pas d'effectif pour le taxon, le stade, et la période sélectionnée"	Spanish
1000	334	"Ecriture du bilanMigrationInterannuelle dans l'environnement envir_stacomi : ecrire bmi=get('bilanMigrationInterannuelle',envir_stacomi) "	French
1001	334	"please translate this : Writing bilanMigrationInterannuelle in the environment envir_stacomi : write bmi=get('bilanMigrationInterannuelle',envir_stacomi) "	Spanish
1002	334	"Writing bilanMigrationInterannuelle in the environment envir_stacomi : write bmi=get('bilanMigrationInterannuelle',envir_stacomi) "	English
1003	336	Jours	French
1004	336	Days	English
1005	336	Dias	Spanish
1006	342	"Bilan migration pour plusieurs DC, taxons, ou stades"	French
1007	342	"Daily migration for several DC, species, or stage"	English
1008	342	"La migración al día durante varios DC, especie, o etapa de la vida"	Spanish
562	230	"Mean weight report"	English
894	230	"Informes de pesos moyen "	Spanish
628	296	"Summary of average weight for the calculation of the relation between length and number."	English
960	296	"conclusión de pesos medios para calcular la relaciones de las cantidades de pesos"	Spanish
566	234	"choice of number in sample (one, several,all)"	English
898	234	"elección de número en la muestra (uno, varios o todos)"	Spanish
1010	343	Attention vous n'avez probablement pas rentre tous les coefficients dans la base, vérifier car les poids depuis effectif ou effectifs depuis poids seront faux	French
1011	343	Attention there are probably missing coefficients in the database, verify or the conversion from number to weights and weights to number might be wrong	English
1012	343	La atención es probable que haya perdido los coeficientes de la base de datos, verificar o la conversión de número a los pesos y pesos con el número podría estar equivocado	Spanish
117	117	", Effectifs cumulés"	French
449	117	", Cumulated numbers"	English
781	117	", Acumulado real"	Spanish
559	227	"dotplot"	English
891	227	"dotplot"	Spanish
1013	344	"boxplot"	French
1014	344	"boxplot"	English
1015	344	"boxplot"	Spanish
1016	345	"densité"	French
1017	345	"density"	English
1018	345	"density"	Spanish
227	227	"dotplot"	French
1019	346	"Migration multiple"	French
1020	346	"Migration multiple"	English
1021	346	"Migración multiple"	Spanish
985	321	Anual	Spanish
209	209	"Attention, il y a des quantite de lots rentrees pour un taxon autre que civelles, verifier"	French
1022	347	"Le(s) DC(s) a(ont) ete selectionne(s) "	French
1023	347	"The DC(s) have been selected"	English
1024	347	"El(los) DC(s) ha sido seleccionado"	Spanish
1025	348	"Le(s) Taxons(s) a(ont) ete selectionne(s) "	French
1026	348	"The taxa(s) have been selected"	English
1027	348	"El(los) taxón(s) ha sido seleccionado"	Spanish
1028	349	"Pas de valeur pour l'argument taxon"	French
1029	349	"No value for argument taxon"	English
1030	349	"No value for argument taxon"	Spanish
1031	350	"Taxons non présents :"	French
1032	350	"Taxa not present :"	English
1033	350	"Taxa not present :"	Spanish
1034	351	"Pas de valeur pour l'argument stades"	French
1035	351	"No value for argument stage"	English
1036	351	"No value for argument stage"	Spanish
1037	352	"Stades non présents :"	French
1038	352	"Stage not present :"	English
1039	352	"Stage not present"	Spanish
541	209	"Attention, there are batch quantities entered for another stage than glass eel, please check"	English
213	213	c("mesure","calcule","expert","ponctuel")	French
545	213	c("measured","calculated","expert","direct")	English
877	213	c("mesure","calcule","expert","ponctuel")	Spanish
298	298	"Pour de l'aide cedric Briand - 02 99 90 88 44 - cedric.briand@eptb-vilaine.fr - https://groups.google.com/forum/?hl=fr#!forum/stacomi"	French
94	94	"Pour accéder aux données calculées, tappez bilanMigration@calcdata"	French
426	94	"To access calculated data, type bilanMigration@calcdata"	English
995	331	"Problemas durante la prueba, la conexión ODBC funciona, pero no señala a la base de la versión 0.5, verificar el enlace ODBC")	Spanish
16	16	"Pour recuperer l'objet graphique, tapper : g<-get("g",envir_stacomi), voir http://trac.eptb-vilaine.fr:8066/tracstacomi/wiki/Recette%20BilanLot pour de l'aide"	French
348	16	"To obtain the graphical object, type :  g<-get("g",envir_stacomi), see http://trac.eptb-vilaine.fr:8066/tracstacomi/wiki/Recette%20BilanLot for help"""	English
984	320	Tratamiento salmónidos tamaño - edad	Spanish
652	320	Treatment size - age salmonids	English
1062	353	"Code"	French
1063	353	"Code"	English
1064	353	"Code"	Spanish
1065	354	"Il faut selectionner les DC, les taxons et les stades pour avoir une commande complète"	French
1066	354	"Please select DC, taxa, and stages for a complete command"	English
1067	354	"Please select DC, taxa, and stages for a complete command"	Spanish
1068	355	"L'objet Bilan est stocke dans l'environnement envir_stacomi, ecrire bilanMigrationMult=get('bilanMigrationMult',envir_stacomi)"	French
1069	355	"The summary object is stored in environment envir_stacomi, write bilanMigrationMult=get('bilanMigrationMult',envir_stacomi)"	English
1070	355	"The summary object is stored in environment envir_stacomi, write bilanMigrationMult=get('bilanMigrationMult',envir_stacomi)"	Spanish
1071	356	"Les données brutes sont stockées dans bilanMigrationMult@data, les resultats sont stockés dans bilanMigrationMult@calcdata"	French
1072	356	"Raw data are stored in bilanMigrationMult@data, processed data in bilanMigrationMult@calcdata"	English
1073	356	"Raw data are stored in bilanMigrationMult@data, processed data in bilanMigrationMult@calcdata"	Spanish
758	94	"Para acceder a los datos calculado , tipo bilanMigration @ calcdata")	Spanish
1074	357	c("Fct. normal","Arr. ponctuels","Arrêts","Dysfonct.","Inconnu")	French
1075	357	c("Normal operation","Operational stop","Stop","Dysfunct.","Unknown")	English
1076	357	c("Funcio. normal","parada. operativa","Stop","Disfunción.","Desconocido")	Spanish
1077	358	"Durée en jours (types de fonctionnement):"	French
1078	358	"Duration in days (operation type):"	English
1079	358	"Duración en días (tipo de operación):"	Spanish
1080	359	"Durée en jours (fonctionnement):"	French
1081	359	"Duration in days (operation):"	English
1082	359	"Duración en días (operación):"	Spanish
1083	360	"Chargement des données des opérations:"	French
1084	360	"Loading data for operations"	English
1085	360	"La carga de datos para las operaciones:"	Spanish
15	15	"Pour recuperer le tableau, tapper : bilan_lot=get('bilan_lot',envir_stacomi)"	French
680	16	"Para recuperar el objeto gráfico, escribir: g<-get("g",envir_stacomi), see http://trac.eptb-vilaine.fr:8066/tracstacomi/wiki/Recette%20BilanLot for help"	Spanish
1086	361	"L'objet BilanAnnuels est stocké dans l'environnement stacomi, tappez bilA<-get("bilanAnnuels",envir_stacomi)"	French
1087	361	"The object BilanAnnuels is stored in the stacomi environment, type bilA <-get("bilanAnnuels",envir_stacomi)"	English
1088	361	"El objeto BilanAnnuels se almacena en el entorno stacomi, el tipo  bilA<-get("bilanAnnuels",envir_stacomi)"	Spanish
1089	362	"Les données ont été chargées"	French
1090	362	"Data loaded"	English
1091	362	"Los datos se han cargado"	Spanish
1092	363	"Pas de données d'argentées ou d'anguilles jaunes sur la période demandée"	French
1093	363	"No data of silver or yellow eel on the selected period"	English
1094	363	"No hay datos de plata o anguila amarilla en el período seleccionado"	Spanish
\.


--
-- Name: ts_messagerlang_mrl_mrl_id_seq; Type: SEQUENCE SET; Schema: ref; Owner: postgres
--

SELECT pg_catalog.setval('ts_messagerlang_mrl_mrl_id_seq', 1094, true);


--
-- Data for Name: ts_nomenclature_nom; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY ts_nomenclature_nom (nom_id, nom_nomtable, nom_nomenclaturesandreid, nom_datemiseajour, nom_commentaire) FROM stdin;
1	ref.tr_natureouvrage_nov	284	2016-06-14	En plus du référentiel des obstacles à l'écoulement, la notion de barrage pour la migration est retenue
2	ref.tr_typedf_tdf	571	2016-06-14	Ajout d'un type (barrière, filet, barrière électrique) qui ne correspond pas au référentiel, mais est utile pour la description des stations (ex : Cerisel)
3	ref.tr_typedc_tdc	131	2016-06-14	Référentiel du SANDRE a mettre à jour, modification des définitions. Ajout de types.
4	ref.tr_pathologie_pat	129	2016-06-11	La pathologie 51 pas dans le Sandre a du sens, je la garde
\.


--
-- Name: ts_nomenclature_nom_nom_id_seq; Type: SEQUENCE SET; Schema: ref; Owner: postgres
--

SELECT pg_catalog.setval('ts_nomenclature_nom_nom_id_seq', 4, true);


--
-- Data for Name: ts_organisme_org; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY ts_organisme_org (org_code, org_description) FROM stdin;
IAV	EPTB Vilaine
LOGRAMI	Association pour la gestion et la restauration des poissons migrateurs du bassin de la Loire
MIGRADOUR	L’association MIGRADOUR a été créée en 1994 à l’initiative des 4 fédérations départementales de pêche du bassin de l’Adour et du Conseil Supérieur de la Pêche.
INRA	Institut National de la Recherche Agronomique
SAUMONRHIN	Association Saumon Rhin
MIGADO	Association Migrateurs Garonne Dordogne
BGM	Bretagne Grands Migrateurs
BRESLE	Suivi des poissons migrateurs sur la Bresle par l'ONEMA
SEINORMIGR	Association interrégionale pour la gestion des poissons migrateurs sur les bassins de la Seine et des cours d’eau Nord/Seine
invite	Compte invité pour la visualisation des données et la connexion initale
MRM	Association Migrateurs Rhone Méditerrannée
CHARENTE	Cellule d`animation poissons  migrateurs du bassin de la Charente contact : Audrey Postic Puivif audrey.postic-puivif@fleuve-charente.net
SMATAH	Syndicat mixte d'aménagement touristique de l'Aulne
FD80	Fédération d epêche de la Somme
AZTI	AZTI technalia (Basque country)
nat	utilisateur de la base nationale
ONEMA	Office National de l'eau et des milieux aquatiques
PMP	Parc du marais Poitevin
NGM	Association Normandie Grands Migrateurs
\.


--
-- Data for Name: ts_sequence_seq; Type: TABLE DATA; Schema: ref; Owner: postgres
--

COPY ts_sequence_seq (seq_sequence, seq_table, seq_column) FROM stdin;
t_operation_ope_ope_identifiant_seq	t_operation_ope	ope_identifiant
t_lot_lot_lot_identifiant_seq	t_lot_lot	lot_identifiant
t_ouvrage_ouv_ouv_identifiant_seq	t_ouvrage_ouv	ouv_identifiant
tg_dispositif_dis_dis_identifiant_seq	tg_dispositif_dis	dis_identifiant
tj_stationmesure_stm_stm_identifiant_seq	tj_stationmesure_stm	stm_identifiant
\.


SET search_path = tiger, pg_catalog;

--
-- Data for Name: geocode_settings; Type: TABLE DATA; Schema: tiger; Owner: postgres
--

COPY geocode_settings  FROM stdin;
\.


--
-- Data for Name: pagc_gaz; Type: TABLE DATA; Schema: tiger; Owner: postgres
--

COPY pagc_gaz  FROM stdin;
\.


--
-- Data for Name: pagc_lex; Type: TABLE DATA; Schema: tiger; Owner: postgres
--

COPY pagc_lex  FROM stdin;
\.


--
-- Data for Name: pagc_rules; Type: TABLE DATA; Schema: tiger; Owner: postgres
--

COPY pagc_rules  FROM stdin;
\.


SET search_path = topology, pg_catalog;

--
-- Data for Name: topology; Type: TABLE DATA; Schema: topology; Owner: postgres
--

COPY topology  FROM stdin;
\.


--
-- Data for Name: layer; Type: TABLE DATA; Schema: topology; Owner: postgres
--

COPY layer  FROM stdin;
\.


SET search_path = user_1, pg_catalog;

--
-- Data for Name: t_bilanmigrationjournalier_bjo; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY t_bilanmigrationjournalier_bjo (bjo_identifiant, bjo_dis_identifiant, bjo_tax_code, bjo_std_code, bjo_annee, bjo_jour, bjo_labelquantite, bjo_valeur, bjo_horodateexport, bjo_org_code) FROM stdin;
\.


--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: SEQUENCE SET; Schema: user_1; Owner: postgres
--

SELECT pg_catalog.setval('t_bilanmigrationjournalier_bjo_bjo_identifiant_seq', 1, false);


--
-- Data for Name: t_bilanmigrationmensuel_bme; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY t_bilanmigrationmensuel_bme (bme_identifiant, bme_dis_identifiant, bme_tax_code, bme_std_code, bme_annee, bme_mois, bme_labelquantite, bme_valeur, bme_horodateexport, bme_org_code) FROM stdin;
\.


--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: SEQUENCE SET; Schema: user_1; Owner: postgres
--

SELECT pg_catalog.setval('t_bilanmigrationmensuel_bme_bme_identifiant_seq', 1, false);


--
-- Data for Name: t_dispositifcomptage_dic; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY t_dispositifcomptage_dic (dic_dis_identifiant, dic_dif_identifiant, dic_code, dic_tdc_code, dic_org_code) FROM stdin;
\.


--
-- Data for Name: t_dispositiffranchissement_dif; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY t_dispositiffranchissement_dif (dif_dis_identifiant, dif_ouv_identifiant, dif_code, dif_localisation, dif_orientation, dif_org_code) FROM stdin;
\.


--
-- Data for Name: t_lot_lot; Type: TABLE DATA; Schema: user_1; Owner: iav
--

COPY t_lot_lot (lot_identifiant, lot_ope_identifiant, lot_tax_code, lot_std_code, lot_effectif, lot_quantite, lot_qte_code, lot_methode_obtention, lot_lot_identifiant, lot_dev_code, lot_commentaires, lot_org_code) FROM stdin;
\.


--
-- Name: t_lot_lot_lot_identifiant_seq; Type: SEQUENCE SET; Schema: user_1; Owner: iav
--

SELECT pg_catalog.setval('t_lot_lot_lot_identifiant_seq', 175116, false);


--
-- Data for Name: t_marque_mqe; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY t_marque_mqe (mqe_reference, mqe_loc_code, mqe_nmq_code, mqe_omq_reference, mqe_commentaires, mqe_org_code) FROM stdin;
\.


--
-- Data for Name: t_operation_ope; Type: TABLE DATA; Schema: user_1; Owner: iav
--

COPY t_operation_ope (ope_identifiant, ope_dic_identifiant, ope_date_debut, ope_date_fin, ope_organisme, ope_operateur, ope_commentaires, ope_org_code) FROM stdin;
\.


--
-- Name: t_operation_ope_ope_identifiant_seq; Type: SEQUENCE SET; Schema: user_1; Owner: iav
--

SELECT pg_catalog.setval('t_operation_ope_ope_identifiant_seq', 1, false);


--
-- Data for Name: t_operationmarquage_omq; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY t_operationmarquage_omq (omq_reference, omq_commentaires, omq_org_code) FROM stdin;
\.


--
-- Data for Name: t_ouvrage_ouv; Type: TABLE DATA; Schema: user_1; Owner: iav
--

COPY t_ouvrage_ouv (ouv_identifiant, ouv_sta_code, ouv_code, ouv_libelle, ouv_localisation, ouv_coordonnee_x, ouv_coordonnee_y, ouv_altitude, ouv_carte_localisation, ouv_commentaires, ouv_nov_code, ouv_org_code) FROM stdin;
\.


--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: SEQUENCE SET; Schema: user_1; Owner: iav
--

SELECT pg_catalog.setval('t_ouvrage_ouv_ouv_identifiant_seq', 7, false);


--
-- Data for Name: t_periodefonctdispositif_per; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY t_periodefonctdispositif_per (per_dis_identifiant, per_date_debut, per_date_fin, per_commentaires, per_etat_fonctionnement, per_tar_code, per_org_code) FROM stdin;
\.


--
-- Data for Name: t_station_sta; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY t_station_sta (sta_code, sta_nom, sta_localisation, sta_coordonnee_x, sta_coordonnee_y, sta_altitude, sta_carte_localisation, sta_superficie, sta_distance_mer, sta_date_creation, sta_date_suppression, sta_commentaires, sta_dernier_import_conditions, sta_org_code) FROM stdin;
\.


--
-- Data for Name: tg_dispositif_dis; Type: TABLE DATA; Schema: user_1; Owner: iav
--

COPY tg_dispositif_dis (dis_identifiant, dis_date_creation, dis_date_suppression, dis_commentaires, dis_org_code) FROM stdin;
\.


--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: SEQUENCE SET; Schema: user_1; Owner: iav
--

SELECT pg_catalog.setval('tg_dispositif_dis_dis_identifiant_seq', 19, false);


--
-- Data for Name: tj_actionmarquage_act; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY tj_actionmarquage_act (act_lot_identifiant, act_mqe_reference, act_action, act_commentaires, act_org_code, act_nmq_code) FROM stdin;
\.


--
-- Data for Name: tj_caracteristiquelot_car; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY tj_caracteristiquelot_car (car_lot_identifiant, car_par_code, car_methode_obtention, car_val_identifiant, car_valeur_quantitatif, car_precision, car_commentaires, car_org_code) FROM stdin;
\.


--
-- Data for Name: tj_coefficientconversion_coe; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY tj_coefficientconversion_coe (coe_tax_code, coe_std_code, coe_qte_code, coe_date_debut, coe_date_fin, coe_valeur_coefficient, coe_commentaires, coe_org_code) FROM stdin;
\.


--
-- Data for Name: tj_conditionenvironnementale_env; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY tj_conditionenvironnementale_env (env_date_debut, env_date_fin, env_methode_obtention, env_val_identifiant, env_valeur_quantitatif, env_stm_identifiant, env_org_code) FROM stdin;
\.


--
-- Data for Name: tj_dfestdestinea_dtx; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY tj_dfestdestinea_dtx (dtx_dif_identifiant, dtx_tax_code, dtx_org_code) FROM stdin;
\.


--
-- Data for Name: tj_dfesttype_dft; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY tj_dfesttype_dft (dft_df_identifiant, dft_rang, dft_org_code, dft_tdf_code) FROM stdin;
\.


--
-- Data for Name: tj_pathologieconstatee_pco; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY tj_pathologieconstatee_pco (pco_lot_identifiant, pco_pat_code, pco_loc_code, pco_commentaires, pco_org_code, pco_imp_code) FROM stdin;
\.


--
-- Data for Name: tj_prelevementlot_prl; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY tj_prelevementlot_prl (prl_pre_typeprelevement, prl_lot_identifiant, prl_code, prl_operateur, prl_loc_code, prl_commentaires, prl_org_code) FROM stdin;
\.


--
-- Data for Name: tj_stationmesure_stm; Type: TABLE DATA; Schema: user_1; Owner: iav
--

COPY tj_stationmesure_stm (stm_identifiant, stm_libelle, stm_sta_code, stm_par_code, stm_description, stm_org_code) FROM stdin;
\.


--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: SEQUENCE SET; Schema: user_1; Owner: iav
--

SELECT pg_catalog.setval('tj_stationmesure_stm_stm_identifiant_seq', 19, false);


--
-- Data for Name: tj_tauxechappement_txe; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY tj_tauxechappement_txe (txe_tax_code, txe_std_code, txe_date_debut, txe_date_fin, txe_methode_estimation, txe_ech_code, txe_valeur_taux, txe_commentaires, txe_org_code, txe_sta_code) FROM stdin;
\.


--
-- Data for Name: ts_maintenance_main; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY ts_maintenance_main (main_identifiant, main_ticket, main_description) FROM stdin;
\.


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE SET; Schema: user_1; Owner: postgres
--

SELECT pg_catalog.setval('ts_maintenance_main_main_identifiant_seq', 1, false);


--
-- Data for Name: ts_masque_mas; Type: TABLE DATA; Schema: user_1; Owner: iav
--

COPY ts_masque_mas (mas_id, mas_code, mas_description, mas_raccourci, mas_type) FROM stdin;
\.


--
-- Name: ts_masque_mas_mas_id_seq; Type: SEQUENCE SET; Schema: user_1; Owner: iav
--

SELECT pg_catalog.setval('ts_masque_mas_mas_id_seq', 1, false);


--
-- Data for Name: ts_masquecaracteristiquelot_mac; Type: TABLE DATA; Schema: user_1; Owner: iav
--

COPY ts_masquecaracteristiquelot_mac (mac_id, mac_mal_id, mac_par_code, mac_affichagevaleur, mac_affichageprecision, mac_affichagemethodeobtention, mac_affichagecommentaire, mac_valeurquantitatifdefaut, mac_valeurqualitatifdefaut, mac_precisiondefaut, mac_methodeobtentiondefaut, mac_commentairedefaut) FROM stdin;
\.


--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: SEQUENCE SET; Schema: user_1; Owner: iav
--

SELECT pg_catalog.setval('ts_masquecaracteristiquelot_mac_mac_id_seq', 1, false);


--
-- Data for Name: ts_masqueconditionsenvironnementales_mae; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY ts_masqueconditionsenvironnementales_mae (mae_mao_id, mae_stm_identifiant, mae_affichage, mae_valeurdefaut) FROM stdin;
\.


--
-- Data for Name: ts_masquelot_mal; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY ts_masquelot_mal (mal_mas_id, mal_affichage, mal_valeurdefaut) FROM stdin;
\.


--
-- Data for Name: ts_masqueope_mao; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY ts_masqueope_mao (mao_mas_id, mao_affichage, mao_valeurdefaut) FROM stdin;
\.


--
-- Data for Name: ts_masqueordreaffichage_maa; Type: TABLE DATA; Schema: user_1; Owner: iav
--

COPY ts_masqueordreaffichage_maa (maa_id, maa_mal_id, maa_table, maa_valeur, maa_champdumasque, maa_rang) FROM stdin;
\.


--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: SEQUENCE SET; Schema: user_1; Owner: iav
--

SELECT pg_catalog.setval('ts_masqueordreaffichage_maa_maa_id_seq', 1, false);


--
-- Data for Name: ts_taillevideo_tav; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY ts_taillevideo_tav (tav_dic_identifiant, tav_coefconversion, tav_distance, tav_org_code) FROM stdin;
\.


--
-- Data for Name: ts_taxonvideo_txv; Type: TABLE DATA; Schema: user_1; Owner: postgres
--

COPY ts_taxonvideo_txv (txv_code, txv_tax_code, txv_std_code, txv_org_code) FROM stdin;
\.


SET search_path = user_2, pg_catalog;

--
-- Data for Name: t_bilanmigrationjournalier_bjo; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY t_bilanmigrationjournalier_bjo (bjo_identifiant, bjo_dis_identifiant, bjo_tax_code, bjo_std_code, bjo_annee, bjo_jour, bjo_labelquantite, bjo_valeur, bjo_horodateexport, bjo_org_code) FROM stdin;
\.


--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: SEQUENCE SET; Schema: user_2; Owner: postgres
--

SELECT pg_catalog.setval('t_bilanmigrationjournalier_bjo_bjo_identifiant_seq', 1, false);


--
-- Data for Name: t_bilanmigrationmensuel_bme; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY t_bilanmigrationmensuel_bme (bme_identifiant, bme_dis_identifiant, bme_tax_code, bme_std_code, bme_annee, bme_mois, bme_labelquantite, bme_valeur, bme_horodateexport, bme_org_code) FROM stdin;
\.


--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: SEQUENCE SET; Schema: user_2; Owner: postgres
--

SELECT pg_catalog.setval('t_bilanmigrationmensuel_bme_bme_identifiant_seq', 1, false);


--
-- Data for Name: t_dispositifcomptage_dic; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY t_dispositifcomptage_dic (dic_dis_identifiant, dic_dif_identifiant, dic_code, dic_tdc_code, dic_org_code) FROM stdin;
\.


--
-- Data for Name: t_dispositiffranchissement_dif; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY t_dispositiffranchissement_dif (dif_dis_identifiant, dif_ouv_identifiant, dif_code, dif_localisation, dif_orientation, dif_org_code) FROM stdin;
\.


--
-- Data for Name: t_lot_lot; Type: TABLE DATA; Schema: user_2; Owner: iav
--

COPY t_lot_lot (lot_identifiant, lot_ope_identifiant, lot_tax_code, lot_std_code, lot_effectif, lot_quantite, lot_qte_code, lot_methode_obtention, lot_lot_identifiant, lot_dev_code, lot_commentaires, lot_org_code) FROM stdin;
\.


--
-- Name: t_lot_lot_lot_identifiant_seq; Type: SEQUENCE SET; Schema: user_2; Owner: iav
--

SELECT pg_catalog.setval('t_lot_lot_lot_identifiant_seq', 175116, false);


--
-- Data for Name: t_marque_mqe; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY t_marque_mqe (mqe_reference, mqe_loc_code, mqe_nmq_code, mqe_omq_reference, mqe_commentaires, mqe_org_code) FROM stdin;
\.


--
-- Data for Name: t_operation_ope; Type: TABLE DATA; Schema: user_2; Owner: iav
--

COPY t_operation_ope (ope_identifiant, ope_dic_identifiant, ope_date_debut, ope_date_fin, ope_organisme, ope_operateur, ope_commentaires, ope_org_code) FROM stdin;
\.


--
-- Name: t_operation_ope_ope_identifiant_seq; Type: SEQUENCE SET; Schema: user_2; Owner: iav
--

SELECT pg_catalog.setval('t_operation_ope_ope_identifiant_seq', 1, false);


--
-- Data for Name: t_operationmarquage_omq; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY t_operationmarquage_omq (omq_reference, omq_commentaires, omq_org_code) FROM stdin;
\.


--
-- Data for Name: t_ouvrage_ouv; Type: TABLE DATA; Schema: user_2; Owner: iav
--

COPY t_ouvrage_ouv (ouv_identifiant, ouv_sta_code, ouv_code, ouv_libelle, ouv_localisation, ouv_coordonnee_x, ouv_coordonnee_y, ouv_altitude, ouv_carte_localisation, ouv_commentaires, ouv_nov_code, ouv_org_code) FROM stdin;
\.


--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: SEQUENCE SET; Schema: user_2; Owner: iav
--

SELECT pg_catalog.setval('t_ouvrage_ouv_ouv_identifiant_seq', 7, false);


--
-- Data for Name: t_periodefonctdispositif_per; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY t_periodefonctdispositif_per (per_dis_identifiant, per_date_debut, per_date_fin, per_commentaires, per_etat_fonctionnement, per_tar_code, per_org_code) FROM stdin;
\.


--
-- Data for Name: t_station_sta; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY t_station_sta (sta_code, sta_nom, sta_localisation, sta_coordonnee_x, sta_coordonnee_y, sta_altitude, sta_carte_localisation, sta_superficie, sta_distance_mer, sta_date_creation, sta_date_suppression, sta_commentaires, sta_dernier_import_conditions, sta_org_code) FROM stdin;
\.


--
-- Data for Name: tg_dispositif_dis; Type: TABLE DATA; Schema: user_2; Owner: iav
--

COPY tg_dispositif_dis (dis_identifiant, dis_date_creation, dis_date_suppression, dis_commentaires, dis_org_code) FROM stdin;
\.


--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: SEQUENCE SET; Schema: user_2; Owner: iav
--

SELECT pg_catalog.setval('tg_dispositif_dis_dis_identifiant_seq', 19, false);


--
-- Data for Name: tj_actionmarquage_act; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY tj_actionmarquage_act (act_lot_identifiant, act_mqe_reference, act_action, act_commentaires, act_org_code, act_nmq_code) FROM stdin;
\.


--
-- Data for Name: tj_caracteristiquelot_car; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY tj_caracteristiquelot_car (car_lot_identifiant, car_par_code, car_methode_obtention, car_val_identifiant, car_valeur_quantitatif, car_precision, car_commentaires, car_org_code) FROM stdin;
\.


--
-- Data for Name: tj_coefficientconversion_coe; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY tj_coefficientconversion_coe (coe_tax_code, coe_std_code, coe_qte_code, coe_date_debut, coe_date_fin, coe_valeur_coefficient, coe_commentaires, coe_org_code) FROM stdin;
\.


--
-- Data for Name: tj_conditionenvironnementale_env; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY tj_conditionenvironnementale_env (env_date_debut, env_date_fin, env_methode_obtention, env_val_identifiant, env_valeur_quantitatif, env_stm_identifiant, env_org_code) FROM stdin;
\.


--
-- Data for Name: tj_dfestdestinea_dtx; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY tj_dfestdestinea_dtx (dtx_dif_identifiant, dtx_tax_code, dtx_org_code) FROM stdin;
\.


--
-- Data for Name: tj_dfesttype_dft; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY tj_dfesttype_dft (dft_df_identifiant, dft_rang, dft_org_code, dft_tdf_code) FROM stdin;
\.


--
-- Data for Name: tj_pathologieconstatee_pco; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY tj_pathologieconstatee_pco (pco_lot_identifiant, pco_pat_code, pco_loc_code, pco_commentaires, pco_org_code, pco_imp_code) FROM stdin;
\.


--
-- Data for Name: tj_prelevementlot_prl; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY tj_prelevementlot_prl (prl_pre_typeprelevement, prl_lot_identifiant, prl_code, prl_operateur, prl_loc_code, prl_commentaires, prl_org_code) FROM stdin;
\.


--
-- Data for Name: tj_stationmesure_stm; Type: TABLE DATA; Schema: user_2; Owner: iav
--

COPY tj_stationmesure_stm (stm_identifiant, stm_libelle, stm_sta_code, stm_par_code, stm_description, stm_org_code) FROM stdin;
\.


--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: SEQUENCE SET; Schema: user_2; Owner: iav
--

SELECT pg_catalog.setval('tj_stationmesure_stm_stm_identifiant_seq', 19, false);


--
-- Data for Name: tj_tauxechappement_txe; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY tj_tauxechappement_txe (txe_tax_code, txe_std_code, txe_date_debut, txe_date_fin, txe_methode_estimation, txe_ech_code, txe_valeur_taux, txe_commentaires, txe_org_code, txe_sta_code) FROM stdin;
\.


--
-- Data for Name: ts_maintenance_main; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY ts_maintenance_main (main_identifiant, main_ticket, main_description) FROM stdin;
\.


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: SEQUENCE SET; Schema: user_2; Owner: postgres
--

SELECT pg_catalog.setval('ts_maintenance_main_main_identifiant_seq', 1, false);


--
-- Data for Name: ts_masque_mas; Type: TABLE DATA; Schema: user_2; Owner: iav
--

COPY ts_masque_mas (mas_id, mas_code, mas_description, mas_raccourci, mas_type) FROM stdin;
\.


--
-- Name: ts_masque_mas_mas_id_seq; Type: SEQUENCE SET; Schema: user_2; Owner: iav
--

SELECT pg_catalog.setval('ts_masque_mas_mas_id_seq', 1, false);


--
-- Data for Name: ts_masquecaracteristiquelot_mac; Type: TABLE DATA; Schema: user_2; Owner: iav
--

COPY ts_masquecaracteristiquelot_mac (mac_id, mac_mal_id, mac_par_code, mac_affichagevaleur, mac_affichageprecision, mac_affichagemethodeobtention, mac_affichagecommentaire, mac_valeurquantitatifdefaut, mac_valeurqualitatifdefaut, mac_precisiondefaut, mac_methodeobtentiondefaut, mac_commentairedefaut) FROM stdin;
\.


--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: SEQUENCE SET; Schema: user_2; Owner: iav
--

SELECT pg_catalog.setval('ts_masquecaracteristiquelot_mac_mac_id_seq', 1, false);


--
-- Data for Name: ts_masqueconditionsenvironnementales_mae; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY ts_masqueconditionsenvironnementales_mae (mae_mao_id, mae_stm_identifiant, mae_affichage, mae_valeurdefaut) FROM stdin;
\.


--
-- Data for Name: ts_masquelot_mal; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY ts_masquelot_mal (mal_mas_id, mal_affichage, mal_valeurdefaut) FROM stdin;
\.


--
-- Data for Name: ts_masqueope_mao; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY ts_masqueope_mao (mao_mas_id, mao_affichage, mao_valeurdefaut) FROM stdin;
\.


--
-- Data for Name: ts_masqueordreaffichage_maa; Type: TABLE DATA; Schema: user_2; Owner: iav
--

COPY ts_masqueordreaffichage_maa (maa_id, maa_mal_id, maa_table, maa_valeur, maa_champdumasque, maa_rang) FROM stdin;
\.


--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: SEQUENCE SET; Schema: user_2; Owner: iav
--

SELECT pg_catalog.setval('ts_masqueordreaffichage_maa_maa_id_seq', 1, false);


--
-- Data for Name: ts_taillevideo_tav; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY ts_taillevideo_tav (tav_dic_identifiant, tav_coefconversion, tav_distance, tav_org_code) FROM stdin;
\.


--
-- Data for Name: ts_taxonvideo_txv; Type: TABLE DATA; Schema: user_2; Owner: postgres
--

COPY ts_taxonvideo_txv (txv_code, txv_tax_code, txv_std_code, txv_org_code) FROM stdin;
\.


SET search_path = nat, pg_catalog;

--
-- Name: c_pk_act; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_actionmarquage_act
    ADD CONSTRAINT c_pk_act PRIMARY KEY (act_lot_identifiant, act_mqe_reference, act_org_code);


--
-- Name: c_pk_bjo; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY t_bilanmigrationjournalier_bjo
    ADD CONSTRAINT c_pk_bjo PRIMARY KEY (bjo_identifiant, bjo_org_code);


--
-- Name: c_pk_bme; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY t_bilanmigrationmensuel_bme
    ADD CONSTRAINT c_pk_bme PRIMARY KEY (bme_identifiant, bme_org_code);


--
-- Name: c_pk_car; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_caracteristiquelot_car
    ADD CONSTRAINT c_pk_car PRIMARY KEY (car_lot_identifiant, car_par_code, car_org_code);


--
-- Name: c_pk_coe; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_coefficientconversion_coe
    ADD CONSTRAINT c_pk_coe PRIMARY KEY (coe_tax_code, coe_std_code, coe_qte_code, coe_date_debut, coe_date_fin, coe_org_code);


--
-- Name: c_pk_dft; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_dfesttype_dft
    ADD CONSTRAINT c_pk_dft PRIMARY KEY (dft_df_identifiant, dft_tdf_code, dft_rang, dft_org_code);


--
-- Name: c_pk_dic; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_pk_dic PRIMARY KEY (dic_dis_identifiant, dic_org_code);


--
-- Name: c_pk_dif; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_pk_dif PRIMARY KEY (dif_dis_identifiant, dif_org_code);


--
-- Name: c_pk_dis; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY tg_dispositif_dis
    ADD CONSTRAINT c_pk_dis PRIMARY KEY (dis_identifiant, dis_org_code);


--
-- Name: c_pk_dtx; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_dfestdestinea_dtx
    ADD CONSTRAINT c_pk_dtx PRIMARY KEY (dtx_dif_identifiant, dtx_tax_code, dtx_org_code);


--
-- Name: c_pk_env; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_conditionenvironnementale_env
    ADD CONSTRAINT c_pk_env PRIMARY KEY (env_stm_identifiant, env_date_debut, env_date_fin, env_org_code);


--
-- Name: c_pk_lot; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY t_lot_lot
    ADD CONSTRAINT c_pk_lot PRIMARY KEY (lot_identifiant, lot_org_code);


--
-- Name: c_pk_mac; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY ts_masquecaracteristiquelot_mac
    ADD CONSTRAINT c_pk_mac PRIMARY KEY (mac_mal_id, mac_par_code);


--
-- Name: c_pk_mae; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_masqueconditionsenvironnementales_mae
    ADD CONSTRAINT c_pk_mae PRIMARY KEY (mae_mao_id, mae_stm_identifiant);


--
-- Name: c_pk_mal_mas_id; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_masquelot_mal
    ADD CONSTRAINT c_pk_mal_mas_id PRIMARY KEY (mal_mas_id);


--
-- Name: c_pk_mao_mas_id; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_masqueope_mao
    ADD CONSTRAINT c_pk_mao_mas_id PRIMARY KEY (mao_mas_id);


--
-- Name: c_pk_mas_id; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY ts_masque_mas
    ADD CONSTRAINT c_pk_mas_id PRIMARY KEY (mas_id);


--
-- Name: c_pk_mqe; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_marque_mqe
    ADD CONSTRAINT c_pk_mqe PRIMARY KEY (mqe_reference, mqe_nmq_code, mqe_org_code);


--
-- Name: c_pk_omq; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_operationmarquage_omq
    ADD CONSTRAINT c_pk_omq PRIMARY KEY (omq_reference, omq_org_code);


--
-- Name: c_pk_ope; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY t_operation_ope
    ADD CONSTRAINT c_pk_ope PRIMARY KEY (ope_identifiant, ope_org_code);


--
-- Name: c_pk_ouv; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY t_ouvrage_ouv
    ADD CONSTRAINT c_pk_ouv PRIMARY KEY (ouv_identifiant, ouv_org_code);


--
-- Name: c_pk_pco; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_pathologieconstatee_pco
    ADD CONSTRAINT c_pk_pco PRIMARY KEY (pco_lot_identifiant, pco_pat_code, pco_loc_code, pco_org_code);


--
-- Name: c_pk_per; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_periodefonctdispositif_per
    ADD CONSTRAINT c_pk_per PRIMARY KEY (per_dis_identifiant, per_org_code, per_date_debut, per_date_fin);


--
-- Name: c_pk_prl; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_prelevementlot_prl
    ADD CONSTRAINT c_pk_prl PRIMARY KEY (prl_pre_typeprelevement, prl_lot_identifiant, prl_code, prl_org_code);


--
-- Name: c_pk_sta; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_station_sta
    ADD CONSTRAINT c_pk_sta PRIMARY KEY (sta_code, sta_org_code);


--
-- Name: c_pk_stm; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_pk_stm PRIMARY KEY (stm_identifiant, stm_org_code);


--
-- Name: c_pk_tav; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_taillevideo_tav
    ADD CONSTRAINT c_pk_tav PRIMARY KEY (tav_distance, tav_dic_identifiant, tav_org_code);


--
-- Name: c_pk_txe; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_tauxechappement_txe
    ADD CONSTRAINT c_pk_txe PRIMARY KEY (txe_sta_code, txe_org_code, txe_tax_code, txe_std_code, txe_date_debut, txe_date_fin);


--
-- Name: c_pk_txv; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_taxonvideo_txv
    ADD CONSTRAINT c_pk_txv PRIMARY KEY (txv_code, txv_org_code);


--
-- Name: c_uk_dic_dis_identifiant; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_uk_dic_dis_identifiant UNIQUE (dic_dis_identifiant, dic_org_code);


--
-- Name: c_uk_dif_dis_identifiant; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_uk_dif_dis_identifiant UNIQUE (dif_dis_identifiant, dif_org_code);


--
-- Name: c_uk_maa; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa
    ADD CONSTRAINT c_uk_maa UNIQUE (maa_mal_id, maa_table, maa_valeur, maa_champdumasque);


--
-- Name: c_uk_main_ticket; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_maintenance_main
    ADD CONSTRAINT c_uk_main_ticket UNIQUE (main_ticket);


--
-- Name: c_uk_prl_code; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_prelevementlot_prl
    ADD CONSTRAINT c_uk_prl_code UNIQUE (prl_code);


--
-- Name: c_uk_stm; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_uk_stm UNIQUE (stm_libelle, stm_org_code);


--
-- Name: c_uq_dic; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_uq_dic UNIQUE (dic_dif_identifiant, dic_code);


--
-- Name: c_uq_dif; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_uq_dif UNIQUE (dif_ouv_identifiant, dif_code);


--
-- Name: c_uq_ope; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY t_operation_ope
    ADD CONSTRAINT c_uq_ope UNIQUE (ope_org_code, ope_dic_identifiant, ope_date_debut, ope_date_fin);


--
-- Name: c_uq_ouv; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY t_ouvrage_ouv
    ADD CONSTRAINT c_uq_ouv UNIQUE (ouv_sta_code, ouv_code);


--
-- Name: c_uq_sta_nom; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_station_sta
    ADD CONSTRAINT c_uq_sta_nom UNIQUE (sta_nom, sta_org_code);


--
-- Name: ts_maintenance_main_pkey; Type: CONSTRAINT; Schema: nat; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_maintenance_main
    ADD CONSTRAINT ts_maintenance_main_pkey PRIMARY KEY (main_identifiant);


--
-- Name: ts_masque_mas_mas_code_key; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY ts_masque_mas
    ADD CONSTRAINT ts_masque_mas_mas_code_key UNIQUE (mas_code);


--
-- Name: ts_masqueordreaffichage_maa_pkey; Type: CONSTRAINT; Schema: nat; Owner: nat; Tablespace: 
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa
    ADD CONSTRAINT ts_masqueordreaffichage_maa_pkey PRIMARY KEY (maa_id);


SET search_path = ref, pg_catalog;

--
-- Name: c_pk_dev; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_devenirlot_dev
    ADD CONSTRAINT c_pk_dev PRIMARY KEY (dev_code);


--
-- Name: c_pk_ech; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_niveauechappement_ech
    ADD CONSTRAINT c_pk_ech PRIMARY KEY (ech_code);


--
-- Name: c_pk_loc; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_localisationanatomique_loc
    ADD CONSTRAINT c_pk_loc PRIMARY KEY (loc_code);


--
-- Name: c_pk_nmq; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_naturemarque_nmq
    ADD CONSTRAINT c_pk_nmq PRIMARY KEY (nmq_code);


--
-- Name: c_pk_nov; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_natureouvrage_nov
    ADD CONSTRAINT c_pk_nov PRIMARY KEY (nov_code);


--
-- Name: c_pk_ntx; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_niveautaxonomique_ntx
    ADD CONSTRAINT c_pk_ntx PRIMARY KEY (ntx_code);


--
-- Name: c_pk_org; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_organisme_org
    ADD CONSTRAINT c_pk_org PRIMARY KEY (org_code);


--
-- Name: c_pk_par; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tg_parametre_par
    ADD CONSTRAINT c_pk_par PRIMARY KEY (par_code);


--
-- Name: c_pk_pat; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_pathologie_pat
    ADD CONSTRAINT c_pk_pat PRIMARY KEY (pat_code);


--
-- Name: c_pk_pre_nom; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_prelevement_pre
    ADD CONSTRAINT c_pk_pre_nom PRIMARY KEY (pre_typeprelevement);


--
-- Name: c_pk_qal; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_parametrequalitatif_qal
    ADD CONSTRAINT c_pk_qal PRIMARY KEY (qal_par_code);


--
-- Name: c_pk_qan; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_parametrequantitatif_qan
    ADD CONSTRAINT c_pk_qan PRIMARY KEY (qan_par_code);


--
-- Name: c_pk_qte; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_typequantitelot_qte
    ADD CONSTRAINT c_pk_qte PRIMARY KEY (qte_code);


--
-- Name: c_pk_std; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_stadedeveloppement_std
    ADD CONSTRAINT c_pk_std PRIMARY KEY (std_code);


--
-- Name: c_pk_tar; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_typearretdisp_tar
    ADD CONSTRAINT c_pk_tar PRIMARY KEY (tar_code);


--
-- Name: c_pk_tax; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_taxon_tax
    ADD CONSTRAINT c_pk_tax PRIMARY KEY (tax_code);


--
-- Name: c_pk_tdc; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_typedc_tdc
    ADD CONSTRAINT c_pk_tdc PRIMARY KEY (tdc_code);


--
-- Name: c_pk_tdf; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_typedf_tdf
    ADD CONSTRAINT c_pk_tdf PRIMARY KEY (tdf_code);


--
-- Name: c_pk_val; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_valeurparametrequalitatif_val
    ADD CONSTRAINT c_pk_val PRIMARY KEY (val_identifiant);


--
-- Name: c_seq_sequence; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_sequence_seq
    ADD CONSTRAINT c_seq_sequence PRIMARY KEY (seq_sequence);


--
-- Name: c_uk2_ts_messagerlang_mrl; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_messagerlang_mrl
    ADD CONSTRAINT c_uk2_ts_messagerlang_mrl UNIQUE (mrl_msr_id, mrl_lang);


--
-- Name: c_uk_tdfmnemonique; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_typedf_tdf
    ADD CONSTRAINT c_uk_tdfmnemonique UNIQUE (tdf_mnemonique);


--
-- Name: c_uk_ts_messager_msr; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_messager_msr
    ADD CONSTRAINT c_uk_ts_messager_msr UNIQUE (msr_element, msr_number);


--
-- Name: c_uk_ts_messagerlang_mrl; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_messagerlang_mrl
    ADD CONSTRAINT c_uk_ts_messagerlang_mrl UNIQUE (mrl_id, mrl_lang);


--
-- Name: c_uk_val_identifiant; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_valeurparametrequalitatif_val
    ADD CONSTRAINT c_uk_val_identifiant UNIQUE (val_identifiant, val_qal_code);


--
-- Name: c_uq_dev_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_devenirlot_dev
    ADD CONSTRAINT c_uq_dev_libelle UNIQUE (dev_libelle);


--
-- Name: c_uq_ech_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_niveauechappement_ech
    ADD CONSTRAINT c_uq_ech_libelle UNIQUE (ech_libelle);


--
-- Name: c_uq_loc_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_localisationanatomique_loc
    ADD CONSTRAINT c_uq_loc_libelle UNIQUE (loc_libelle);


--
-- Name: c_uq_nmq_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_naturemarque_nmq
    ADD CONSTRAINT c_uq_nmq_libelle UNIQUE (nmq_libelle);


--
-- Name: c_uq_nov_nom; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_natureouvrage_nov
    ADD CONSTRAINT c_uq_nov_nom UNIQUE (nov_nom);


--
-- Name: c_uq_ntx_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_niveautaxonomique_ntx
    ADD CONSTRAINT c_uq_ntx_libelle UNIQUE (ntx_libelle);


--
-- Name: c_uq_par_nom; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tg_parametre_par
    ADD CONSTRAINT c_uq_par_nom UNIQUE (par_nom);


--
-- Name: c_uq_pat_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_pathologie_pat
    ADD CONSTRAINT c_uq_pat_libelle UNIQUE (pat_libelle);


--
-- Name: c_uq_qte_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_typequantitelot_qte
    ADD CONSTRAINT c_uq_qte_libelle UNIQUE (qte_libelle);


--
-- Name: c_uq_std_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_stadedeveloppement_std
    ADD CONSTRAINT c_uq_std_libelle UNIQUE (std_libelle);


--
-- Name: c_uq_tar_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_typearretdisp_tar
    ADD CONSTRAINT c_uq_tar_libelle UNIQUE (tar_libelle);


--
-- Name: c_uq_tdc_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_typedc_tdc
    ADD CONSTRAINT c_uq_tdc_libelle UNIQUE (tdc_libelle);


--
-- Name: c_uq_tdf_libelle; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_typedf_tdf
    ADD CONSTRAINT c_uq_tdf_libelle UNIQUE (tdf_libelle);


--
-- Name: c_uq_val; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_valeurparametrequalitatif_val
    ADD CONSTRAINT c_uq_val UNIQUE (val_qal_code, val_rang);


--
-- Name: tr_importancepatho_imp_pkey; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tr_importancepatho_imp
    ADD CONSTRAINT tr_importancepatho_imp_pkey PRIMARY KEY (imp_code);


--
-- Name: ts_maintenance_main_pkey; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_maintenance_main
    ADD CONSTRAINT ts_maintenance_main_pkey PRIMARY KEY (main_identifiant);


--
-- Name: ts_messager_msr_pkey; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_messager_msr
    ADD CONSTRAINT ts_messager_msr_pkey PRIMARY KEY (msr_id);


--
-- Name: ts_messagerlang_mrl_pkey; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_messagerlang_mrl
    ADD CONSTRAINT ts_messagerlang_mrl_pkey PRIMARY KEY (mrl_id);


--
-- Name: ts_nomenclature_nom_pkey; Type: CONSTRAINT; Schema: ref; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_nomenclature_nom
    ADD CONSTRAINT ts_nomenclature_nom_pkey PRIMARY KEY (nom_id);


SET search_path = user_1, pg_catalog;

--
-- Name: c_pk_act; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_actionmarquage_act
    ADD CONSTRAINT c_pk_act PRIMARY KEY (act_lot_identifiant, act_mqe_reference, act_org_code);


--
-- Name: c_pk_bjo; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_bilanmigrationjournalier_bjo
    ADD CONSTRAINT c_pk_bjo PRIMARY KEY (bjo_identifiant, bjo_org_code);


--
-- Name: c_pk_bme; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_bilanmigrationmensuel_bme
    ADD CONSTRAINT c_pk_bme PRIMARY KEY (bme_identifiant, bme_org_code);


--
-- Name: c_pk_car; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_caracteristiquelot_car
    ADD CONSTRAINT c_pk_car PRIMARY KEY (car_lot_identifiant, car_par_code, car_org_code);


--
-- Name: c_pk_coe; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_coefficientconversion_coe
    ADD CONSTRAINT c_pk_coe PRIMARY KEY (coe_tax_code, coe_std_code, coe_qte_code, coe_date_debut, coe_date_fin, coe_org_code);


--
-- Name: c_pk_dft; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_dfesttype_dft
    ADD CONSTRAINT c_pk_dft PRIMARY KEY (dft_df_identifiant, dft_tdf_code, dft_rang, dft_org_code);


--
-- Name: c_pk_dic; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_pk_dic PRIMARY KEY (dic_code, dic_org_code);


--
-- Name: c_pk_dif; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_pk_dif PRIMARY KEY (dif_code, dif_org_code);


--
-- Name: c_pk_dis; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tg_dispositif_dis
    ADD CONSTRAINT c_pk_dis PRIMARY KEY (dis_identifiant, dis_org_code);


--
-- Name: c_pk_dtx; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_dfestdestinea_dtx
    ADD CONSTRAINT c_pk_dtx PRIMARY KEY (dtx_dif_identifiant, dtx_tax_code, dtx_org_code);


--
-- Name: c_pk_env; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_conditionenvironnementale_env
    ADD CONSTRAINT c_pk_env PRIMARY KEY (env_stm_identifiant, env_date_debut, env_date_fin, env_org_code);


--
-- Name: c_pk_lot; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_lot_lot
    ADD CONSTRAINT c_pk_lot PRIMARY KEY (lot_identifiant, lot_org_code);


--
-- Name: c_pk_mac; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masquecaracteristiquelot_mac
    ADD CONSTRAINT c_pk_mac PRIMARY KEY (mac_mal_id, mac_par_code);


--
-- Name: c_pk_mae; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_masqueconditionsenvironnementales_mae
    ADD CONSTRAINT c_pk_mae PRIMARY KEY (mae_mao_id, mae_stm_identifiant);


--
-- Name: c_pk_mal_mas_id; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_masquelot_mal
    ADD CONSTRAINT c_pk_mal_mas_id PRIMARY KEY (mal_mas_id);


--
-- Name: c_pk_mao_mas_id; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_masqueope_mao
    ADD CONSTRAINT c_pk_mao_mas_id PRIMARY KEY (mao_mas_id);


--
-- Name: c_pk_mas_id; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masque_mas
    ADD CONSTRAINT c_pk_mas_id PRIMARY KEY (mas_id);


--
-- Name: c_pk_mqe; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_marque_mqe
    ADD CONSTRAINT c_pk_mqe PRIMARY KEY (mqe_reference, mqe_nmq_code, mqe_org_code);


--
-- Name: c_pk_omq; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_operationmarquage_omq
    ADD CONSTRAINT c_pk_omq PRIMARY KEY (omq_reference, omq_org_code);


--
-- Name: c_pk_ope; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_operation_ope
    ADD CONSTRAINT c_pk_ope PRIMARY KEY (ope_identifiant, ope_org_code);


--
-- Name: c_pk_ouv; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_ouvrage_ouv
    ADD CONSTRAINT c_pk_ouv PRIMARY KEY (ouv_identifiant, ouv_org_code);


--
-- Name: c_pk_pco; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_pathologieconstatee_pco
    ADD CONSTRAINT c_pk_pco PRIMARY KEY (pco_lot_identifiant, pco_pat_code, pco_loc_code, pco_org_code);


--
-- Name: c_pk_per; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_periodefonctdispositif_per
    ADD CONSTRAINT c_pk_per PRIMARY KEY (per_dis_identifiant, per_org_code, per_date_debut, per_date_fin);


--
-- Name: c_pk_prl; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_prelevementlot_prl
    ADD CONSTRAINT c_pk_prl PRIMARY KEY (prl_pre_typeprelevement, prl_lot_identifiant, prl_code, prl_org_code);


--
-- Name: c_pk_sta; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_station_sta
    ADD CONSTRAINT c_pk_sta PRIMARY KEY (sta_code, sta_org_code);


--
-- Name: c_pk_stm; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_pk_stm PRIMARY KEY (stm_identifiant, stm_org_code);


--
-- Name: c_pk_tav; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_taillevideo_tav
    ADD CONSTRAINT c_pk_tav PRIMARY KEY (tav_distance, tav_dic_identifiant, tav_org_code);


--
-- Name: c_pk_txe; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_tauxechappement_txe
    ADD CONSTRAINT c_pk_txe PRIMARY KEY (txe_sta_code, txe_org_code, txe_tax_code, txe_std_code, txe_date_debut, txe_date_fin);


--
-- Name: c_pk_txv; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_taxonvideo_txv
    ADD CONSTRAINT c_pk_txv PRIMARY KEY (txv_code, txv_org_code);


--
-- Name: c_uk_dic_dis_identifiant; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_uk_dic_dis_identifiant UNIQUE (dic_dis_identifiant, dic_org_code);


--
-- Name: c_uk_dif_dis_identifiant; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_uk_dif_dis_identifiant UNIQUE (dif_dis_identifiant, dif_org_code);


--
-- Name: c_uk_maa; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa
    ADD CONSTRAINT c_uk_maa UNIQUE (maa_mal_id, maa_table, maa_valeur, maa_champdumasque);


--
-- Name: c_uk_main_ticket; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_maintenance_main
    ADD CONSTRAINT c_uk_main_ticket UNIQUE (main_ticket);


--
-- Name: c_uk_prl_code; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_prelevementlot_prl
    ADD CONSTRAINT c_uk_prl_code UNIQUE (prl_code);


--
-- Name: c_uk_stm; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_uk_stm UNIQUE (stm_libelle);


--
-- Name: c_uk_stm_identifiant; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_uk_stm_identifiant UNIQUE (stm_identifiant);


--
-- Name: c_uk_stm_libelle; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_uk_stm_libelle UNIQUE (stm_libelle);


--
-- Name: c_uq_dic; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_uq_dic UNIQUE (dic_dif_identifiant, dic_code);


--
-- Name: c_uq_dif; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_uq_dif UNIQUE (dif_ouv_identifiant, dif_code);


--
-- Name: c_uq_ope; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_operation_ope
    ADD CONSTRAINT c_uq_ope UNIQUE (ope_dic_identifiant, ope_date_debut, ope_date_fin);


--
-- Name: c_uq_ouv; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_ouvrage_ouv
    ADD CONSTRAINT c_uq_ouv UNIQUE (ouv_sta_code, ouv_code);


--
-- Name: c_uq_sta_nom; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_station_sta
    ADD CONSTRAINT c_uq_sta_nom UNIQUE (sta_nom);


--
-- Name: ts_maintenance_main_pkey; Type: CONSTRAINT; Schema: user_1; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_maintenance_main
    ADD CONSTRAINT ts_maintenance_main_pkey PRIMARY KEY (main_identifiant);


--
-- Name: ts_masque_mas_mas_code_key; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masque_mas
    ADD CONSTRAINT ts_masque_mas_mas_code_key UNIQUE (mas_code);


--
-- Name: ts_masqueordreaffichage_maa_pkey; Type: CONSTRAINT; Schema: user_1; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa
    ADD CONSTRAINT ts_masqueordreaffichage_maa_pkey PRIMARY KEY (maa_id);


SET search_path = user_2, pg_catalog;

--
-- Name: c_pk_act; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_actionmarquage_act
    ADD CONSTRAINT c_pk_act PRIMARY KEY (act_lot_identifiant, act_mqe_reference, act_org_code);


--
-- Name: c_pk_bjo; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_bilanmigrationjournalier_bjo
    ADD CONSTRAINT c_pk_bjo PRIMARY KEY (bjo_identifiant, bjo_org_code);


--
-- Name: c_pk_bme; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_bilanmigrationmensuel_bme
    ADD CONSTRAINT c_pk_bme PRIMARY KEY (bme_identifiant, bme_org_code);


--
-- Name: c_pk_car; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_caracteristiquelot_car
    ADD CONSTRAINT c_pk_car PRIMARY KEY (car_lot_identifiant, car_par_code, car_org_code);


--
-- Name: c_pk_coe; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_coefficientconversion_coe
    ADD CONSTRAINT c_pk_coe PRIMARY KEY (coe_tax_code, coe_std_code, coe_qte_code, coe_date_debut, coe_date_fin, coe_org_code);


--
-- Name: c_pk_dft; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_dfesttype_dft
    ADD CONSTRAINT c_pk_dft PRIMARY KEY (dft_df_identifiant, dft_tdf_code, dft_rang, dft_org_code);


--
-- Name: c_pk_dic; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_pk_dic PRIMARY KEY (dic_code, dic_org_code);


--
-- Name: c_pk_dif; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_pk_dif PRIMARY KEY (dif_code, dif_org_code);


--
-- Name: c_pk_dis; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tg_dispositif_dis
    ADD CONSTRAINT c_pk_dis PRIMARY KEY (dis_identifiant, dis_org_code);


--
-- Name: c_pk_dtx; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_dfestdestinea_dtx
    ADD CONSTRAINT c_pk_dtx PRIMARY KEY (dtx_dif_identifiant, dtx_tax_code, dtx_org_code);


--
-- Name: c_pk_env; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_conditionenvironnementale_env
    ADD CONSTRAINT c_pk_env PRIMARY KEY (env_stm_identifiant, env_date_debut, env_date_fin, env_org_code);


--
-- Name: c_pk_lot; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_lot_lot
    ADD CONSTRAINT c_pk_lot PRIMARY KEY (lot_identifiant, lot_org_code);


--
-- Name: c_pk_mac; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masquecaracteristiquelot_mac
    ADD CONSTRAINT c_pk_mac PRIMARY KEY (mac_mal_id, mac_par_code);


--
-- Name: c_pk_mae; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_masqueconditionsenvironnementales_mae
    ADD CONSTRAINT c_pk_mae PRIMARY KEY (mae_mao_id, mae_stm_identifiant);


--
-- Name: c_pk_mal_mas_id; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_masquelot_mal
    ADD CONSTRAINT c_pk_mal_mas_id PRIMARY KEY (mal_mas_id);


--
-- Name: c_pk_mao_mas_id; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_masqueope_mao
    ADD CONSTRAINT c_pk_mao_mas_id PRIMARY KEY (mao_mas_id);


--
-- Name: c_pk_mas_id; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masque_mas
    ADD CONSTRAINT c_pk_mas_id PRIMARY KEY (mas_id);


--
-- Name: c_pk_mqe; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_marque_mqe
    ADD CONSTRAINT c_pk_mqe PRIMARY KEY (mqe_reference, mqe_nmq_code, mqe_org_code);


--
-- Name: c_pk_omq; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_operationmarquage_omq
    ADD CONSTRAINT c_pk_omq PRIMARY KEY (omq_reference, omq_org_code);


--
-- Name: c_pk_ope; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_operation_ope
    ADD CONSTRAINT c_pk_ope PRIMARY KEY (ope_identifiant, ope_org_code);


--
-- Name: c_pk_ouv; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_ouvrage_ouv
    ADD CONSTRAINT c_pk_ouv PRIMARY KEY (ouv_identifiant, ouv_org_code);


--
-- Name: c_pk_pco; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_pathologieconstatee_pco
    ADD CONSTRAINT c_pk_pco PRIMARY KEY (pco_lot_identifiant, pco_pat_code, pco_loc_code, pco_org_code);


--
-- Name: c_pk_per; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_periodefonctdispositif_per
    ADD CONSTRAINT c_pk_per PRIMARY KEY (per_dis_identifiant, per_org_code, per_date_debut, per_date_fin);


--
-- Name: c_pk_prl; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_prelevementlot_prl
    ADD CONSTRAINT c_pk_prl PRIMARY KEY (prl_pre_typeprelevement, prl_lot_identifiant, prl_code, prl_org_code);


--
-- Name: c_pk_sta; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_station_sta
    ADD CONSTRAINT c_pk_sta PRIMARY KEY (sta_code, sta_org_code);


--
-- Name: c_pk_stm; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_pk_stm PRIMARY KEY (stm_identifiant, stm_org_code);


--
-- Name: c_pk_tav; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_taillevideo_tav
    ADD CONSTRAINT c_pk_tav PRIMARY KEY (tav_distance, tav_dic_identifiant, tav_org_code);


--
-- Name: c_pk_txe; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_tauxechappement_txe
    ADD CONSTRAINT c_pk_txe PRIMARY KEY (txe_sta_code, txe_org_code, txe_tax_code, txe_std_code, txe_date_debut, txe_date_fin);


--
-- Name: c_pk_txv; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_taxonvideo_txv
    ADD CONSTRAINT c_pk_txv PRIMARY KEY (txv_code, txv_org_code);


--
-- Name: c_uk_dic_dis_identifiant; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_uk_dic_dis_identifiant UNIQUE (dic_dis_identifiant, dic_org_code);


--
-- Name: c_uk_dif_dis_identifiant; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_uk_dif_dis_identifiant UNIQUE (dif_dis_identifiant, dif_org_code);


--
-- Name: c_uk_maa; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa
    ADD CONSTRAINT c_uk_maa UNIQUE (maa_mal_id, maa_table, maa_valeur, maa_champdumasque);


--
-- Name: c_uk_main_ticket; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_maintenance_main
    ADD CONSTRAINT c_uk_main_ticket UNIQUE (main_ticket);


--
-- Name: c_uk_prl_code; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY tj_prelevementlot_prl
    ADD CONSTRAINT c_uk_prl_code UNIQUE (prl_code);


--
-- Name: c_uk_stm; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_uk_stm UNIQUE (stm_libelle);


--
-- Name: c_uk_stm_identifiant; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_uk_stm_identifiant UNIQUE (stm_identifiant);


--
-- Name: c_uk_stm_libelle; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_uk_stm_libelle UNIQUE (stm_libelle);


--
-- Name: c_uq_dic; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_uq_dic UNIQUE (dic_dif_identifiant, dic_code);


--
-- Name: c_uq_dif; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_uq_dif UNIQUE (dif_ouv_identifiant, dif_code);


--
-- Name: c_uq_ope; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_operation_ope
    ADD CONSTRAINT c_uq_ope UNIQUE (ope_dic_identifiant, ope_date_debut, ope_date_fin);


--
-- Name: c_uq_ouv; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY t_ouvrage_ouv
    ADD CONSTRAINT c_uq_ouv UNIQUE (ouv_sta_code, ouv_code);


--
-- Name: c_uq_sta_nom; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY t_station_sta
    ADD CONSTRAINT c_uq_sta_nom UNIQUE (sta_nom);


--
-- Name: ts_maintenance_main_pkey; Type: CONSTRAINT; Schema: user_2; Owner: postgres; Tablespace: 
--

ALTER TABLE ONLY ts_maintenance_main
    ADD CONSTRAINT ts_maintenance_main_pkey PRIMARY KEY (main_identifiant);


--
-- Name: ts_masque_mas_mas_code_key; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masque_mas
    ADD CONSTRAINT ts_masque_mas_mas_code_key UNIQUE (mas_code);


--
-- Name: ts_masqueordreaffichage_maa_pkey; Type: CONSTRAINT; Schema: user_2; Owner: iav; Tablespace: 
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa
    ADD CONSTRAINT ts_masqueordreaffichage_maa_pkey PRIMARY KEY (maa_id);


SET search_path = nat, pg_catalog;

--
-- Name: i_car_lotETpar; Type: INDEX; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE INDEX "i_car_lotETpar" ON tj_caracteristiquelot_car USING btree (car_lot_identifiant, car_par_code);


--
-- Name: i_env_stm_identifiant; Type: INDEX; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE INDEX i_env_stm_identifiant ON tj_conditionenvironnementale_env USING btree (env_stm_identifiant);


--
-- Name: i_lot_identifiant; Type: INDEX; Schema: nat; Owner: nat; Tablespace: 
--

CREATE INDEX i_lot_identifiant ON t_lot_lot USING btree (lot_identifiant);


--
-- Name: i_lot_lot_identifiant; Type: INDEX; Schema: nat; Owner: nat; Tablespace: 
--

CREATE INDEX i_lot_lot_identifiant ON t_lot_lot USING btree (lot_lot_identifiant);


--
-- Name: i_lot_ope_identifiant; Type: INDEX; Schema: nat; Owner: nat; Tablespace: 
--

CREATE INDEX i_lot_ope_identifiant ON t_lot_lot USING btree (lot_ope_identifiant);


--
-- Name: i_lot_taxETstd_code; Type: INDEX; Schema: nat; Owner: nat; Tablespace: 
--

CREATE INDEX "i_lot_taxETstd_code" ON t_lot_lot USING btree (lot_tax_code, lot_std_code);


--
-- Name: i_ope_date; Type: INDEX; Schema: nat; Owner: nat; Tablespace: 
--

CREATE INDEX i_ope_date ON t_operation_ope USING btree (ope_date_debut, ope_date_fin);


--
-- Name: i_ope_identifiant; Type: INDEX; Schema: nat; Owner: nat; Tablespace: 
--

CREATE INDEX i_ope_identifiant ON t_operation_ope USING btree (ope_identifiant);


--
-- Name: i_per_dates; Type: INDEX; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE INDEX i_per_dates ON t_periodefonctdispositif_per USING btree (per_date_debut, per_date_fin);


--
-- Name: i_txv_code; Type: INDEX; Schema: nat; Owner: postgres; Tablespace: 
--

CREATE INDEX i_txv_code ON ts_taxonvideo_txv USING btree (txv_code);


SET search_path = ref, pg_catalog;

--
-- Name: i_tax_code; Type: INDEX; Schema: ref; Owner: postgres; Tablespace: 
--

CREATE INDEX i_tax_code ON tr_taxon_tax USING btree (tax_code);


SET search_path = user_1, pg_catalog;

--
-- Name: i_car_lotETpar; Type: INDEX; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE INDEX "i_car_lotETpar" ON tj_caracteristiquelot_car USING btree (car_lot_identifiant, car_par_code);


--
-- Name: i_lot_identifiant; Type: INDEX; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE INDEX i_lot_identifiant ON t_lot_lot USING btree (lot_identifiant);


--
-- Name: i_lot_lot_identifiant; Type: INDEX; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE INDEX i_lot_lot_identifiant ON t_lot_lot USING btree (lot_lot_identifiant);


--
-- Name: i_lot_ope_identifiant; Type: INDEX; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE INDEX i_lot_ope_identifiant ON t_lot_lot USING btree (lot_ope_identifiant);


--
-- Name: i_lot_taxETstd_code; Type: INDEX; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE INDEX "i_lot_taxETstd_code" ON t_lot_lot USING btree (lot_tax_code, lot_std_code);


--
-- Name: i_ope_date; Type: INDEX; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE INDEX i_ope_date ON t_operation_ope USING btree (ope_date_debut, ope_date_fin);


--
-- Name: i_ope_identifiant; Type: INDEX; Schema: user_1; Owner: iav; Tablespace: 
--

CREATE INDEX i_ope_identifiant ON t_operation_ope USING btree (ope_identifiant);


--
-- Name: i_per_dates; Type: INDEX; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE INDEX i_per_dates ON t_periodefonctdispositif_per USING btree (per_date_debut, per_date_fin);


--
-- Name: i_txv_code; Type: INDEX; Schema: user_1; Owner: postgres; Tablespace: 
--

CREATE INDEX i_txv_code ON ts_taxonvideo_txv USING btree (txv_code);


SET search_path = user_2, pg_catalog;

--
-- Name: i_car_lotETpar; Type: INDEX; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE INDEX "i_car_lotETpar" ON tj_caracteristiquelot_car USING btree (car_lot_identifiant, car_par_code);


--
-- Name: i_lot_identifiant; Type: INDEX; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE INDEX i_lot_identifiant ON t_lot_lot USING btree (lot_identifiant);


--
-- Name: i_lot_lot_identifiant; Type: INDEX; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE INDEX i_lot_lot_identifiant ON t_lot_lot USING btree (lot_lot_identifiant);


--
-- Name: i_lot_ope_identifiant; Type: INDEX; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE INDEX i_lot_ope_identifiant ON t_lot_lot USING btree (lot_ope_identifiant);


--
-- Name: i_lot_taxETstd_code; Type: INDEX; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE INDEX "i_lot_taxETstd_code" ON t_lot_lot USING btree (lot_tax_code, lot_std_code);


--
-- Name: i_ope_date; Type: INDEX; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE INDEX i_ope_date ON t_operation_ope USING btree (ope_date_debut, ope_date_fin);


--
-- Name: i_ope_identifiant; Type: INDEX; Schema: user_2; Owner: iav; Tablespace: 
--

CREATE INDEX i_ope_identifiant ON t_operation_ope USING btree (ope_identifiant);


--
-- Name: i_per_dates; Type: INDEX; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE INDEX i_per_dates ON t_periodefonctdispositif_per USING btree (per_date_debut, per_date_fin);


--
-- Name: i_txv_code; Type: INDEX; Schema: user_2; Owner: postgres; Tablespace: 
--

CREATE INDEX i_txv_code ON ts_taxonvideo_txv USING btree (txv_code);


SET search_path = nat, pg_catalog;

--
-- Name: check_do_not_delete_defaut; Type: TRIGGER; Schema: nat; Owner: nat
--

CREATE TRIGGER check_do_not_delete_defaut BEFORE DELETE ON ts_masque_mas FOR EACH ROW EXECUTE PROCEDURE do_not_delete_default();


--
-- Name: check_do_not_update_defaut; Type: TRIGGER; Schema: nat; Owner: nat
--

CREATE TRIGGER check_do_not_update_defaut BEFORE UPDATE ON ts_masque_mas FOR EACH ROW EXECUTE PROCEDURE do_not_update_default();


--
-- Name: trg_coe_date; Type: TRIGGER; Schema: nat; Owner: postgres
--

CREATE TRIGGER trg_coe_date AFTER INSERT OR UPDATE ON tj_coefficientconversion_coe FOR EACH ROW EXECUTE PROCEDURE fct_coe_date();


--
-- Name: trg_masqueope; Type: TRIGGER; Schema: nat; Owner: postgres
--

CREATE TRIGGER trg_masqueope BEFORE INSERT OR UPDATE ON ts_masqueope_mao FOR EACH ROW EXECUTE PROCEDURE masqueope();


--
-- Name: trg_per_date; Type: TRIGGER; Schema: nat; Owner: postgres
--

CREATE TRIGGER trg_per_date AFTER INSERT OR UPDATE ON t_periodefonctdispositif_per FOR EACH ROW EXECUTE PROCEDURE fct_per_date();


--
-- Name: trg_txe_date; Type: TRIGGER; Schema: nat; Owner: postgres
--

CREATE TRIGGER trg_txe_date AFTER INSERT OR UPDATE ON tj_tauxechappement_txe FOR EACH ROW EXECUTE PROCEDURE fct_txe_date();


SET search_path = user_1, pg_catalog;

--
-- Name: check_do_not_delete_defaut; Type: TRIGGER; Schema: user_1; Owner: iav
--

CREATE TRIGGER check_do_not_delete_defaut BEFORE DELETE ON ts_masque_mas FOR EACH ROW EXECUTE PROCEDURE do_not_delete_default();


--
-- Name: check_do_not_update_defaut; Type: TRIGGER; Schema: user_1; Owner: iav
--

CREATE TRIGGER check_do_not_update_defaut BEFORE UPDATE ON ts_masque_mas FOR EACH ROW EXECUTE PROCEDURE do_not_update_default();


--
-- Name: trg_coe_date; Type: TRIGGER; Schema: user_1; Owner: postgres
--

CREATE TRIGGER trg_coe_date AFTER INSERT OR UPDATE ON tj_coefficientconversion_coe FOR EACH ROW EXECUTE PROCEDURE fct_coe_date();


--
-- Name: trg_masqueope; Type: TRIGGER; Schema: user_1; Owner: postgres
--

CREATE TRIGGER trg_masqueope BEFORE INSERT OR UPDATE ON ts_masqueope_mao FOR EACH ROW EXECUTE PROCEDURE masqueope();


--
-- Name: trg_ope_date; Type: TRIGGER; Schema: user_1; Owner: iav
--

CREATE TRIGGER trg_ope_date AFTER INSERT OR UPDATE ON t_operation_ope FOR EACH ROW EXECUTE PROCEDURE fct_ope_date();


--
-- Name: trg_per_date; Type: TRIGGER; Schema: user_1; Owner: postgres
--

CREATE TRIGGER trg_per_date AFTER INSERT OR UPDATE ON t_periodefonctdispositif_per FOR EACH ROW EXECUTE PROCEDURE fct_per_date();


--
-- Name: trg_txe_date; Type: TRIGGER; Schema: user_1; Owner: postgres
--

CREATE TRIGGER trg_txe_date AFTER INSERT OR UPDATE ON tj_tauxechappement_txe FOR EACH ROW EXECUTE PROCEDURE fct_txe_date();


SET search_path = user_2, pg_catalog;

--
-- Name: check_do_not_delete_defaut; Type: TRIGGER; Schema: user_2; Owner: iav
--

CREATE TRIGGER check_do_not_delete_defaut BEFORE DELETE ON ts_masque_mas FOR EACH ROW EXECUTE PROCEDURE do_not_delete_default();


--
-- Name: check_do_not_update_defaut; Type: TRIGGER; Schema: user_2; Owner: iav
--

CREATE TRIGGER check_do_not_update_defaut BEFORE UPDATE ON ts_masque_mas FOR EACH ROW EXECUTE PROCEDURE do_not_update_default();


--
-- Name: trg_coe_date; Type: TRIGGER; Schema: user_2; Owner: postgres
--

CREATE TRIGGER trg_coe_date AFTER INSERT OR UPDATE ON tj_coefficientconversion_coe FOR EACH ROW EXECUTE PROCEDURE fct_coe_date();


--
-- Name: trg_masqueope; Type: TRIGGER; Schema: user_2; Owner: postgres
--

CREATE TRIGGER trg_masqueope BEFORE INSERT OR UPDATE ON ts_masqueope_mao FOR EACH ROW EXECUTE PROCEDURE masqueope();


--
-- Name: trg_ope_date; Type: TRIGGER; Schema: user_2; Owner: iav
--

CREATE TRIGGER trg_ope_date AFTER INSERT OR UPDATE ON t_operation_ope FOR EACH ROW EXECUTE PROCEDURE fct_ope_date();


--
-- Name: trg_per_date; Type: TRIGGER; Schema: user_2; Owner: postgres
--

CREATE TRIGGER trg_per_date AFTER INSERT OR UPDATE ON t_periodefonctdispositif_per FOR EACH ROW EXECUTE PROCEDURE fct_per_date();


--
-- Name: trg_txe_date; Type: TRIGGER; Schema: user_2; Owner: postgres
--

CREATE TRIGGER trg_txe_date AFTER INSERT OR UPDATE ON tj_tauxechappement_txe FOR EACH ROW EXECUTE PROCEDURE fct_txe_date();


SET search_path = nat, pg_catalog;

--
-- Name: c_fk_act_lot_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY tj_actionmarquage_act
    ADD CONSTRAINT c_fk_act_lot_identifiant FOREIGN KEY (act_lot_identifiant, act_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_act_mqe_reference; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY tj_actionmarquage_act
    ADD CONSTRAINT c_fk_act_mqe_reference FOREIGN KEY (act_mqe_reference, act_nmq_code, act_org_code) REFERENCES t_marque_mqe(mqe_reference, mqe_nmq_code, mqe_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_car_lot_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY tj_caracteristiquelot_car
    ADD CONSTRAINT c_fk_car_lot_identifiant FOREIGN KEY (car_lot_identifiant, car_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_dft_df_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY tj_dfesttype_dft
    ADD CONSTRAINT c_fk_dft_df_identifiant FOREIGN KEY (dft_df_identifiant, dft_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_dic_dif_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_fk_dic_dif_identifiant FOREIGN KEY (dic_dif_identifiant, dic_org_code) REFERENCES t_dispositiffranchissement_dif(dif_dis_identifiant, dif_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_dic_dis_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_fk_dic_dis_identifiant FOREIGN KEY (dic_dis_identifiant, dic_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dif_dis_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_fk_dif_dis_identifiant FOREIGN KEY (dif_dis_identifiant, dif_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_dif_ouv_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_fk_dif_ouv_identifiant FOREIGN KEY (dif_ouv_identifiant, dif_org_code) REFERENCES t_ouvrage_ouv(ouv_identifiant, ouv_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_dtx_dif_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY tj_dfestdestinea_dtx
    ADD CONSTRAINT c_fk_dtx_dif_identifiant FOREIGN KEY (dtx_dif_identifiant, dtx_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_env_stm_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY tj_conditionenvironnementale_env
    ADD CONSTRAINT c_fk_env_stm_identifiant FOREIGN KEY (env_stm_identifiant, env_org_code) REFERENCES tj_stationmesure_stm(stm_identifiant, stm_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_lot_lot_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY t_lot_lot
    ADD CONSTRAINT c_fk_lot_lot_identifiant FOREIGN KEY (lot_lot_identifiant, lot_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_lot_ope_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY t_lot_lot
    ADD CONSTRAINT c_fk_lot_ope_identifiant FOREIGN KEY (lot_ope_identifiant, lot_org_code) REFERENCES t_operation_ope(ope_identifiant, ope_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_maa_mal_id; Type: FK CONSTRAINT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa
    ADD CONSTRAINT c_fk_maa_mal_id FOREIGN KEY (maa_mal_id) REFERENCES ts_masquelot_mal(mal_mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mae_mao_id; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY ts_masqueconditionsenvironnementales_mae
    ADD CONSTRAINT c_fk_mae_mao_id FOREIGN KEY (mae_mao_id) REFERENCES ts_masqueope_mao(mao_mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mal_mas_id; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY ts_masquelot_mal
    ADD CONSTRAINT c_fk_mal_mas_id FOREIGN KEY (mal_mas_id) REFERENCES ts_masque_mas(mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mal_mas_id; Type: FK CONSTRAINT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY ts_masquecaracteristiquelot_mac
    ADD CONSTRAINT c_fk_mal_mas_id FOREIGN KEY (mac_mal_id) REFERENCES ts_masquelot_mal(mal_mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mao_mas_id; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY ts_masqueope_mao
    ADD CONSTRAINT c_fk_mao_mas_id FOREIGN KEY (mao_mas_id) REFERENCES ts_masque_mas(mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mqe_omq_reference; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY t_marque_mqe
    ADD CONSTRAINT c_fk_mqe_omq_reference FOREIGN KEY (mqe_omq_reference, mqe_org_code) REFERENCES t_operationmarquage_omq(omq_reference, omq_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_ope_dic_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY t_operation_ope
    ADD CONSTRAINT c_fk_ope_dic_identifiant FOREIGN KEY (ope_dic_identifiant, ope_org_code) REFERENCES t_dispositifcomptage_dic(dic_dis_identifiant, dic_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_ouv_sta_code; Type: FK CONSTRAINT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY t_ouvrage_ouv
    ADD CONSTRAINT c_fk_ouv_sta_code FOREIGN KEY (ouv_sta_code, ouv_org_code) REFERENCES t_station_sta(sta_code, sta_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_pco_lot_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY tj_pathologieconstatee_pco
    ADD CONSTRAINT c_fk_pco_lot_identifiant FOREIGN KEY (pco_lot_identifiant, pco_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_per_dis_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY t_periodefonctdispositif_per
    ADD CONSTRAINT c_fk_per_dis_identifiant FOREIGN KEY (per_dis_identifiant, per_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_prl_lot_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY tj_prelevementlot_prl
    ADD CONSTRAINT c_fk_prl_lot_identifiant FOREIGN KEY (prl_lot_identifiant, prl_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_stm_sta_code; Type: FK CONSTRAINT; Schema: nat; Owner: nat
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_fk_stm_sta_code FOREIGN KEY (stm_sta_code, stm_org_code) REFERENCES t_station_sta(sta_code, sta_org_code) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: c_fk_tav_dic_identifiant; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY ts_taillevideo_tav
    ADD CONSTRAINT c_fk_tav_dic_identifiant FOREIGN KEY (tav_dic_identifiant, tav_org_code) REFERENCES t_dispositifcomptage_dic(dic_dis_identifiant, dic_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_txe_sta_code; Type: FK CONSTRAINT; Schema: nat; Owner: postgres
--

ALTER TABLE ONLY tj_tauxechappement_txe
    ADD CONSTRAINT c_fk_txe_sta_code FOREIGN KEY (txe_sta_code, txe_org_code) REFERENCES t_station_sta(sta_code, sta_org_code) ON UPDATE CASCADE;


SET search_path = ref, pg_catalog;

--
-- Name: c_fk_mrl_msr_id; Type: FK CONSTRAINT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY ts_messagerlang_mrl
    ADD CONSTRAINT c_fk_mrl_msr_id FOREIGN KEY (mrl_msr_id) REFERENCES ts_messager_msr(msr_id);


--
-- Name: c_fk_qal_par_code; Type: FK CONSTRAINT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY tr_parametrequalitatif_qal
    ADD CONSTRAINT c_fk_qal_par_code FOREIGN KEY (qal_par_code) REFERENCES tg_parametre_par(par_code);


--
-- Name: c_fk_qan_par_code; Type: FK CONSTRAINT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY tr_parametrequantitatif_qan
    ADD CONSTRAINT c_fk_qan_par_code FOREIGN KEY (qan_par_code) REFERENCES tg_parametre_par(par_code);


--
-- Name: c_fk_tax_ntx_code; Type: FK CONSTRAINT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY tr_taxon_tax
    ADD CONSTRAINT c_fk_tax_ntx_code FOREIGN KEY (tax_ntx_code) REFERENCES tr_niveautaxonomique_ntx(ntx_code);


--
-- Name: c_fk_tax_tax_tax_code; Type: FK CONSTRAINT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY tr_taxon_tax
    ADD CONSTRAINT c_fk_tax_tax_tax_code FOREIGN KEY (tax_tax_code) REFERENCES tr_taxon_tax(tax_code);


--
-- Name: c_fk_val_qal_code; Type: FK CONSTRAINT; Schema: ref; Owner: postgres
--

ALTER TABLE ONLY tr_valeurparametrequalitatif_val
    ADD CONSTRAINT c_fk_val_qal_code FOREIGN KEY (val_qal_code) REFERENCES tr_parametrequalitatif_qal(qal_par_code);


SET search_path = user_1, pg_catalog;

--
-- Name: c_fk_act_lot_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY tj_actionmarquage_act
    ADD CONSTRAINT c_fk_act_lot_identifiant FOREIGN KEY (act_lot_identifiant, act_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_act_mqe_reference; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY tj_actionmarquage_act
    ADD CONSTRAINT c_fk_act_mqe_reference FOREIGN KEY (act_mqe_reference, act_nmq_code, act_org_code) REFERENCES t_marque_mqe(mqe_reference, mqe_nmq_code, mqe_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_car_lot_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY tj_caracteristiquelot_car
    ADD CONSTRAINT c_fk_car_lot_identifiant FOREIGN KEY (car_lot_identifiant, car_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dft_df_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY tj_dfesttype_dft
    ADD CONSTRAINT c_fk_dft_df_identifiant FOREIGN KEY (dft_df_identifiant, dft_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dic_dif_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_fk_dic_dif_identifiant FOREIGN KEY (dic_dif_identifiant, dic_org_code) REFERENCES t_dispositiffranchissement_dif(dif_dis_identifiant, dif_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dic_dis_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_fk_dic_dis_identifiant FOREIGN KEY (dic_dis_identifiant, dic_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dif_dis_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_fk_dif_dis_identifiant FOREIGN KEY (dif_dis_identifiant, dif_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dif_ouv_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_fk_dif_ouv_identifiant FOREIGN KEY (dif_ouv_identifiant, dif_org_code) REFERENCES t_ouvrage_ouv(ouv_identifiant, ouv_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dtx_dif_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY tj_dfestdestinea_dtx
    ADD CONSTRAINT c_fk_dtx_dif_identifiant FOREIGN KEY (dtx_dif_identifiant, dtx_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_env_stm_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY tj_conditionenvironnementale_env
    ADD CONSTRAINT c_fk_env_stm_identifiant FOREIGN KEY (env_stm_identifiant, env_org_code) REFERENCES tj_stationmesure_stm(stm_identifiant, stm_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_lot_lot_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY t_lot_lot
    ADD CONSTRAINT c_fk_lot_lot_identifiant FOREIGN KEY (lot_lot_identifiant, lot_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_lot_ope_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY t_lot_lot
    ADD CONSTRAINT c_fk_lot_ope_identifiant FOREIGN KEY (lot_ope_identifiant, lot_org_code) REFERENCES t_operation_ope(ope_identifiant, ope_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_maa_mal_id; Type: FK CONSTRAINT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa
    ADD CONSTRAINT c_fk_maa_mal_id FOREIGN KEY (maa_mal_id) REFERENCES ts_masquelot_mal(mal_mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mae_mao_id; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY ts_masqueconditionsenvironnementales_mae
    ADD CONSTRAINT c_fk_mae_mao_id FOREIGN KEY (mae_mao_id) REFERENCES ts_masqueope_mao(mao_mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mae_stm_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY ts_masqueconditionsenvironnementales_mae
    ADD CONSTRAINT c_fk_mae_stm_identifiant FOREIGN KEY (mae_stm_identifiant) REFERENCES tj_stationmesure_stm(stm_identifiant);


--
-- Name: c_fk_mal_mas_id; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY ts_masquelot_mal
    ADD CONSTRAINT c_fk_mal_mas_id FOREIGN KEY (mal_mas_id) REFERENCES ts_masque_mas(mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mal_mas_id; Type: FK CONSTRAINT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY ts_masquecaracteristiquelot_mac
    ADD CONSTRAINT c_fk_mal_mas_id FOREIGN KEY (mac_mal_id) REFERENCES ts_masquelot_mal(mal_mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mao_mas_id; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY ts_masqueope_mao
    ADD CONSTRAINT c_fk_mao_mas_id FOREIGN KEY (mao_mas_id) REFERENCES ts_masque_mas(mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mqe_omq_reference; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY t_marque_mqe
    ADD CONSTRAINT c_fk_mqe_omq_reference FOREIGN KEY (mqe_omq_reference, mqe_org_code) REFERENCES t_operationmarquage_omq(omq_reference, omq_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_ope_dic_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY t_operation_ope
    ADD CONSTRAINT c_fk_ope_dic_identifiant FOREIGN KEY (ope_dic_identifiant, ope_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_ouv_sta_code; Type: FK CONSTRAINT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY t_ouvrage_ouv
    ADD CONSTRAINT c_fk_ouv_sta_code FOREIGN KEY (ouv_sta_code, ouv_org_code) REFERENCES t_station_sta(sta_code, sta_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_pco_lot_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY tj_pathologieconstatee_pco
    ADD CONSTRAINT c_fk_pco_lot_identifiant FOREIGN KEY (pco_lot_identifiant, pco_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_per_dis_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY t_periodefonctdispositif_per
    ADD CONSTRAINT c_fk_per_dis_identifiant FOREIGN KEY (per_dis_identifiant, per_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_prl_lot_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY tj_prelevementlot_prl
    ADD CONSTRAINT c_fk_prl_lot_identifiant FOREIGN KEY (prl_lot_identifiant, prl_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_stm_sta_code; Type: FK CONSTRAINT; Schema: user_1; Owner: iav
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_fk_stm_sta_code FOREIGN KEY (stm_sta_code, stm_org_code) REFERENCES t_station_sta(sta_code, sta_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_tav_dic_identifiant; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY ts_taillevideo_tav
    ADD CONSTRAINT c_fk_tav_dic_identifiant FOREIGN KEY (tav_dic_identifiant, tav_org_code) REFERENCES t_dispositifcomptage_dic(dic_dis_identifiant, dic_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_txe_sta_code; Type: FK CONSTRAINT; Schema: user_1; Owner: postgres
--

ALTER TABLE ONLY tj_tauxechappement_txe
    ADD CONSTRAINT c_fk_txe_sta_code FOREIGN KEY (txe_sta_code, txe_org_code) REFERENCES t_station_sta(sta_code, sta_org_code) ON UPDATE CASCADE;


SET search_path = user_2, pg_catalog;

--
-- Name: c_fk_act_lot_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY tj_actionmarquage_act
    ADD CONSTRAINT c_fk_act_lot_identifiant FOREIGN KEY (act_lot_identifiant, act_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_act_mqe_reference; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY tj_actionmarquage_act
    ADD CONSTRAINT c_fk_act_mqe_reference FOREIGN KEY (act_mqe_reference, act_nmq_code, act_org_code) REFERENCES t_marque_mqe(mqe_reference, mqe_nmq_code, mqe_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_car_lot_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY tj_caracteristiquelot_car
    ADD CONSTRAINT c_fk_car_lot_identifiant FOREIGN KEY (car_lot_identifiant, car_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dft_df_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY tj_dfesttype_dft
    ADD CONSTRAINT c_fk_dft_df_identifiant FOREIGN KEY (dft_df_identifiant, dft_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dic_dif_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_fk_dic_dif_identifiant FOREIGN KEY (dic_dif_identifiant, dic_org_code) REFERENCES t_dispositiffranchissement_dif(dif_dis_identifiant, dif_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dic_dis_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY t_dispositifcomptage_dic
    ADD CONSTRAINT c_fk_dic_dis_identifiant FOREIGN KEY (dic_dis_identifiant, dic_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dif_dis_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_fk_dif_dis_identifiant FOREIGN KEY (dif_dis_identifiant, dif_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dif_ouv_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY t_dispositiffranchissement_dif
    ADD CONSTRAINT c_fk_dif_ouv_identifiant FOREIGN KEY (dif_ouv_identifiant, dif_org_code) REFERENCES t_ouvrage_ouv(ouv_identifiant, ouv_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_dtx_dif_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY tj_dfestdestinea_dtx
    ADD CONSTRAINT c_fk_dtx_dif_identifiant FOREIGN KEY (dtx_dif_identifiant, dtx_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_env_stm_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY tj_conditionenvironnementale_env
    ADD CONSTRAINT c_fk_env_stm_identifiant FOREIGN KEY (env_stm_identifiant, env_org_code) REFERENCES tj_stationmesure_stm(stm_identifiant, stm_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_lot_lot_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY t_lot_lot
    ADD CONSTRAINT c_fk_lot_lot_identifiant FOREIGN KEY (lot_lot_identifiant, lot_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_lot_ope_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY t_lot_lot
    ADD CONSTRAINT c_fk_lot_ope_identifiant FOREIGN KEY (lot_ope_identifiant, lot_org_code) REFERENCES t_operation_ope(ope_identifiant, ope_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_maa_mal_id; Type: FK CONSTRAINT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY ts_masqueordreaffichage_maa
    ADD CONSTRAINT c_fk_maa_mal_id FOREIGN KEY (maa_mal_id) REFERENCES ts_masquelot_mal(mal_mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mae_mao_id; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY ts_masqueconditionsenvironnementales_mae
    ADD CONSTRAINT c_fk_mae_mao_id FOREIGN KEY (mae_mao_id) REFERENCES ts_masqueope_mao(mao_mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mae_stm_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY ts_masqueconditionsenvironnementales_mae
    ADD CONSTRAINT c_fk_mae_stm_identifiant FOREIGN KEY (mae_stm_identifiant) REFERENCES tj_stationmesure_stm(stm_identifiant);


--
-- Name: c_fk_mal_mas_id; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY ts_masquelot_mal
    ADD CONSTRAINT c_fk_mal_mas_id FOREIGN KEY (mal_mas_id) REFERENCES ts_masque_mas(mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mal_mas_id; Type: FK CONSTRAINT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY ts_masquecaracteristiquelot_mac
    ADD CONSTRAINT c_fk_mal_mas_id FOREIGN KEY (mac_mal_id) REFERENCES ts_masquelot_mal(mal_mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mao_mas_id; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY ts_masqueope_mao
    ADD CONSTRAINT c_fk_mao_mas_id FOREIGN KEY (mao_mas_id) REFERENCES ts_masque_mas(mas_id) ON DELETE CASCADE;


--
-- Name: c_fk_mqe_omq_reference; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY t_marque_mqe
    ADD CONSTRAINT c_fk_mqe_omq_reference FOREIGN KEY (mqe_omq_reference, mqe_org_code) REFERENCES t_operationmarquage_omq(omq_reference, omq_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_ope_dic_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY t_operation_ope
    ADD CONSTRAINT c_fk_ope_dic_identifiant FOREIGN KEY (ope_dic_identifiant, ope_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_ouv_sta_code; Type: FK CONSTRAINT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY t_ouvrage_ouv
    ADD CONSTRAINT c_fk_ouv_sta_code FOREIGN KEY (ouv_sta_code, ouv_org_code) REFERENCES t_station_sta(sta_code, sta_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_pco_lot_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY tj_pathologieconstatee_pco
    ADD CONSTRAINT c_fk_pco_lot_identifiant FOREIGN KEY (pco_lot_identifiant, pco_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_per_dis_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY t_periodefonctdispositif_per
    ADD CONSTRAINT c_fk_per_dis_identifiant FOREIGN KEY (per_dis_identifiant, per_org_code) REFERENCES tg_dispositif_dis(dis_identifiant, dis_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_prl_lot_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY tj_prelevementlot_prl
    ADD CONSTRAINT c_fk_prl_lot_identifiant FOREIGN KEY (prl_lot_identifiant, prl_org_code) REFERENCES t_lot_lot(lot_identifiant, lot_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_stm_sta_code; Type: FK CONSTRAINT; Schema: user_2; Owner: iav
--

ALTER TABLE ONLY tj_stationmesure_stm
    ADD CONSTRAINT c_fk_stm_sta_code FOREIGN KEY (stm_sta_code, stm_org_code) REFERENCES t_station_sta(sta_code, sta_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_tav_dic_identifiant; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY ts_taillevideo_tav
    ADD CONSTRAINT c_fk_tav_dic_identifiant FOREIGN KEY (tav_dic_identifiant, tav_org_code) REFERENCES t_dispositifcomptage_dic(dic_dis_identifiant, dic_org_code) ON UPDATE CASCADE;


--
-- Name: c_fk_txe_sta_code; Type: FK CONSTRAINT; Schema: user_2; Owner: postgres
--

ALTER TABLE ONLY tj_tauxechappement_txe
    ADD CONSTRAINT c_fk_txe_sta_code FOREIGN KEY (txe_sta_code, txe_org_code) REFERENCES t_station_sta(sta_code, sta_org_code) ON UPDATE CASCADE;


--
-- Name: nat; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA nat FROM PUBLIC;
REVOKE ALL ON SCHEMA nat FROM postgres;
GRANT ALL ON SCHEMA nat TO postgres;
GRANT ALL ON SCHEMA nat TO invite;
GRANT ALL ON SCHEMA nat TO nat;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- Name: ref; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA ref FROM PUBLIC;
REVOKE ALL ON SCHEMA ref FROM postgres;
GRANT ALL ON SCHEMA ref TO postgres;
GRANT ALL ON SCHEMA ref TO PUBLIC;


--
-- Name: user_1; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA user_1 FROM PUBLIC;
REVOKE ALL ON SCHEMA user_1 FROM postgres;
GRANT ALL ON SCHEMA user_1 TO postgres;
GRANT ALL ON SCHEMA user_1 TO PUBLIC;
GRANT ALL ON SCHEMA user_1 TO iav;


--
-- Name: user_2; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA user_2 FROM PUBLIC;
REVOKE ALL ON SCHEMA user_2 FROM postgres;
GRANT ALL ON SCHEMA user_2 TO postgres;
GRANT ALL ON SCHEMA user_2 TO PUBLIC;
GRANT ALL ON SCHEMA user_2 TO iav;


SET search_path = user_1, pg_catalog;

--
-- Name: breakpoint; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TYPE breakpoint FROM PUBLIC;
REVOKE ALL ON TYPE breakpoint FROM postgres;
GRANT ALL ON TYPE breakpoint TO PUBLIC;


--
-- Name: dblink_pkey_results; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TYPE dblink_pkey_results FROM PUBLIC;
REVOKE ALL ON TYPE dblink_pkey_results FROM postgres;
GRANT ALL ON TYPE dblink_pkey_results TO PUBLIC;


--
-- Name: frame; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TYPE frame FROM PUBLIC;
REVOKE ALL ON TYPE frame FROM postgres;
GRANT ALL ON TYPE frame TO PUBLIC;


--
-- Name: proxyinfo; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TYPE proxyinfo FROM PUBLIC;
REVOKE ALL ON TYPE proxyinfo FROM postgres;
GRANT ALL ON TYPE proxyinfo TO PUBLIC;


--
-- Name: targetinfo; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TYPE targetinfo FROM PUBLIC;
REVOKE ALL ON TYPE targetinfo FROM postgres;
GRANT ALL ON TYPE targetinfo TO PUBLIC;


--
-- Name: var; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TYPE var FROM PUBLIC;
REVOKE ALL ON TYPE var FROM postgres;
GRANT ALL ON TYPE var TO PUBLIC;


SET search_path = user_2, pg_catalog;

--
-- Name: breakpoint; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TYPE breakpoint FROM PUBLIC;
REVOKE ALL ON TYPE breakpoint FROM postgres;
GRANT ALL ON TYPE breakpoint TO PUBLIC;


--
-- Name: dblink_pkey_results; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TYPE dblink_pkey_results FROM PUBLIC;
REVOKE ALL ON TYPE dblink_pkey_results FROM postgres;
GRANT ALL ON TYPE dblink_pkey_results TO PUBLIC;


--
-- Name: frame; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TYPE frame FROM PUBLIC;
REVOKE ALL ON TYPE frame FROM postgres;
GRANT ALL ON TYPE frame TO PUBLIC;


--
-- Name: proxyinfo; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TYPE proxyinfo FROM PUBLIC;
REVOKE ALL ON TYPE proxyinfo FROM postgres;
GRANT ALL ON TYPE proxyinfo TO PUBLIC;


--
-- Name: targetinfo; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TYPE targetinfo FROM PUBLIC;
REVOKE ALL ON TYPE targetinfo FROM postgres;
GRANT ALL ON TYPE targetinfo TO PUBLIC;


--
-- Name: var; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TYPE var FROM PUBLIC;
REVOKE ALL ON TYPE var FROM postgres;
GRANT ALL ON TYPE var TO PUBLIC;


SET search_path = public, pg_catalog;

--
-- Name: dblink_connect_u(text); Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON FUNCTION dblink_connect_u(text) FROM PUBLIC;
REVOKE ALL ON FUNCTION dblink_connect_u(text) FROM postgres;
GRANT ALL ON FUNCTION dblink_connect_u(text) TO postgres;


--
-- Name: dblink_connect_u(text, text); Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON FUNCTION dblink_connect_u(text, text) FROM PUBLIC;
REVOKE ALL ON FUNCTION dblink_connect_u(text, text) FROM postgres;
GRANT ALL ON FUNCTION dblink_connect_u(text, text) TO postgres;


SET search_path = nat, pg_catalog;

--
-- Name: serial_ann; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON SEQUENCE serial_ann FROM PUBLIC;
REVOKE ALL ON SEQUENCE serial_ann FROM postgres;
GRANT ALL ON SEQUENCE serial_ann TO postgres;
GRANT ALL ON SEQUENCE serial_ann TO nat;


--
-- Name: serial_sem; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON SEQUENCE serial_sem FROM PUBLIC;
REVOKE ALL ON SEQUENCE serial_sem FROM postgres;
GRANT ALL ON SEQUENCE serial_sem TO postgres;
GRANT ALL ON SEQUENCE serial_sem TO nat;


--
-- Name: t_bilanmigrationjournalier_bjo; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE t_bilanmigrationjournalier_bjo FROM PUBLIC;
REVOKE ALL ON TABLE t_bilanmigrationjournalier_bjo FROM nat;
GRANT ALL ON TABLE t_bilanmigrationjournalier_bjo TO nat;
GRANT SELECT ON TABLE t_bilanmigrationjournalier_bjo TO invite;


--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq FROM nat;
GRANT ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq TO nat;
GRANT SELECT,UPDATE ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq TO invite;


--
-- Name: t_bilanmigrationmensuel_bme; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE t_bilanmigrationmensuel_bme FROM PUBLIC;
REVOKE ALL ON TABLE t_bilanmigrationmensuel_bme FROM nat;
GRANT ALL ON TABLE t_bilanmigrationmensuel_bme TO nat;
GRANT SELECT ON TABLE t_bilanmigrationmensuel_bme TO invite;


--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq FROM nat;
GRANT ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq TO nat;
GRANT SELECT,UPDATE ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq TO invite;


--
-- Name: t_dispositifcomptage_dic; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE t_dispositifcomptage_dic FROM PUBLIC;
REVOKE ALL ON TABLE t_dispositifcomptage_dic FROM postgres;
GRANT ALL ON TABLE t_dispositifcomptage_dic TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_dispositifcomptage_dic TO nat;
GRANT SELECT ON TABLE t_dispositifcomptage_dic TO invite;


--
-- Name: t_dispositiffranchissement_dif; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE t_dispositiffranchissement_dif FROM PUBLIC;
REVOKE ALL ON TABLE t_dispositiffranchissement_dif FROM postgres;
GRANT ALL ON TABLE t_dispositiffranchissement_dif TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_dispositiffranchissement_dif TO nat;
GRANT SELECT ON TABLE t_dispositiffranchissement_dif TO invite;


--
-- Name: t_lot_lot; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE t_lot_lot FROM PUBLIC;
REVOKE ALL ON TABLE t_lot_lot FROM nat;
GRANT ALL ON TABLE t_lot_lot TO nat;
GRANT SELECT ON TABLE t_lot_lot TO invite;


--
-- Name: t_lot_lot_lot_identifiant_seq; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON SEQUENCE t_lot_lot_lot_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_lot_lot_lot_identifiant_seq FROM nat;
GRANT ALL ON SEQUENCE t_lot_lot_lot_identifiant_seq TO nat;
GRANT SELECT,UPDATE ON SEQUENCE t_lot_lot_lot_identifiant_seq TO invite;


--
-- Name: t_marque_mqe; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE t_marque_mqe FROM PUBLIC;
REVOKE ALL ON TABLE t_marque_mqe FROM postgres;
GRANT ALL ON TABLE t_marque_mqe TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_marque_mqe TO nat;
GRANT SELECT ON TABLE t_marque_mqe TO invite;


--
-- Name: t_operation_ope; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE t_operation_ope FROM PUBLIC;
REVOKE ALL ON TABLE t_operation_ope FROM nat;
GRANT ALL ON TABLE t_operation_ope TO nat;
GRANT SELECT ON TABLE t_operation_ope TO invite;


--
-- Name: t_operation_ope_ope_identifiant_seq; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON SEQUENCE t_operation_ope_ope_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_operation_ope_ope_identifiant_seq FROM nat;
GRANT ALL ON SEQUENCE t_operation_ope_ope_identifiant_seq TO nat;
GRANT SELECT,UPDATE ON SEQUENCE t_operation_ope_ope_identifiant_seq TO invite;


--
-- Name: t_operationmarquage_omq; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE t_operationmarquage_omq FROM PUBLIC;
REVOKE ALL ON TABLE t_operationmarquage_omq FROM postgres;
GRANT ALL ON TABLE t_operationmarquage_omq TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_operationmarquage_omq TO nat;
GRANT SELECT ON TABLE t_operationmarquage_omq TO invite;


--
-- Name: t_ouvrage_ouv; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE t_ouvrage_ouv FROM PUBLIC;
REVOKE ALL ON TABLE t_ouvrage_ouv FROM nat;
GRANT ALL ON TABLE t_ouvrage_ouv TO nat;
GRANT SELECT ON TABLE t_ouvrage_ouv TO invite;


--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq FROM nat;
GRANT ALL ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq TO nat;
GRANT SELECT,UPDATE ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq TO invite;


--
-- Name: t_periodefonctdispositif_per; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE t_periodefonctdispositif_per FROM PUBLIC;
REVOKE ALL ON TABLE t_periodefonctdispositif_per FROM postgres;
GRANT ALL ON TABLE t_periodefonctdispositif_per TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_periodefonctdispositif_per TO nat;
GRANT SELECT ON TABLE t_periodefonctdispositif_per TO invite;


--
-- Name: t_station_sta; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE t_station_sta FROM PUBLIC;
REVOKE ALL ON TABLE t_station_sta FROM postgres;
GRANT ALL ON TABLE t_station_sta TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_station_sta TO nat;
GRANT SELECT ON TABLE t_station_sta TO invite;


--
-- Name: tg_dispositif_dis; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE tg_dispositif_dis FROM PUBLIC;
REVOKE ALL ON TABLE tg_dispositif_dis FROM nat;
GRANT ALL ON TABLE tg_dispositif_dis TO nat;
GRANT SELECT ON TABLE tg_dispositif_dis TO invite;


--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq FROM nat;
GRANT ALL ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq TO nat;
GRANT SELECT,UPDATE ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq TO invite;


--
-- Name: tj_actionmarquage_act; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE tj_actionmarquage_act FROM PUBLIC;
REVOKE ALL ON TABLE tj_actionmarquage_act FROM postgres;
GRANT ALL ON TABLE tj_actionmarquage_act TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_actionmarquage_act TO nat;
GRANT SELECT ON TABLE tj_actionmarquage_act TO invite;


--
-- Name: tj_caracteristiquelot_car; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE tj_caracteristiquelot_car FROM PUBLIC;
REVOKE ALL ON TABLE tj_caracteristiquelot_car FROM postgres;
GRANT ALL ON TABLE tj_caracteristiquelot_car TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_caracteristiquelot_car TO nat;
GRANT SELECT ON TABLE tj_caracteristiquelot_car TO invite;


--
-- Name: tj_coefficientconversion_coe; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE tj_coefficientconversion_coe FROM PUBLIC;
REVOKE ALL ON TABLE tj_coefficientconversion_coe FROM postgres;
GRANT ALL ON TABLE tj_coefficientconversion_coe TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_coefficientconversion_coe TO nat;
GRANT SELECT ON TABLE tj_coefficientconversion_coe TO invite;


--
-- Name: tj_conditionenvironnementale_env; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE tj_conditionenvironnementale_env FROM PUBLIC;
REVOKE ALL ON TABLE tj_conditionenvironnementale_env FROM postgres;
GRANT ALL ON TABLE tj_conditionenvironnementale_env TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_conditionenvironnementale_env TO nat;
GRANT SELECT ON TABLE tj_conditionenvironnementale_env TO invite;


--
-- Name: tj_dfestdestinea_dtx; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE tj_dfestdestinea_dtx FROM PUBLIC;
REVOKE ALL ON TABLE tj_dfestdestinea_dtx FROM postgres;
GRANT ALL ON TABLE tj_dfestdestinea_dtx TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_dfestdestinea_dtx TO nat;
GRANT SELECT ON TABLE tj_dfestdestinea_dtx TO invite;


--
-- Name: tj_dfesttype_dft; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE tj_dfesttype_dft FROM PUBLIC;
REVOKE ALL ON TABLE tj_dfesttype_dft FROM postgres;
GRANT ALL ON TABLE tj_dfesttype_dft TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_dfesttype_dft TO nat;
GRANT SELECT ON TABLE tj_dfesttype_dft TO invite;


--
-- Name: tj_pathologieconstatee_pco; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE tj_pathologieconstatee_pco FROM PUBLIC;
REVOKE ALL ON TABLE tj_pathologieconstatee_pco FROM postgres;
GRANT ALL ON TABLE tj_pathologieconstatee_pco TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_pathologieconstatee_pco TO nat;
GRANT SELECT ON TABLE tj_pathologieconstatee_pco TO invite;


--
-- Name: tj_prelevementlot_prl; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE tj_prelevementlot_prl FROM PUBLIC;
REVOKE ALL ON TABLE tj_prelevementlot_prl FROM postgres;
GRANT ALL ON TABLE tj_prelevementlot_prl TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_prelevementlot_prl TO nat;
GRANT SELECT ON TABLE tj_prelevementlot_prl TO invite;


--
-- Name: tj_stationmesure_stm; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE tj_stationmesure_stm FROM PUBLIC;
REVOKE ALL ON TABLE tj_stationmesure_stm FROM nat;
GRANT ALL ON TABLE tj_stationmesure_stm TO nat;
GRANT SELECT ON TABLE tj_stationmesure_stm TO invite;


--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq FROM nat;
GRANT ALL ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq TO nat;
GRANT SELECT,UPDATE ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq TO invite;


--
-- Name: tj_tauxechappement_txe; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE tj_tauxechappement_txe FROM PUBLIC;
REVOKE ALL ON TABLE tj_tauxechappement_txe FROM postgres;
GRANT ALL ON TABLE tj_tauxechappement_txe TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_tauxechappement_txe TO nat;
GRANT SELECT ON TABLE tj_tauxechappement_txe TO invite;


--
-- Name: ts_maintenance_main; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE ts_maintenance_main FROM PUBLIC;
REVOKE ALL ON TABLE ts_maintenance_main FROM postgres;
GRANT ALL ON TABLE ts_maintenance_main TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_maintenance_main TO nat;


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq FROM postgres;
GRANT ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq TO postgres;
GRANT ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq TO nat;


--
-- Name: ts_masque_mas; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE ts_masque_mas FROM PUBLIC;
REVOKE ALL ON TABLE ts_masque_mas FROM nat;
GRANT ALL ON TABLE ts_masque_mas TO nat;
GRANT SELECT ON TABLE ts_masque_mas TO invite;


--
-- Name: ts_masquecaracteristiquelot_mac; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE ts_masquecaracteristiquelot_mac FROM PUBLIC;
REVOKE ALL ON TABLE ts_masquecaracteristiquelot_mac FROM nat;
GRANT ALL ON TABLE ts_masquecaracteristiquelot_mac TO nat;
GRANT SELECT ON TABLE ts_masquecaracteristiquelot_mac TO invite;


--
-- Name: ts_masqueconditionsenvironnementales_mae; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE ts_masqueconditionsenvironnementales_mae FROM PUBLIC;
REVOKE ALL ON TABLE ts_masqueconditionsenvironnementales_mae FROM postgres;
GRANT ALL ON TABLE ts_masqueconditionsenvironnementales_mae TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_masqueconditionsenvironnementales_mae TO nat;
GRANT SELECT ON TABLE ts_masqueconditionsenvironnementales_mae TO invite;


--
-- Name: ts_masquelot_mal; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE ts_masquelot_mal FROM PUBLIC;
REVOKE ALL ON TABLE ts_masquelot_mal FROM postgres;
GRANT ALL ON TABLE ts_masquelot_mal TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_masquelot_mal TO nat;
GRANT SELECT ON TABLE ts_masquelot_mal TO invite;


--
-- Name: ts_masqueope_mao; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE ts_masqueope_mao FROM PUBLIC;
REVOKE ALL ON TABLE ts_masqueope_mao FROM postgres;
GRANT ALL ON TABLE ts_masqueope_mao TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_masqueope_mao TO nat;
GRANT SELECT ON TABLE ts_masqueope_mao TO invite;


--
-- Name: ts_masqueordreaffichage_maa; Type: ACL; Schema: nat; Owner: nat
--

REVOKE ALL ON TABLE ts_masqueordreaffichage_maa FROM PUBLIC;
REVOKE ALL ON TABLE ts_masqueordreaffichage_maa FROM nat;
GRANT ALL ON TABLE ts_masqueordreaffichage_maa TO nat;
GRANT SELECT ON TABLE ts_masqueordreaffichage_maa TO invite;


--
-- Name: ts_taillevideo_tav; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE ts_taillevideo_tav FROM PUBLIC;
REVOKE ALL ON TABLE ts_taillevideo_tav FROM postgres;
GRANT ALL ON TABLE ts_taillevideo_tav TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_taillevideo_tav TO nat;
GRANT SELECT ON TABLE ts_taillevideo_tav TO invite;


--
-- Name: ts_taxonvideo_txv; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE ts_taxonvideo_txv FROM PUBLIC;
REVOKE ALL ON TABLE ts_taxonvideo_txv FROM postgres;
GRANT ALL ON TABLE ts_taxonvideo_txv TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_taxonvideo_txv TO nat;
GRANT SELECT ON TABLE ts_taxonvideo_txv TO invite;


--
-- Name: vue_cond_env_mj; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE vue_cond_env_mj FROM PUBLIC;
REVOKE ALL ON TABLE vue_cond_env_mj FROM postgres;
GRANT ALL ON TABLE vue_cond_env_mj TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE vue_cond_env_mj TO nat;


--
-- Name: vue_ope_cond_env_mj; Type: ACL; Schema: nat; Owner: postgres
--

REVOKE ALL ON TABLE vue_ope_cond_env_mj FROM PUBLIC;
REVOKE ALL ON TABLE vue_ope_cond_env_mj FROM postgres;
GRANT ALL ON TABLE vue_ope_cond_env_mj TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE vue_ope_cond_env_mj TO nat;


SET search_path = ref, pg_catalog;

--
-- Name: tg_parametre_par; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tg_parametre_par FROM PUBLIC;
REVOKE ALL ON TABLE tg_parametre_par FROM postgres;
GRANT ALL ON TABLE tg_parametre_par TO postgres;
GRANT SELECT ON TABLE tg_parametre_par TO iav;
GRANT SELECT ON TABLE tg_parametre_par TO invite;
GRANT SELECT ON TABLE tg_parametre_par TO inra;
GRANT SELECT ON TABLE tg_parametre_par TO logrami;
GRANT SELECT ON TABLE tg_parametre_par TO mrm;
GRANT SELECT ON TABLE tg_parametre_par TO migado;
GRANT SELECT ON TABLE tg_parametre_par TO saumonrhin;
GRANT SELECT ON TABLE tg_parametre_par TO migradour;
GRANT SELECT ON TABLE tg_parametre_par TO charente;
GRANT SELECT ON TABLE tg_parametre_par TO fd80;
GRANT SELECT ON TABLE tg_parametre_par TO smatah;
GRANT SELECT ON TABLE tg_parametre_par TO azti;
GRANT SELECT ON TABLE tg_parametre_par TO bgm;
GRANT SELECT ON TABLE tg_parametre_par TO ngm;


--
-- Name: tr_devenirlot_dev; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_devenirlot_dev FROM PUBLIC;
REVOKE ALL ON TABLE tr_devenirlot_dev FROM postgres;
GRANT ALL ON TABLE tr_devenirlot_dev TO postgres;
GRANT SELECT ON TABLE tr_devenirlot_dev TO iav;
GRANT SELECT ON TABLE tr_devenirlot_dev TO invite;
GRANT SELECT ON TABLE tr_devenirlot_dev TO inra;
GRANT SELECT ON TABLE tr_devenirlot_dev TO logrami;
GRANT SELECT ON TABLE tr_devenirlot_dev TO mrm;
GRANT SELECT ON TABLE tr_devenirlot_dev TO migado;
GRANT SELECT ON TABLE tr_devenirlot_dev TO saumonrhin;
GRANT SELECT ON TABLE tr_devenirlot_dev TO migradour;
GRANT SELECT ON TABLE tr_devenirlot_dev TO charente;
GRANT SELECT ON TABLE tr_devenirlot_dev TO fd80;
GRANT SELECT ON TABLE tr_devenirlot_dev TO smatah;
GRANT SELECT ON TABLE tr_devenirlot_dev TO azti;
GRANT SELECT ON TABLE tr_devenirlot_dev TO bgm;
GRANT SELECT ON TABLE tr_devenirlot_dev TO ngm;


--
-- Name: tr_importancepatho_imp; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_importancepatho_imp FROM PUBLIC;
REVOKE ALL ON TABLE tr_importancepatho_imp FROM postgres;
GRANT ALL ON TABLE tr_importancepatho_imp TO postgres;
GRANT SELECT ON TABLE tr_importancepatho_imp TO iav;
GRANT SELECT ON TABLE tr_importancepatho_imp TO invite;
GRANT SELECT ON TABLE tr_importancepatho_imp TO inra;
GRANT SELECT ON TABLE tr_importancepatho_imp TO logrami;
GRANT SELECT ON TABLE tr_importancepatho_imp TO mrm;
GRANT SELECT ON TABLE tr_importancepatho_imp TO migado;
GRANT SELECT ON TABLE tr_importancepatho_imp TO saumonrhin;
GRANT SELECT ON TABLE tr_importancepatho_imp TO migradour;
GRANT SELECT ON TABLE tr_importancepatho_imp TO charente;
GRANT SELECT ON TABLE tr_importancepatho_imp TO fd80;
GRANT SELECT ON TABLE tr_importancepatho_imp TO smatah;
GRANT SELECT ON TABLE tr_importancepatho_imp TO azti;
GRANT SELECT ON TABLE tr_importancepatho_imp TO bgm;
GRANT SELECT ON TABLE tr_importancepatho_imp TO pmp;
GRANT SELECT ON TABLE tr_importancepatho_imp TO ngm;


--
-- Name: tr_localisationanatomique_loc; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_localisationanatomique_loc FROM PUBLIC;
REVOKE ALL ON TABLE tr_localisationanatomique_loc FROM postgres;
GRANT ALL ON TABLE tr_localisationanatomique_loc TO postgres;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO iav;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO invite;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO inra;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO logrami;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO mrm;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO migado;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO saumonrhin;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO migradour;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO charente;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO fd80;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO smatah;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO azti;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO bgm;
GRANT SELECT ON TABLE tr_localisationanatomique_loc TO ngm;


--
-- Name: tr_naturemarque_nmq; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_naturemarque_nmq FROM PUBLIC;
REVOKE ALL ON TABLE tr_naturemarque_nmq FROM postgres;
GRANT ALL ON TABLE tr_naturemarque_nmq TO postgres;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO iav;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO invite;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO inra;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO logrami;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO mrm;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO migado;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO saumonrhin;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO migradour;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO charente;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO fd80;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO smatah;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO azti;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO bgm;
GRANT SELECT ON TABLE tr_naturemarque_nmq TO ngm;


--
-- Name: tr_natureouvrage_nov; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_natureouvrage_nov FROM PUBLIC;
REVOKE ALL ON TABLE tr_natureouvrage_nov FROM postgres;
GRANT ALL ON TABLE tr_natureouvrage_nov TO postgres;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO iav;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO invite;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO inra;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO logrami;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO mrm;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO migado;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO saumonrhin;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO migradour;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO charente;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO fd80;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO smatah;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO azti;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO bgm;
GRANT SELECT ON TABLE tr_natureouvrage_nov TO ngm;


--
-- Name: tr_niveauechappement_ech; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_niveauechappement_ech FROM PUBLIC;
REVOKE ALL ON TABLE tr_niveauechappement_ech FROM postgres;
GRANT ALL ON TABLE tr_niveauechappement_ech TO postgres;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO iav;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO invite;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO inra;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO logrami;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO mrm;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO migado;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO saumonrhin;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO migradour;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO charente;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO fd80;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO smatah;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO azti;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO bgm;
GRANT SELECT ON TABLE tr_niveauechappement_ech TO ngm;


--
-- Name: tr_niveautaxonomique_ntx; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_niveautaxonomique_ntx FROM PUBLIC;
REVOKE ALL ON TABLE tr_niveautaxonomique_ntx FROM postgres;
GRANT ALL ON TABLE tr_niveautaxonomique_ntx TO postgres;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO iav;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO invite;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO inra;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO logrami;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO mrm;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO migado;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO saumonrhin;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO migradour;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO charente;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO fd80;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO smatah;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO azti;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO bgm;
GRANT SELECT ON TABLE tr_niveautaxonomique_ntx TO ngm;


--
-- Name: tr_parametrequalitatif_qal; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_parametrequalitatif_qal FROM PUBLIC;
REVOKE ALL ON TABLE tr_parametrequalitatif_qal FROM postgres;
GRANT ALL ON TABLE tr_parametrequalitatif_qal TO postgres;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO iav;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO invite;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO inra;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO logrami;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO mrm;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO migado;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO saumonrhin;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO migradour;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO charente;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO fd80;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO smatah;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO azti;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO bgm;
GRANT SELECT ON TABLE tr_parametrequalitatif_qal TO ngm;


--
-- Name: tr_parametrequantitatif_qan; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_parametrequantitatif_qan FROM PUBLIC;
REVOKE ALL ON TABLE tr_parametrequantitatif_qan FROM postgres;
GRANT ALL ON TABLE tr_parametrequantitatif_qan TO postgres;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO iav;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO invite;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO inra;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO logrami;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO mrm;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO migado;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO saumonrhin;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO migradour;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO charente;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO fd80;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO smatah;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO azti;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO bgm;
GRANT SELECT ON TABLE tr_parametrequantitatif_qan TO ngm;


--
-- Name: tr_pathologie_pat; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_pathologie_pat FROM PUBLIC;
REVOKE ALL ON TABLE tr_pathologie_pat FROM postgres;
GRANT ALL ON TABLE tr_pathologie_pat TO postgres;
GRANT SELECT ON TABLE tr_pathologie_pat TO iav;
GRANT SELECT ON TABLE tr_pathologie_pat TO invite;
GRANT SELECT ON TABLE tr_pathologie_pat TO inra;
GRANT SELECT ON TABLE tr_pathologie_pat TO logrami;
GRANT SELECT ON TABLE tr_pathologie_pat TO mrm;
GRANT SELECT ON TABLE tr_pathologie_pat TO migado;
GRANT SELECT ON TABLE tr_pathologie_pat TO saumonrhin;
GRANT SELECT ON TABLE tr_pathologie_pat TO migradour;
GRANT SELECT ON TABLE tr_pathologie_pat TO charente;
GRANT SELECT ON TABLE tr_pathologie_pat TO fd80;
GRANT SELECT ON TABLE tr_pathologie_pat TO smatah;
GRANT SELECT ON TABLE tr_pathologie_pat TO azti;
GRANT SELECT ON TABLE tr_pathologie_pat TO bgm;
GRANT SELECT ON TABLE tr_pathologie_pat TO ngm;


--
-- Name: tr_prelevement_pre; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_prelevement_pre FROM PUBLIC;
REVOKE ALL ON TABLE tr_prelevement_pre FROM postgres;
GRANT ALL ON TABLE tr_prelevement_pre TO postgres;
GRANT SELECT ON TABLE tr_prelevement_pre TO iav;
GRANT SELECT ON TABLE tr_prelevement_pre TO invite;
GRANT SELECT ON TABLE tr_prelevement_pre TO inra;
GRANT SELECT ON TABLE tr_prelevement_pre TO logrami;
GRANT SELECT ON TABLE tr_prelevement_pre TO mrm;
GRANT SELECT ON TABLE tr_prelevement_pre TO migado;
GRANT SELECT ON TABLE tr_prelevement_pre TO saumonrhin;
GRANT SELECT ON TABLE tr_prelevement_pre TO migradour;
GRANT SELECT ON TABLE tr_prelevement_pre TO charente;
GRANT SELECT ON TABLE tr_prelevement_pre TO fd80;
GRANT SELECT ON TABLE tr_prelevement_pre TO smatah;
GRANT SELECT ON TABLE tr_prelevement_pre TO azti;
GRANT SELECT ON TABLE tr_prelevement_pre TO bgm;
GRANT SELECT ON TABLE tr_prelevement_pre TO ngm;


--
-- Name: tr_stadedeveloppement_std; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_stadedeveloppement_std FROM PUBLIC;
REVOKE ALL ON TABLE tr_stadedeveloppement_std FROM postgres;
GRANT ALL ON TABLE tr_stadedeveloppement_std TO postgres;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO iav;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO invite;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO inra;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO logrami;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO mrm;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO migado;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO saumonrhin;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO migradour;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO charente;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO fd80;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO smatah;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO azti;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO bgm;
GRANT SELECT ON TABLE tr_stadedeveloppement_std TO ngm;


--
-- Name: tr_taxon_tax; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_taxon_tax FROM PUBLIC;
REVOKE ALL ON TABLE tr_taxon_tax FROM postgres;
GRANT ALL ON TABLE tr_taxon_tax TO postgres;
GRANT SELECT ON TABLE tr_taxon_tax TO iav;
GRANT SELECT ON TABLE tr_taxon_tax TO invite;
GRANT SELECT ON TABLE tr_taxon_tax TO inra;
GRANT SELECT ON TABLE tr_taxon_tax TO logrami;
GRANT SELECT ON TABLE tr_taxon_tax TO mrm;
GRANT SELECT ON TABLE tr_taxon_tax TO migado;
GRANT SELECT ON TABLE tr_taxon_tax TO saumonrhin;
GRANT SELECT ON TABLE tr_taxon_tax TO migradour;
GRANT SELECT ON TABLE tr_taxon_tax TO charente;
GRANT SELECT ON TABLE tr_taxon_tax TO fd80;
GRANT SELECT ON TABLE tr_taxon_tax TO smatah;
GRANT SELECT ON TABLE tr_taxon_tax TO azti;
GRANT SELECT ON TABLE tr_taxon_tax TO bgm;
GRANT SELECT ON TABLE tr_taxon_tax TO ngm;


--
-- Name: tr_typearretdisp_tar; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_typearretdisp_tar FROM PUBLIC;
REVOKE ALL ON TABLE tr_typearretdisp_tar FROM postgres;
GRANT ALL ON TABLE tr_typearretdisp_tar TO postgres;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO iav;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO invite;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO inra;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO logrami;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO mrm;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO migado;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO saumonrhin;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO migradour;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO charente;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO fd80;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO smatah;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO azti;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO bgm;
GRANT SELECT ON TABLE tr_typearretdisp_tar TO ngm;


--
-- Name: tr_typedc_tdc; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_typedc_tdc FROM PUBLIC;
REVOKE ALL ON TABLE tr_typedc_tdc FROM postgres;
GRANT ALL ON TABLE tr_typedc_tdc TO postgres;
GRANT SELECT ON TABLE tr_typedc_tdc TO iav;
GRANT SELECT ON TABLE tr_typedc_tdc TO invite;
GRANT SELECT ON TABLE tr_typedc_tdc TO inra;
GRANT SELECT ON TABLE tr_typedc_tdc TO logrami;
GRANT SELECT ON TABLE tr_typedc_tdc TO mrm;
GRANT SELECT ON TABLE tr_typedc_tdc TO migado;
GRANT SELECT ON TABLE tr_typedc_tdc TO saumonrhin;
GRANT SELECT ON TABLE tr_typedc_tdc TO migradour;
GRANT SELECT ON TABLE tr_typedc_tdc TO charente;
GRANT SELECT ON TABLE tr_typedc_tdc TO fd80;
GRANT SELECT ON TABLE tr_typedc_tdc TO smatah;
GRANT SELECT ON TABLE tr_typedc_tdc TO azti;
GRANT SELECT ON TABLE tr_typedc_tdc TO bgm;
GRANT SELECT ON TABLE tr_typedc_tdc TO ngm;


--
-- Name: tr_typedf_tdf; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_typedf_tdf FROM PUBLIC;
REVOKE ALL ON TABLE tr_typedf_tdf FROM postgres;
GRANT ALL ON TABLE tr_typedf_tdf TO postgres;
GRANT SELECT ON TABLE tr_typedf_tdf TO iav;
GRANT SELECT ON TABLE tr_typedf_tdf TO invite;
GRANT SELECT ON TABLE tr_typedf_tdf TO inra;
GRANT SELECT ON TABLE tr_typedf_tdf TO logrami;
GRANT SELECT ON TABLE tr_typedf_tdf TO charente;
GRANT SELECT ON TABLE tr_typedf_tdf TO fd80;
GRANT SELECT ON TABLE tr_typedf_tdf TO smatah;
GRANT SELECT ON TABLE tr_typedf_tdf TO azti;
GRANT SELECT ON TABLE tr_typedf_tdf TO bgm;
GRANT SELECT ON TABLE tr_typedf_tdf TO migradour;
GRANT SELECT ON TABLE tr_typedf_tdf TO migado;
GRANT SELECT ON TABLE tr_typedf_tdf TO mrm;
GRANT SELECT ON TABLE tr_typedf_tdf TO saumonrhin;
GRANT SELECT ON TABLE tr_typedf_tdf TO ngm;


--
-- Name: tr_typequantitelot_qte; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_typequantitelot_qte FROM PUBLIC;
REVOKE ALL ON TABLE tr_typequantitelot_qte FROM postgres;
GRANT ALL ON TABLE tr_typequantitelot_qte TO postgres;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO iav;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO invite;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO inra;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO logrami;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO mrm;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO migado;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO saumonrhin;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO migradour;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO charente;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO fd80;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO smatah;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO azti;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO bgm;
GRANT SELECT ON TABLE tr_typequantitelot_qte TO ngm;


--
-- Name: tr_valeurparametrequalitatif_val; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE tr_valeurparametrequalitatif_val FROM PUBLIC;
REVOKE ALL ON TABLE tr_valeurparametrequalitatif_val FROM postgres;
GRANT ALL ON TABLE tr_valeurparametrequalitatif_val TO postgres;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO iav;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO invite;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO inra;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO logrami;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO mrm;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO migado;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO saumonrhin;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO migradour;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO charente;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO fd80;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO smatah;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO azti;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO bgm;
GRANT SELECT ON TABLE tr_valeurparametrequalitatif_val TO ngm;


--
-- Name: ts_maintenance_main; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE ts_maintenance_main FROM PUBLIC;
REVOKE ALL ON TABLE ts_maintenance_main FROM postgres;
GRANT ALL ON TABLE ts_maintenance_main TO postgres;
GRANT SELECT ON TABLE ts_maintenance_main TO ngm;


--
-- Name: ts_messager_msr; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE ts_messager_msr FROM PUBLIC;
REVOKE ALL ON TABLE ts_messager_msr FROM postgres;
GRANT ALL ON TABLE ts_messager_msr TO postgres;
GRANT SELECT ON TABLE ts_messager_msr TO iav;
GRANT SELECT ON TABLE ts_messager_msr TO invite;
GRANT SELECT ON TABLE ts_messager_msr TO inra;
GRANT SELECT ON TABLE ts_messager_msr TO logrami;
GRANT SELECT ON TABLE ts_messager_msr TO mrm;
GRANT SELECT ON TABLE ts_messager_msr TO migado;
GRANT SELECT ON TABLE ts_messager_msr TO saumonrhin;
GRANT SELECT ON TABLE ts_messager_msr TO migradour;
GRANT SELECT ON TABLE ts_messager_msr TO charente;
GRANT SELECT ON TABLE ts_messager_msr TO fd80;
GRANT SELECT ON TABLE ts_messager_msr TO smatah;
GRANT SELECT ON TABLE ts_messager_msr TO azti;
GRANT SELECT ON TABLE ts_messager_msr TO bgm;
GRANT SELECT ON TABLE ts_messager_msr TO ngm;


--
-- Name: ts_messagerlang_mrl; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE ts_messagerlang_mrl FROM PUBLIC;
REVOKE ALL ON TABLE ts_messagerlang_mrl FROM postgres;
GRANT ALL ON TABLE ts_messagerlang_mrl TO postgres;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO iav;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO invite;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO inra;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO logrami;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO mrm;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO migado;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO saumonrhin;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO migradour;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO charente;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO fd80;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO smatah;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO azti;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO bgm;
GRANT SELECT ON TABLE ts_messagerlang_mrl TO ngm;


--
-- Name: ts_organisme_org; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE ts_organisme_org FROM PUBLIC;
REVOKE ALL ON TABLE ts_organisme_org FROM postgres;
GRANT ALL ON TABLE ts_organisme_org TO postgres;
GRANT SELECT ON TABLE ts_organisme_org TO invite;
GRANT SELECT ON TABLE ts_organisme_org TO bgm;
GRANT SELECT ON TABLE ts_organisme_org TO inra;
GRANT SELECT ON TABLE ts_organisme_org TO logrami;
GRANT SELECT ON TABLE ts_organisme_org TO mrm;
GRANT SELECT ON TABLE ts_organisme_org TO ngm;
GRANT SELECT ON TABLE ts_organisme_org TO migado;
GRANT SELECT ON TABLE ts_organisme_org TO saumonrhin;
GRANT SELECT ON TABLE ts_organisme_org TO migradour;
GRANT SELECT ON TABLE ts_organisme_org TO charente;
GRANT SELECT ON TABLE ts_organisme_org TO fd80;
GRANT SELECT ON TABLE ts_organisme_org TO smatah;
GRANT SELECT ON TABLE ts_organisme_org TO azti;


--
-- Name: ts_sequence_seq; Type: ACL; Schema: ref; Owner: postgres
--

REVOKE ALL ON TABLE ts_sequence_seq FROM PUBLIC;
REVOKE ALL ON TABLE ts_sequence_seq FROM postgres;
GRANT ALL ON TABLE ts_sequence_seq TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_sequence_seq TO iav;
GRANT SELECT ON TABLE ts_sequence_seq TO invite;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_sequence_seq TO inra;
GRANT SELECT ON TABLE ts_sequence_seq TO logrami;
GRANT SELECT ON TABLE ts_sequence_seq TO mrm;
GRANT SELECT ON TABLE ts_sequence_seq TO migado;
GRANT SELECT ON TABLE ts_sequence_seq TO saumonrhin;
GRANT SELECT ON TABLE ts_sequence_seq TO migradour;
GRANT SELECT ON TABLE ts_sequence_seq TO charente;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_sequence_seq TO fd80;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_sequence_seq TO smatah;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_sequence_seq TO azti;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_sequence_seq TO bgm;
GRANT SELECT ON TABLE ts_sequence_seq TO ngm;


SET search_path = user_1, pg_catalog;

--
-- Name: t_bilanmigrationjournalier_bjo; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE t_bilanmigrationjournalier_bjo FROM PUBLIC;
REVOKE ALL ON TABLE t_bilanmigrationjournalier_bjo FROM postgres;
GRANT ALL ON TABLE t_bilanmigrationjournalier_bjo TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_bilanmigrationjournalier_bjo TO iav;
GRANT SELECT ON TABLE t_bilanmigrationjournalier_bjo TO invite;


--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq FROM postgres;
GRANT ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq TO postgres;
GRANT ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq TO invite;


--
-- Name: t_bilanmigrationmensuel_bme; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE t_bilanmigrationmensuel_bme FROM PUBLIC;
REVOKE ALL ON TABLE t_bilanmigrationmensuel_bme FROM postgres;
GRANT ALL ON TABLE t_bilanmigrationmensuel_bme TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_bilanmigrationmensuel_bme TO iav;
GRANT SELECT ON TABLE t_bilanmigrationmensuel_bme TO invite;


--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq FROM postgres;
GRANT ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq TO postgres;
GRANT ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq TO invite;


--
-- Name: t_dispositifcomptage_dic; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE t_dispositifcomptage_dic FROM PUBLIC;
REVOKE ALL ON TABLE t_dispositifcomptage_dic FROM postgres;
GRANT ALL ON TABLE t_dispositifcomptage_dic TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_dispositifcomptage_dic TO iav;
GRANT SELECT ON TABLE t_dispositifcomptage_dic TO invite;


--
-- Name: t_dispositiffranchissement_dif; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE t_dispositiffranchissement_dif FROM PUBLIC;
REVOKE ALL ON TABLE t_dispositiffranchissement_dif FROM postgres;
GRANT ALL ON TABLE t_dispositiffranchissement_dif TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_dispositiffranchissement_dif TO iav;
GRANT SELECT ON TABLE t_dispositiffranchissement_dif TO invite;


--
-- Name: t_lot_lot; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON TABLE t_lot_lot FROM PUBLIC;
REVOKE ALL ON TABLE t_lot_lot FROM iav;
GRANT ALL ON TABLE t_lot_lot TO iav;
GRANT SELECT ON TABLE t_lot_lot TO invite;


--
-- Name: t_lot_lot_lot_identifiant_seq; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON SEQUENCE t_lot_lot_lot_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_lot_lot_lot_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE t_lot_lot_lot_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_lot_lot_lot_identifiant_seq TO invite;


--
-- Name: t_marque_mqe; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE t_marque_mqe FROM PUBLIC;
REVOKE ALL ON TABLE t_marque_mqe FROM postgres;
GRANT ALL ON TABLE t_marque_mqe TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_marque_mqe TO iav;
GRANT SELECT ON TABLE t_marque_mqe TO invite;


--
-- Name: t_operation_ope; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON TABLE t_operation_ope FROM PUBLIC;
REVOKE ALL ON TABLE t_operation_ope FROM iav;
GRANT ALL ON TABLE t_operation_ope TO iav;
GRANT SELECT ON TABLE t_operation_ope TO invite;


--
-- Name: t_operation_ope_ope_identifiant_seq; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON SEQUENCE t_operation_ope_ope_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_operation_ope_ope_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE t_operation_ope_ope_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_operation_ope_ope_identifiant_seq TO invite;


--
-- Name: t_operationmarquage_omq; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE t_operationmarquage_omq FROM PUBLIC;
REVOKE ALL ON TABLE t_operationmarquage_omq FROM postgres;
GRANT ALL ON TABLE t_operationmarquage_omq TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_operationmarquage_omq TO iav;
GRANT SELECT ON TABLE t_operationmarquage_omq TO invite;


--
-- Name: t_ouvrage_ouv; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON TABLE t_ouvrage_ouv FROM PUBLIC;
REVOKE ALL ON TABLE t_ouvrage_ouv FROM iav;
GRANT ALL ON TABLE t_ouvrage_ouv TO iav;
GRANT SELECT ON TABLE t_ouvrage_ouv TO invite;


--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq TO invite;


--
-- Name: t_periodefonctdispositif_per; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE t_periodefonctdispositif_per FROM PUBLIC;
REVOKE ALL ON TABLE t_periodefonctdispositif_per FROM postgres;
GRANT ALL ON TABLE t_periodefonctdispositif_per TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_periodefonctdispositif_per TO iav;
GRANT SELECT ON TABLE t_periodefonctdispositif_per TO invite;


--
-- Name: t_station_sta; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE t_station_sta FROM PUBLIC;
REVOKE ALL ON TABLE t_station_sta FROM postgres;
GRANT ALL ON TABLE t_station_sta TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_station_sta TO iav;
GRANT SELECT ON TABLE t_station_sta TO invite;


--
-- Name: tg_dispositif_dis; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON TABLE tg_dispositif_dis FROM PUBLIC;
REVOKE ALL ON TABLE tg_dispositif_dis FROM iav;
GRANT ALL ON TABLE tg_dispositif_dis TO iav;
GRANT SELECT ON TABLE tg_dispositif_dis TO invite;


--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq TO invite;


--
-- Name: tj_actionmarquage_act; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE tj_actionmarquage_act FROM PUBLIC;
REVOKE ALL ON TABLE tj_actionmarquage_act FROM postgres;
GRANT ALL ON TABLE tj_actionmarquage_act TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_actionmarquage_act TO iav;
GRANT SELECT ON TABLE tj_actionmarquage_act TO invite;


--
-- Name: tj_caracteristiquelot_car; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE tj_caracteristiquelot_car FROM PUBLIC;
REVOKE ALL ON TABLE tj_caracteristiquelot_car FROM postgres;
GRANT ALL ON TABLE tj_caracteristiquelot_car TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_caracteristiquelot_car TO iav;
GRANT SELECT ON TABLE tj_caracteristiquelot_car TO invite;


--
-- Name: tj_coefficientconversion_coe; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE tj_coefficientconversion_coe FROM PUBLIC;
REVOKE ALL ON TABLE tj_coefficientconversion_coe FROM postgres;
GRANT ALL ON TABLE tj_coefficientconversion_coe TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_coefficientconversion_coe TO iav;
GRANT SELECT ON TABLE tj_coefficientconversion_coe TO invite;


--
-- Name: tj_conditionenvironnementale_env; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE tj_conditionenvironnementale_env FROM PUBLIC;
REVOKE ALL ON TABLE tj_conditionenvironnementale_env FROM postgres;
GRANT ALL ON TABLE tj_conditionenvironnementale_env TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_conditionenvironnementale_env TO iav;
GRANT SELECT ON TABLE tj_conditionenvironnementale_env TO invite;


--
-- Name: tj_dfestdestinea_dtx; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE tj_dfestdestinea_dtx FROM PUBLIC;
REVOKE ALL ON TABLE tj_dfestdestinea_dtx FROM postgres;
GRANT ALL ON TABLE tj_dfestdestinea_dtx TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_dfestdestinea_dtx TO iav;
GRANT SELECT ON TABLE tj_dfestdestinea_dtx TO invite;


--
-- Name: tj_dfesttype_dft; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE tj_dfesttype_dft FROM PUBLIC;
REVOKE ALL ON TABLE tj_dfesttype_dft FROM postgres;
GRANT ALL ON TABLE tj_dfesttype_dft TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_dfesttype_dft TO iav;
GRANT SELECT ON TABLE tj_dfesttype_dft TO invite;


--
-- Name: tj_pathologieconstatee_pco; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE tj_pathologieconstatee_pco FROM PUBLIC;
REVOKE ALL ON TABLE tj_pathologieconstatee_pco FROM postgres;
GRANT ALL ON TABLE tj_pathologieconstatee_pco TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_pathologieconstatee_pco TO iav;
GRANT SELECT ON TABLE tj_pathologieconstatee_pco TO invite;


--
-- Name: tj_prelevementlot_prl; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE tj_prelevementlot_prl FROM PUBLIC;
REVOKE ALL ON TABLE tj_prelevementlot_prl FROM postgres;
GRANT ALL ON TABLE tj_prelevementlot_prl TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_prelevementlot_prl TO iav;
GRANT SELECT ON TABLE tj_prelevementlot_prl TO invite;


--
-- Name: tj_stationmesure_stm; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON TABLE tj_stationmesure_stm FROM PUBLIC;
REVOKE ALL ON TABLE tj_stationmesure_stm FROM iav;
GRANT ALL ON TABLE tj_stationmesure_stm TO iav;
GRANT SELECT ON TABLE tj_stationmesure_stm TO invite;


--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq TO invite;


--
-- Name: tj_tauxechappement_txe; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE tj_tauxechappement_txe FROM PUBLIC;
REVOKE ALL ON TABLE tj_tauxechappement_txe FROM postgres;
GRANT ALL ON TABLE tj_tauxechappement_txe TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_tauxechappement_txe TO iav;
GRANT SELECT ON TABLE tj_tauxechappement_txe TO invite;


--
-- Name: ts_maintenance_main; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE ts_maintenance_main FROM PUBLIC;
REVOKE ALL ON TABLE ts_maintenance_main FROM postgres;
GRANT ALL ON TABLE ts_maintenance_main TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_maintenance_main TO iav;


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq FROM postgres;
GRANT ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq TO postgres;
GRANT ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq TO iav;


--
-- Name: ts_masque_mas; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON TABLE ts_masque_mas FROM PUBLIC;
REVOKE ALL ON TABLE ts_masque_mas FROM iav;
GRANT ALL ON TABLE ts_masque_mas TO iav;
GRANT SELECT ON TABLE ts_masque_mas TO invite;


--
-- Name: ts_masque_mas_mas_id_seq; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON SEQUENCE ts_masque_mas_mas_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE ts_masque_mas_mas_id_seq FROM iav;
GRANT ALL ON SEQUENCE ts_masque_mas_mas_id_seq TO iav;


--
-- Name: ts_masquecaracteristiquelot_mac; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON TABLE ts_masquecaracteristiquelot_mac FROM PUBLIC;
REVOKE ALL ON TABLE ts_masquecaracteristiquelot_mac FROM iav;
GRANT ALL ON TABLE ts_masquecaracteristiquelot_mac TO iav;
GRANT SELECT ON TABLE ts_masquecaracteristiquelot_mac TO invite;


--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq FROM iav;
GRANT ALL ON SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq TO iav;


--
-- Name: ts_masqueconditionsenvironnementales_mae; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE ts_masqueconditionsenvironnementales_mae FROM PUBLIC;
REVOKE ALL ON TABLE ts_masqueconditionsenvironnementales_mae FROM postgres;
GRANT ALL ON TABLE ts_masqueconditionsenvironnementales_mae TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_masqueconditionsenvironnementales_mae TO iav;
GRANT SELECT ON TABLE ts_masqueconditionsenvironnementales_mae TO invite;


--
-- Name: ts_masquelot_mal; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE ts_masquelot_mal FROM PUBLIC;
REVOKE ALL ON TABLE ts_masquelot_mal FROM postgres;
GRANT ALL ON TABLE ts_masquelot_mal TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_masquelot_mal TO iav;
GRANT SELECT ON TABLE ts_masquelot_mal TO invite;


--
-- Name: ts_masqueope_mao; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE ts_masqueope_mao FROM PUBLIC;
REVOKE ALL ON TABLE ts_masqueope_mao FROM postgres;
GRANT ALL ON TABLE ts_masqueope_mao TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_masqueope_mao TO iav;
GRANT SELECT ON TABLE ts_masqueope_mao TO invite;


--
-- Name: ts_masqueordreaffichage_maa; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON TABLE ts_masqueordreaffichage_maa FROM PUBLIC;
REVOKE ALL ON TABLE ts_masqueordreaffichage_maa FROM iav;
GRANT ALL ON TABLE ts_masqueordreaffichage_maa TO iav;
GRANT SELECT ON TABLE ts_masqueordreaffichage_maa TO invite;


--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: ACL; Schema: user_1; Owner: iav
--

REVOKE ALL ON SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq FROM iav;
GRANT ALL ON SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq TO iav;


--
-- Name: ts_taillevideo_tav; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE ts_taillevideo_tav FROM PUBLIC;
REVOKE ALL ON TABLE ts_taillevideo_tav FROM postgres;
GRANT ALL ON TABLE ts_taillevideo_tav TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_taillevideo_tav TO iav;
GRANT SELECT ON TABLE ts_taillevideo_tav TO invite;


--
-- Name: ts_taxonvideo_txv; Type: ACL; Schema: user_1; Owner: postgres
--

REVOKE ALL ON TABLE ts_taxonvideo_txv FROM PUBLIC;
REVOKE ALL ON TABLE ts_taxonvideo_txv FROM postgres;
GRANT ALL ON TABLE ts_taxonvideo_txv TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_taxonvideo_txv TO iav;
GRANT SELECT ON TABLE ts_taxonvideo_txv TO invite;


SET search_path = user_2, pg_catalog;

--
-- Name: t_bilanmigrationjournalier_bjo; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE t_bilanmigrationjournalier_bjo FROM PUBLIC;
REVOKE ALL ON TABLE t_bilanmigrationjournalier_bjo FROM postgres;
GRANT ALL ON TABLE t_bilanmigrationjournalier_bjo TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_bilanmigrationjournalier_bjo TO iav;
GRANT SELECT ON TABLE t_bilanmigrationjournalier_bjo TO invite;


--
-- Name: t_bilanmigrationjournalier_bjo_bjo_identifiant_seq; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq FROM postgres;
GRANT ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq TO postgres;
GRANT ALL ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_bilanmigrationjournalier_bjo_bjo_identifiant_seq TO invite;


--
-- Name: t_bilanmigrationmensuel_bme; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE t_bilanmigrationmensuel_bme FROM PUBLIC;
REVOKE ALL ON TABLE t_bilanmigrationmensuel_bme FROM postgres;
GRANT ALL ON TABLE t_bilanmigrationmensuel_bme TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_bilanmigrationmensuel_bme TO iav;
GRANT SELECT ON TABLE t_bilanmigrationmensuel_bme TO invite;


--
-- Name: t_bilanmigrationmensuel_bme_bme_identifiant_seq; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq FROM postgres;
GRANT ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq TO postgres;
GRANT ALL ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_bilanmigrationmensuel_bme_bme_identifiant_seq TO invite;


--
-- Name: t_dispositifcomptage_dic; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE t_dispositifcomptage_dic FROM PUBLIC;
REVOKE ALL ON TABLE t_dispositifcomptage_dic FROM postgres;
GRANT ALL ON TABLE t_dispositifcomptage_dic TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_dispositifcomptage_dic TO iav;
GRANT SELECT ON TABLE t_dispositifcomptage_dic TO invite;


--
-- Name: t_dispositiffranchissement_dif; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE t_dispositiffranchissement_dif FROM PUBLIC;
REVOKE ALL ON TABLE t_dispositiffranchissement_dif FROM postgres;
GRANT ALL ON TABLE t_dispositiffranchissement_dif TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_dispositiffranchissement_dif TO iav;
GRANT SELECT ON TABLE t_dispositiffranchissement_dif TO invite;


--
-- Name: t_lot_lot; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON TABLE t_lot_lot FROM PUBLIC;
REVOKE ALL ON TABLE t_lot_lot FROM iav;
GRANT ALL ON TABLE t_lot_lot TO iav;
GRANT SELECT ON TABLE t_lot_lot TO invite;


--
-- Name: t_lot_lot_lot_identifiant_seq; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON SEQUENCE t_lot_lot_lot_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_lot_lot_lot_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE t_lot_lot_lot_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_lot_lot_lot_identifiant_seq TO invite;


--
-- Name: t_marque_mqe; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE t_marque_mqe FROM PUBLIC;
REVOKE ALL ON TABLE t_marque_mqe FROM postgres;
GRANT ALL ON TABLE t_marque_mqe TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_marque_mqe TO iav;
GRANT SELECT ON TABLE t_marque_mqe TO invite;


--
-- Name: t_operation_ope; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON TABLE t_operation_ope FROM PUBLIC;
REVOKE ALL ON TABLE t_operation_ope FROM iav;
GRANT ALL ON TABLE t_operation_ope TO iav;
GRANT SELECT ON TABLE t_operation_ope TO invite;


--
-- Name: t_operation_ope_ope_identifiant_seq; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON SEQUENCE t_operation_ope_ope_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_operation_ope_ope_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE t_operation_ope_ope_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_operation_ope_ope_identifiant_seq TO invite;


--
-- Name: t_operationmarquage_omq; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE t_operationmarquage_omq FROM PUBLIC;
REVOKE ALL ON TABLE t_operationmarquage_omq FROM postgres;
GRANT ALL ON TABLE t_operationmarquage_omq TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_operationmarquage_omq TO iav;
GRANT SELECT ON TABLE t_operationmarquage_omq TO invite;


--
-- Name: t_ouvrage_ouv; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON TABLE t_ouvrage_ouv FROM PUBLIC;
REVOKE ALL ON TABLE t_ouvrage_ouv FROM iav;
GRANT ALL ON TABLE t_ouvrage_ouv TO iav;
GRANT SELECT ON TABLE t_ouvrage_ouv TO invite;


--
-- Name: t_ouvrage_ouv_ouv_identifiant_seq; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE t_ouvrage_ouv_ouv_identifiant_seq TO invite;


--
-- Name: t_periodefonctdispositif_per; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE t_periodefonctdispositif_per FROM PUBLIC;
REVOKE ALL ON TABLE t_periodefonctdispositif_per FROM postgres;
GRANT ALL ON TABLE t_periodefonctdispositif_per TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_periodefonctdispositif_per TO iav;
GRANT SELECT ON TABLE t_periodefonctdispositif_per TO invite;


--
-- Name: t_station_sta; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE t_station_sta FROM PUBLIC;
REVOKE ALL ON TABLE t_station_sta FROM postgres;
GRANT ALL ON TABLE t_station_sta TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE t_station_sta TO iav;
GRANT SELECT ON TABLE t_station_sta TO invite;


--
-- Name: tg_dispositif_dis; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON TABLE tg_dispositif_dis FROM PUBLIC;
REVOKE ALL ON TABLE tg_dispositif_dis FROM iav;
GRANT ALL ON TABLE tg_dispositif_dis TO iav;
GRANT SELECT ON TABLE tg_dispositif_dis TO invite;


--
-- Name: tg_dispositif_dis_dis_identifiant_seq; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE tg_dispositif_dis_dis_identifiant_seq TO invite;


--
-- Name: tj_actionmarquage_act; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE tj_actionmarquage_act FROM PUBLIC;
REVOKE ALL ON TABLE tj_actionmarquage_act FROM postgres;
GRANT ALL ON TABLE tj_actionmarquage_act TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_actionmarquage_act TO iav;
GRANT SELECT ON TABLE tj_actionmarquage_act TO invite;


--
-- Name: tj_caracteristiquelot_car; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE tj_caracteristiquelot_car FROM PUBLIC;
REVOKE ALL ON TABLE tj_caracteristiquelot_car FROM postgres;
GRANT ALL ON TABLE tj_caracteristiquelot_car TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_caracteristiquelot_car TO iav;
GRANT SELECT ON TABLE tj_caracteristiquelot_car TO invite;


--
-- Name: tj_coefficientconversion_coe; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE tj_coefficientconversion_coe FROM PUBLIC;
REVOKE ALL ON TABLE tj_coefficientconversion_coe FROM postgres;
GRANT ALL ON TABLE tj_coefficientconversion_coe TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_coefficientconversion_coe TO iav;
GRANT SELECT ON TABLE tj_coefficientconversion_coe TO invite;


--
-- Name: tj_conditionenvironnementale_env; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE tj_conditionenvironnementale_env FROM PUBLIC;
REVOKE ALL ON TABLE tj_conditionenvironnementale_env FROM postgres;
GRANT ALL ON TABLE tj_conditionenvironnementale_env TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_conditionenvironnementale_env TO iav;
GRANT SELECT ON TABLE tj_conditionenvironnementale_env TO invite;


--
-- Name: tj_dfestdestinea_dtx; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE tj_dfestdestinea_dtx FROM PUBLIC;
REVOKE ALL ON TABLE tj_dfestdestinea_dtx FROM postgres;
GRANT ALL ON TABLE tj_dfestdestinea_dtx TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_dfestdestinea_dtx TO iav;
GRANT SELECT ON TABLE tj_dfestdestinea_dtx TO invite;


--
-- Name: tj_dfesttype_dft; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE tj_dfesttype_dft FROM PUBLIC;
REVOKE ALL ON TABLE tj_dfesttype_dft FROM postgres;
GRANT ALL ON TABLE tj_dfesttype_dft TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_dfesttype_dft TO iav;
GRANT SELECT ON TABLE tj_dfesttype_dft TO invite;


--
-- Name: tj_pathologieconstatee_pco; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE tj_pathologieconstatee_pco FROM PUBLIC;
REVOKE ALL ON TABLE tj_pathologieconstatee_pco FROM postgres;
GRANT ALL ON TABLE tj_pathologieconstatee_pco TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_pathologieconstatee_pco TO iav;
GRANT SELECT ON TABLE tj_pathologieconstatee_pco TO invite;


--
-- Name: tj_prelevementlot_prl; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE tj_prelevementlot_prl FROM PUBLIC;
REVOKE ALL ON TABLE tj_prelevementlot_prl FROM postgres;
GRANT ALL ON TABLE tj_prelevementlot_prl TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_prelevementlot_prl TO iav;
GRANT SELECT ON TABLE tj_prelevementlot_prl TO invite;


--
-- Name: tj_stationmesure_stm; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON TABLE tj_stationmesure_stm FROM PUBLIC;
REVOKE ALL ON TABLE tj_stationmesure_stm FROM iav;
GRANT ALL ON TABLE tj_stationmesure_stm TO iav;
GRANT SELECT ON TABLE tj_stationmesure_stm TO invite;


--
-- Name: tj_stationmesure_stm_stm_identifiant_seq; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq FROM iav;
GRANT ALL ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq TO iav;
GRANT SELECT,UPDATE ON SEQUENCE tj_stationmesure_stm_stm_identifiant_seq TO invite;


--
-- Name: tj_tauxechappement_txe; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE tj_tauxechappement_txe FROM PUBLIC;
REVOKE ALL ON TABLE tj_tauxechappement_txe FROM postgres;
GRANT ALL ON TABLE tj_tauxechappement_txe TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE tj_tauxechappement_txe TO iav;
GRANT SELECT ON TABLE tj_tauxechappement_txe TO invite;


--
-- Name: ts_maintenance_main; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE ts_maintenance_main FROM PUBLIC;
REVOKE ALL ON TABLE ts_maintenance_main FROM postgres;
GRANT ALL ON TABLE ts_maintenance_main TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_maintenance_main TO iav;


--
-- Name: ts_maintenance_main_main_identifiant_seq; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq FROM postgres;
GRANT ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq TO postgres;
GRANT ALL ON SEQUENCE ts_maintenance_main_main_identifiant_seq TO iav;


--
-- Name: ts_masque_mas; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON TABLE ts_masque_mas FROM PUBLIC;
REVOKE ALL ON TABLE ts_masque_mas FROM iav;
GRANT ALL ON TABLE ts_masque_mas TO iav;
GRANT SELECT ON TABLE ts_masque_mas TO invite;


--
-- Name: ts_masque_mas_mas_id_seq; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON SEQUENCE ts_masque_mas_mas_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE ts_masque_mas_mas_id_seq FROM iav;
GRANT ALL ON SEQUENCE ts_masque_mas_mas_id_seq TO iav;


--
-- Name: ts_masquecaracteristiquelot_mac; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON TABLE ts_masquecaracteristiquelot_mac FROM PUBLIC;
REVOKE ALL ON TABLE ts_masquecaracteristiquelot_mac FROM iav;
GRANT ALL ON TABLE ts_masquecaracteristiquelot_mac TO iav;
GRANT SELECT ON TABLE ts_masquecaracteristiquelot_mac TO invite;


--
-- Name: ts_masquecaracteristiquelot_mac_mac_id_seq; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq FROM iav;
GRANT ALL ON SEQUENCE ts_masquecaracteristiquelot_mac_mac_id_seq TO iav;


--
-- Name: ts_masqueconditionsenvironnementales_mae; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE ts_masqueconditionsenvironnementales_mae FROM PUBLIC;
REVOKE ALL ON TABLE ts_masqueconditionsenvironnementales_mae FROM postgres;
GRANT ALL ON TABLE ts_masqueconditionsenvironnementales_mae TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_masqueconditionsenvironnementales_mae TO iav;
GRANT SELECT ON TABLE ts_masqueconditionsenvironnementales_mae TO invite;


--
-- Name: ts_masquelot_mal; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE ts_masquelot_mal FROM PUBLIC;
REVOKE ALL ON TABLE ts_masquelot_mal FROM postgres;
GRANT ALL ON TABLE ts_masquelot_mal TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_masquelot_mal TO iav;
GRANT SELECT ON TABLE ts_masquelot_mal TO invite;


--
-- Name: ts_masqueope_mao; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE ts_masqueope_mao FROM PUBLIC;
REVOKE ALL ON TABLE ts_masqueope_mao FROM postgres;
GRANT ALL ON TABLE ts_masqueope_mao TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_masqueope_mao TO iav;
GRANT SELECT ON TABLE ts_masqueope_mao TO invite;


--
-- Name: ts_masqueordreaffichage_maa; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON TABLE ts_masqueordreaffichage_maa FROM PUBLIC;
REVOKE ALL ON TABLE ts_masqueordreaffichage_maa FROM iav;
GRANT ALL ON TABLE ts_masqueordreaffichage_maa TO iav;
GRANT SELECT ON TABLE ts_masqueordreaffichage_maa TO invite;


--
-- Name: ts_masqueordreaffichage_maa_maa_id_seq; Type: ACL; Schema: user_2; Owner: iav
--

REVOKE ALL ON SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq FROM PUBLIC;
REVOKE ALL ON SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq FROM iav;
GRANT ALL ON SEQUENCE ts_masqueordreaffichage_maa_maa_id_seq TO iav;


--
-- Name: ts_taillevideo_tav; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE ts_taillevideo_tav FROM PUBLIC;
REVOKE ALL ON TABLE ts_taillevideo_tav FROM postgres;
GRANT ALL ON TABLE ts_taillevideo_tav TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_taillevideo_tav TO iav;
GRANT SELECT ON TABLE ts_taillevideo_tav TO invite;


--
-- Name: ts_taxonvideo_txv; Type: ACL; Schema: user_2; Owner: postgres
--

REVOKE ALL ON TABLE ts_taxonvideo_txv FROM PUBLIC;
REVOKE ALL ON TABLE ts_taxonvideo_txv FROM postgres;
GRANT ALL ON TABLE ts_taxonvideo_txv TO postgres;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE ts_taxonvideo_txv TO iav;
GRANT SELECT ON TABLE ts_taxonvideo_txv TO invite;


--
-- PostgreSQL database dump complete
--

