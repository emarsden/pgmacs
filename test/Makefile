EMACS ?= emacs
DOCKER ?= podman
WORKDIR := $(shell mktemp -d)
PEG := $(shell mktemp --tmpdir --suffix .el pgelpeg-XXXX)
PEGURL = "https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/peg.el?h=externals/peg"
PEGURL = "https://raw.githubusercontent.com/emacsmirror/emacs/be786ab3bb263cb88e548f439562cbdf93587a3f/lisp/progmodes/peg.el"
DOCKER ?= podman
DEPS = -l ../../pg-el/pg.el -l ../pgmacstbl.el -l ../pgmacs-sql-keywords.el -l ../pgmacs-schemaspy.el -l ../pgmacs-chrome.el -l ../pgmacs.el

test-local:
	${EMACS} -Q -l require-pg.el ${DEPS} -l test.el -f pgmacs-test

test: 
	${EMACS} -Q -l require-pg.el ${DEPS} -l test.el -f pgmacs-open-pguri

widget:
	${EMACS} -Q -l require-pg.el ${DEPS} -l test.el -f pgmacs-widget

test-minimal:
	${EMACS} -Q -l require-pg.el ${DEPS} -l test.el -f pgmacs-test-minimal

# Pagila database has too complex a schema for an initial intro to PGmacs
pagila:
	${EMACS} -Q -l require-pg.el ${DEPS} -l test.el -f pgmacs-test-pagila


# Shakespeare database does not work with SchemaSpy because the default schema shakespeare is not found.
shakespeare:
	${EMACS} -Q -l require-pg.el ${DEPS} -l test.el -f pgmacs-test-shakespeare


# Chinook database, a digital media store. A good test case for SchemaSpy support.
chinook:
	${EMACS} -Q -l require-pg.el ${DEPS} -l test.el -f pgmacs-test-chinook


ebiacuk:
	${EMACS} -Q -l require-pg.el ${DEPS} -l test.el -f pgmacs-test-ebiacuk

postgres:
	cp require-pg.el ../pgmacstbl.el ../pgmacs-sql-keywords.el ../pgmacs.el test.el /tmp
	cd /tmp && sudo -u postgres ${EMACS} -nw -Q -l /tmp/require-pg.el -l /tmp/pgmacstbl.el -l /tmp/pgmacs-sql-keywords.el -l /tmp/pgmacs.el -l /tmp/test.el -f pgmacs-test-postgres


terminal:
	${EMACS} -Q -nw -l require-pg.el ${DEPS} -l test.el -f pgmacs-test


# Simulate using PGmacs over a slow network connection to PostgreSQL, using the Toxiproxy proxy and
# a "delay" toxic.
#
# https://github.com/Shopify/toxiproxy
slow:
	${DOCKER} run --rm --name toxiproxy \
          --net=host \
          -d ghcr.io/shopify/toxiproxy:latest
	sleep 5
	# Create a proxy for PostgreSQL (listens on 5433, forwards to postgres:5432)
	curl -X POST http://127.0.0.1:8474/proxies \
          -H "Content-Type: application/json" \
          -d '{"name":"postgres", "listen":"0.0.0.0:5433", "upstream":"127.0.0.1:5432", "enabled":true}'
	sleep 1
	curl -X POST http://127.0.0.1:8474/proxies/postgres/toxics \
          -H "Content-Type: application/json" \
          -d '{"name":"pglatency","type":"latency","stream":"downstream","attributes":{"latency":500,"jitter":100}}'
	sleep 1
	PGURI=postgresql://pgeltestuser:pgeltest@127.0.0.1:5433/pgeltestdb $(MAKE) test
	${DOCKER} stop toxiproxy


container:
	${DOCKER} run -ti --network host ghcr.io/emarsden/pgmacs:latest


# We can't use the package management machinery when starting Emacs with "-Q", so download
# and load our peg dependency explicitly.
.PHONY: peg
peg:
	wget -q -O ${PEG} ${PEGURL}

# From https://rnacentral.org/help/public-database
test-public-instance: peg
	PGURI=postgres://reader:NWDMCE5xdipIjRrp@hh-pgsql-public.ebi.ac.uk:5432/pfmegrnargs $(MAKE) test


prerelease:
	${DOCKER} run -ti docker.io/silex/emacs:master-alpine-ci

# Using Docker images for Emacs from https://hub.docker.com/r/silex/emacs/ and the locally installed
# PostgreSQL.
test-emacs-dev: peg
	cp ${PEG} ${WORKDIR}/peg.el
	cp ../../pg-el/pg.el ../pgmacstbl.el ../pgmacs-sql-keywords.el ../pgmacs.el ${WORKDIR}
	${DOCKER} run --rm -it \
	   -v ${WORKDIR}:/tmp \
	   -e DEPS_DIR=/tmp/deps \
	   -e PGEL_HOSTNAME=10.0.2.2 \
	   --network slirp4netns:allow_host_loopback=true \
	   docker.io/silex/emacs:master-alpine-ci \
	   emacs -Q -l /tmp/peg.el -l /tmp/pg.el -l /tmp/pgmacstbl.el -l /tmp/pgmacs-sql-keywords.el -l /tmp/pgmacs.el --eval '(pgmacs-open-string "dbname=pgeltestdb host=10.0.2.2 user=pgeltestuser password=pgeltest")'


test-postgresql18: peg
	wget -q -O ${WORKDIR}/pg.el https://github.com/emarsden/pg-el/raw/refs/heads/main/pg.el
	cp ${PEG} ${WORKDIR}/peg.el
	cp ../pgmacstbl.el ../pgmacs-sql-keywords.el ../pgmacs-schemaspy.el ../pgmacs-chrome.el ../pgmacs.el ${WORKDIR}
	${DOCKER} run --rm --name pgsql --pull=newer \
           --publish 127.0.0.1:5439:5439 \
           -e POSTGRES_DB=pgeltestdb \
           -e POSTGRES_USER=pgeltestuser \
           -e POSTGRES_PASSWORD=pgeltest \
           -e PGPORT=5439 \
           -d docker.io/library/postgres:18-alpine
	sleep 5
	psql -f tiny-import.sql postgresql://pgeltestuser:pgeltest@127.0.0.1:5439/pgeltestdb
	${EMACS} -Q -l ${WORKDIR}/pg.el -l ${WORKDIR}/pgmacstbl.el -l ${WORKDIR}/pgmacs-sql-keywords.el -l ${WORKDIR}/pgmacs-schemaspy.el -l ${WORKDIR}/pgmacs-chrome.el -l ${WORKDIR}/pgmacs.el --eval '(pgmacs-open-uri "postgresql://pgeltestuser:pgeltest@127.0.0.1:5439/pgeltestdb")'
	${DOCKER} stop pgsql


test-postgresql16: peg
	cp ${PEG} ${WORKDIR}/peg.el
	cp ../../pg-el/pg.el ../pgmacstbl.el ../pgmacs-sql-keywords.el ../pgmacs-schemaspy.el ../pgmacs-chrome.el ../pgmacs.el ${WORKDIR}
	${DOCKER} run --rm --name pgsql --pull=newer \
	   --publish 127.0.0.1:5439:5439 \
	   -e POSTGRES_DB=pgeltestdb \
	   -e POSTGRES_USER=pgeltestuser \
	   -e POSTGRES_PASSWORD=pgeltest \
	   -e PGPORT=5439 \
	   -d docker.io/library/postgres:16
	sleep 5
	psql -f tiny-import.sql postgresql://pgeltestuser:pgeltest@127.0.0.1:5439/pgeltestdb
	${EMACS} -Q -l ${WORKDIR}/pg.el -l ${WORKDIR}/pgmacstbl.el -l ${WORKDIR}/pgmacs-sql-keywords.el -l ${WORKDIR}/pgmacs-schemaspy.el -l ${WORKDIR}/pgmacs-chrome.el -l ${WORKDIR}/pgmacs.el --eval '(pgmacs-open-uri "postgresql://pgeltestuser:pgeltest@127.0.0.1:5439/pgeltestdb")'
	${DOCKER} stop pgsql

test-postgresql14: peg
	cp ${PEG} ${WORKDIR}/peg.el
	cp ../../pg-el/pg.el ../pgmacstbl.el ../pgmacs-sql-keywords.el ../pgmacs-schemaspy.el ../pgmacs-chrome.el ../pgmacs.el ${WORKDIR}
	${DOCKER} run --rm --name pgsql \
	   --publish 5439:5439 \
	   -e PGPORT=5439 \
	   -e POSTGRES_DB=pgeltestdb \
	   -e POSTGRES_USER=peltestuser \
	   -e POSTGRES_PASSWORD=pgeltest \
	   -d docker.io/library/postgres:14
	sleep 5
	${EMACS} -Q -l ${WORKDIR}/pg.el -l ${WORKDIR}/pgmacstbl.el -l ${WORKDIR}/pgmacs-sql-keywords.el -l ${WORKDIR}/pgmacs-schemaspy.el -l ${WORKDIR}/pgmacs-chrome.el -l ${WORKDIR}/pgmacs.el --eval '(pgmacs-open-string "dbname=pgeltestdb hostname=localhost port=5439 user=pgeltestuser password=pgeltest")'
	${DOCKER} stop pgsql

test-shortnames: peg
	cp ${PEG} ${WORKDIR}/peg.el
	cp ../../pg-el/pg.el ../pgmacstbl.el ../pgmacs-sql-keywords.el ../pgmacs.el ${WORKDIR}
	${DOCKER} run --rm --name pgsql \
	   --publish 5926:5926 \
	   -e POSTGRES_DB=d \
	   -e POSTGRES_USER=u \
	   -e POSTGRES_PASSWORD=~ \
	   -e PGPORT=5926 \
	   -d docker.io/library/postgres:14
	sleep 5
	${EMACS} -Q -l ${WORKDIR}/peg.el -l ${WORKDIR}/pg.el -l ${WORKDIR}/pgmacstbl.el -l ${WORKDIR}/pgmacs-sql-keywords.el -l ${WORKDIR}/pgmacs.el --eval '(pgmacs-open-string "dbname=d hostname=localhost port=5926 user=u password=~")'
	${DOCKER} stop pgsql


# Our test on the hstore table is hanging in PGmacs
test-postgresql13: peg
	cp ${PEG} ${WORKDIR}/peg.el
	cp ../../pg-el/pg.el ../pgmacstbl.el ../pgmacs-sql-keywords.el ../pgmacs.el ${WORKDIR}
	${DOCKER} run --rm --name pgsql \
	   --publish 5439:5439 \
	   -e PGPORT=5439 \
	   -e POSTGRES_DB=pgeltestdb \
	   -e POSTGRES_USER=pgeltestuser \
	   -e POSTGRES_PASSWORD=pgeltest \
	   -d docker.io/library/postgres:13-alpine
	sleep 5
	${EMACS} -Q -l ${WORKDIR}/peg.el -l ${WORKDIR}/pg.el -l ${WORKDIR}/pgmacstbl.el -l ${WORKDIR}/pgmacs-sql-keywords.el -l ${WORKDIR}/pgmacs.el --eval '(pgmacs-open-string "dbname=pgeltestdb hostname=localhost port=5439 user=pgeltestuser password=pgeltest")'
	${DOCKER} stop pgsql


# https://hub.docker.com/r/citusdata/citus
#
# An extension for sharding PostgreSQL. We could recognize it by checking for the system tables
# s(pg-qualified-name columnar_internal options) #s(pg-qualified-name columnar_internal stripe)
# #s(pg-qualified-name columnar_internal chunk_group) #s(pg-qualified-name columnar_internal chunk),
# but since we don't see any behavioural difference from standard PostgreSQL, we don't report this
# as a variant.
test-citus: peg
	${DOCKER} run --rm --name citus \
	   --publish 127.0.0.1:5409:5409 \
	   -e POSTGRES_DB=pgeltestdb \
	   -e POSTGRES_USER=pgeltestuser \
	   -e POSTGRES_PASSWORD=pgeltest \
	   -e PGPORT=5409 \
	   -d docker.io/citusdata/citus:latest
	sleep 5
	curl --no-progress-meter -L https://github.com/morenoh149/postgresDBSamples/raw/refs/heads/master/chinook-1.4/Chinook_PostgreSql_utf8.sql \
	| grep -v "^ALTER.*OWNER TO postgres;" \
	| psql postgresql://pgeltestuser:pgeltest@localhost:5409/pgeltestdb
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:5409/pgeltestdb $(MAKE) test
	${DOCKER} stop citus

# A PostgreSQL extension that adds a new storage engine designed for better multithreading and solid
# state storage.
#
# https://github.com/orioledb/orioledb
# https://hub.docker.com/r/orioledb/orioledb
test-orioledb: peg
	${DOCKER} run --rm --name orioledb \
	  --publish 127.0.0.1:5317:5317 \
	   -e POSTGRES_DB=pgeltestdb \
	   -e POSTGRES_USER=pgeltestuser \
	   -e POSTGRES_PASSWORD=pgeltest \
	   -e PGPORT=5317 \
	   -d docker.io/orioledb/orioledb:latest-pg17
	sleep 5
	psql -f tiny-import.sql postgresql://pgeltestuser:pgeltest@localhost:5317/pgeltestdb
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:5317/pgeltestdb $(MAKE) test
	${DOCKER} stop orioledb


# https://docs.ivorysql.org/en/ivorysql-doc/v3.4/v3.4/6#Docker-installation
# An Oracle-compatible flavour of PostgreSQL.
test-ivorysql: peg
	${DOCKER} run --rm --name ivorysql \
	  --publish 5437:5432 \
	  -e LANG=C.UTF8 \
	  -e LC_CTYPE=C.UTF8 \
	  -e IVORYSQL_DB=pgeltestdb \
	  -e IVORYSQL_USER=pgeltestuser \
	  -e IVORYSQL_PASSWORD=pgeltest \
	  -d docker.io/ivorysql/ivorysql:5.0-ubi8
	sleep 5
	curl --no-progress-meter -L https://github.com/morenoh149/postgresDBSamples/raw/refs/heads/master/chinook-1.4/Chinook_PostgreSql_utf8.sql \
	| grep -v "^ALTER.*OWNER TO postgres;" \
	| psql postgresql://pgeltestuser:pgeltest@localhost:5437/pgeltestdb
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:5437/pgeltestdb $(MAKE) test
	${DOCKER} stop ivorysql


# https://docs.timescale.com/self-hosted/latest/install/installation-docker/
#
# Note that Timescaledb has a bunch of internal tables (owned by the currently connected user) in
# schemas named _timescaledb_cache, _timescaledb_catalog, timescaledb_internal and so on. We could
# probably add code in pg-el to not include these in the list of tables.
test-timescale: peg
	${DOCKER} run --rm --name timescale \
	   --publish 5981:5981 \
	   -e POSTGRES_DB=pgeltestdb \
	   -e POSTGRES_USER=pgeltestuser \
	   -e POSTGRES_PASSWORD=pgeltest \
	   -e PGPORT=5981 \
	   -d docker.io/timescale/timescaledb-ha:pg16
	sleep 5
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:5981/pgeltestdb $(MAKE) test
	${DOCKER} stop timescale

test-timescale-chinook: peg
	${DOCKER} run --rm --name timescale \
	   --publish 5981:5981 \
	   -e POSTGRES_DB=chinook \
	   -e POSTGRES_USER=pgeltestuser \
	   -e POSTGRES_PASSWORD=pgeltest \
	   -e PGPORT=5981 \
	   -d docker.io/timescale/timescaledb-ha:pg16
	sleep 5
	curl --no-progress-meter -L https://github.com/morenoh149/postgresDBSamples/raw/refs/heads/master/chinook-1.4/Chinook_PostgreSql_utf8.sql \
	| grep -v "^ALTER.*OWNER TO postgres;" \
	| psql "host=localhost port=5981 dbname=chinook user=pgeltestuser password=pgeltest"
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:5981/chinook $(MAKE) test
	${DOCKER} stop timescale


# Works perfectly; is more an extension than a real variant of PostgreSQL.
test-paradedb: peg
	${DOCKER} run --rm --name paradedb \
	  --publish 5441:5441 \
          -e PGPORT=5441 \
          -e POSTGRESQL_PORT_NUMBER=5441 \
          -e POSTGRESQL_MASTER_PORT_NUMBER=5441 \
          -e POSTGRES_DB=pgeltestdb \
          -e POSTGRES_USER=pgeltestuser \
          -e POSTGRES_PASSWORD=pgeltest \
          -e POSTGRESQL_POSTGRES_PASSWORD=pgeltest \
          -d docker.io/paradedb/paradedb:latest
	sleep 5
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:5441/pgeltestdb $(MAKE) test
	${DOCKER} stop paradedb


# Microsoft DocumentDB (PostgreSQL-based). This is not the same product as Amazon DocumentDB.
# https://github.com/microsoft/documentdb
#
# Here using the prebuilt Docker image provided by FerretDB
# https://docs.ferretdb.io/installation/ferretdb/docker/
#
#            -e POSTGRES_INITDB_ARGS="-c cron.database_name=pgeltestdb" \

# We can't change the database name with POSTGRES_DATABASE as that messes with the initialization of
# the pg_cron extension. Likewise for the POSTGRES_USER because that changes the default database
# name.
#
# This introduces a new type "bson".
test-documentdb: peg
	${DOCKER} run --rm --name documentdb \
	  --publish 5488:5488 \
	   -e POSTGRES_PASSWORD=pgeltest \
	   -e PGPORT=5488 \
	   -d ghcr.io/ferretdb/postgres-documentdb:latest
	sleep 5
	PGURI=postgresql://postgres:pgeltest@127.0.0.1:5488/postgres $(MAKE) test
	${DOCKER} stop documentdb


# Test pgbouncer proxying to our local PostgreSQL (in session pooling mode, so that functionality
# such as LISTEN works).
#
# https://github.com/edoburu/docker-pgbouncer
test-pgbouncer: peg
	${DOCKER} run --rm --name pgbouncer \
	  --net host \
	  -e PGBOUNCER_PORT=6432 \
	  -e PGBOUNCER_DATABASE=pgeltestdb \
	  -e POSTGRESQL_USERNAME=pgeltestuser \
	  -e POSTGRESQL_PASSWORD=pgeltest \
	  -e POSTGRESQL_HOST=localhost \
	  -e POSTGRESQL_PORT=5432 \
	  -d docker.io/bitnami/pgbouncer:latest
	sleep 5
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:6432/pgeltestdb $(MAKE) test
	${DOCKER} stop pgbouncer

# Yugabyte works partially; for example we are not able to run our SQL command that adds a PRIMARY
# KEY column to an existing table, and the total database size which is computed with
# pg_database_size does not work.
#
# Compatibility notes: https://docs.yugabyte.com/preview/develop/postgresql-compatibility/
#
#             --ysql_pg_conf_csv=yb_enable_cbo=on --ysql_yb_enable_cbo
test-yugabyte: peg
	${DOCKER} run --rm --name yugabyte \
	  --net host --pull=newer \
          -e YSQL_USER=pgeltestuser \
          -e YSQL_PASSWORD=pgeltest \
          -e YSQL_DB=pgeltestdb \
          -d docker.io/yugabytedb/yugabyte:latest \
            bin/yugabyted start \
            --advertise_address 127.0.0.1 \
            --ysql_port=5499 \
            --base_dir=/tmp \
            --ysql_enable_auth=true \
            --background=false
	sleep 10
	curl --no-progress-meter -L https://github.com/morenoh149/postgresDBSamples/raw/refs/heads/master/chinook-1.4/Chinook_PostgreSql_utf8.sql \
	| grep -v "^ALTER.*OWNER TO postgres;" \
	| psql postgresql://pgeltestuser:pgeltest@localhost:5499/pgeltestdb
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:5499/pgeltestdb $(MAKE) test
	${DOCKER} stop yugabyte


# https://github.com/ApsaraDB/PolarDB-for-PostgreSQL from Alibaba Cloud
#
# Note: primary runs on $POLARDB_PORT, replicas on ${POLARDB_PORT}+1 and ${POLARDB_PORT}+2
# We don't seem to be able to set POLARDB_DATABASE or equivalent
test-polardb: peg
	${DOCKER} run --rm --name polardb \
          --pull=newer \
          --publish 127.0.0.1:5998:5998 \
          -e POLARDB_PORT=5998 \
          -e POLARDB_USER=pgeltestuser \
          -e POLARDB_PASSWORD=pgeltest \
          -d docker.io/polardb/polardb_pg_local_instance:15
	sleep 10
	psql -f tiny-noprimary-import.sql postgresql://pgeltestuser:pgeltest@127.0.0.1:5998/postgres
	sleep 2
	PGURI=postgresql://pgeltestuser:pgeltest@127.0.0.1:5998/postgres $(MAKE) test
	${DOCKER} stop polardb


# Materialize (here the "Materialize emulator" running in Docker). This proprietary differential
# dataflow database has many limitations in its PostgreSQL compatibility: no support for primary
# keys, unique constraints, check constraints, for the 'bit' type for example.
#
# https://materialize.com/blog/postgres-compatibility/
test-materialize: peg
	${DOCKER} run --rm --name materialize \
          --pull=newer \
          --publish 127.0.0.1:6875:6875 \
          -d docker.io/materialize/materialized:latest
	sleep 10
	psql -f tiny-noprimary-import.sql postgresql://materialize@127.0.0.1:6875/materialize
	sleep 2
	PGURI=postgresql://materialize@127.0.0.1:6875/materialize $(MAKE) test
	${DOCKER} stop materialize


# CrateDB does not allow us to add a PRIMARY KEY to an existing table that contains data. It is not
# possible to use a password for the default crate superuser.
test-cratedb: peg
	${DOCKER} run --rm --name cratedb \
          --pull=newer \
          --net host \
          -e CRATE_HEAP_SIZE=1g \
          -d docker.io/crate/crate:nightly -Cdiscovery.type=single-node -Cpsql.port=5789
	sleep 10
	psql -f tiny-import.sql postgresql://crate@localhost:5789/postgres
	PGURI=postgresql://crate@localhost:5789/postgres $(MAKE) test
	${DOCKER} stop cratedb


test-cratedb-ssl: peg
	openssl req -new -nodes -text -out ${WORKDIR}/root.csr -keyout ${WORKDIR}/root.key \
	   -subj "/CN=localhost"
	chmod og-rwx ${WORKDIR}/root.key
	openssl x509 -req -in ${WORKDIR}/root.csr -text -days 42 \
	   -extfile /etc/ssl/openssl.cnf -extensions v3_ca \
	   -signkey ${WORKDIR}/root.key -out ${WORKDIR}/root.crt
	openssl req -new -nodes -text -out ${WORKDIR}/server.csr -keyout ${WORKDIR}/server.key \
	   -subj "/CN=localhost"
	chmod og-rwx ${WORKDIR}/server.key
	openssl x509 -req -in ${WORKDIR}/server.csr -text -days 42 \
	   -CA ${WORKDIR}/root.crt -CAkey ${WORKDIR}/root.key \
	   -CAcreateserial -out ${WORKDIR}/server.crt
	openssl req -new -nodes -out ${WORKDIR}/client.csr -keyout ${WORKDIR}/client.key \
	   -subj "/CN=pgeltestuser"
	openssl x509 -req -days 42 -in ${WORKDIR}/client.csr \
	   -CA ${WORKDIR}/root.crt \
	   -CAkey ${WORKDIR}/root.key \
	   -CAcreateserial -out ${WORKDIR}/client.crt
	chmod 600 ${WORKDIR}/server.key
	ls -l ${WORKDIR}
	${DOCKER} volume create pgcerts
	tar cf ${WORKDIR}/certs.tar --directory=${WORKDIR} server.crt server.key root.crt
	${DOCKER} volume import pgcerts ${WORKDIR}/certs.tar
	${DOCKER} run --rm --name cratedb \
           --pull=newer \
           --net host \
           -v pgcerts:/certs:U \
           -e CRATE_HEAP_SIZE=1g \
           docker.io/crate:latest -Cdiscovery.type=single-node -Cpsql.port=5789 \
             -Cssl.psql.enabled=true -Cssl.transport.mode=on -Cssl.keystore_filepath=/certs/server.crt \
             -Cssl.keystore_password=/certs/server.key
	sleep 10
	psql -f tiny-import.sl "sslmode=verify-ca sslrootcert=/certs/root.crt sslcert=/certs/client.crt sslkey=/certs/client.key hostaddr=127.0.0.1 port=5789 user=crate dbname=postgres"
	PGEL_TRUST_CA=${WORKDIR}/root.crt PGURI="postgresql://crate@localhost:5789/postgres?sslmode=require" $(MAKE) test-tls
	${DOCKER} stop cratedb
	sleep 2
	${DOCKER} volume rm pgcerts



# We could also run CockroachDB using the "--insecure" commandline argument without specifying
# COCKROACH_DATABASE and so on, in which case the PGURI becomes 
#
#    postgresql://root@localhost:26257/postgres
test-cockroach: peg
	${DOCKER} run --rm --name cockroachdb \
          --pull=newer \
          -v /dev/log:/dev/log \
          --publish 127.0.0.1:26257:26257 \
          -e COCKROACH_DATABASE=pgeltestdb \
          -e COCKROACH_USER=pgeltestuser \
          -e COCKROACH_PASSWORD=pgeltest \
          -d docker.io/cockroachdb/cockroach:latest start-single-node
	sleep 10
	psql -f tiny-import.sql postgresql://pgeltestuser:pgeltest@localhost:26257/pgeltestdb
	PGURI="postgresql://pgeltestuser:pgeltest@localhost:26257/pgeltestdb?sslmode=require" $(MAKE) test
	${DOCKER} stop cockroachdb



# https://cedardb.com/docs/
test-cedardb: peg
	${DOCKER} run --rm --name cedardb \
           --pull=newer \
           --publish 127.0.0.1:6778:5432 \
           -e CEDAR_USER=pgeltestuser \
           -e CEDAR_PASSWORD=pgeltest \
           -e CEDAR_DB=pgeltestdb \
           -d docker.io/cedardb/cedardb:latest
	sleep 10
	psql -f tiny-import.sql postgresql://pgeltestuser:pgeltest@127.0.0.1:6778/pgeltestdb
	PGURI="postgresql://pgeltestuser:pgeltest@127.0.0.1:6778/pgeltestdb" $(MAKE) test
	${DOCKER} stop cedardb


# Google Spanner emulator
# See https://github.com/GoogleCloudPlatform/pgadapter/blob/postgresql-dialect/docs/emulator.md
# https://cloud.google.com/spanner/docs/pgadapter-start#docker
#
# Does not work with PGmacs because it does not implement some functions we use to query the current
# user and database status, such as current_user, pg_backend_pid(), pg_is_in_recovery().
test-spanner: peg
	${DOCKER} run --rm --name spanner \
          --pull=newer \
	  --publish 5499:5432 \
	  -d gcr.io/cloud-spanner-pg-adapter/pgadapter-emulator -d pgeltestdb
	sleep 5
	psql -f tiny-import.sql postgresql://pgeltestuser:pgeltest@localhost:5499/pgeltestdb
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:5499/pgeltestdb $(MAKE) test
	${DOCKER} stop spanner


# Google AlloyDB Omni. This is PostgreSQL with Google-developed extensions, including a columnar
# storage extension, adaptive autovacuum, and an index advisor.
#
# https://cloud.google.com/alloydb/omni/docs/quickstart
# https://hub.docker.com/r/google/alloydbomni
# https://cloud.google.com/alloydb/docs/reference/database-flags
# Seems to ignore the PGPORT environment variable
test-alloydb: peg
	${DOCKER} run --rm --name alloydb \
          --pull=newer \
          --publish 4481:5432 \
          -e POSTGRES_DB=pgeltestdb \
          -e POSTGRES_USER=pgeltestuser \
          -e POSTGRES_PASSWORD=pgeltest \
          -d docker.io/google/alloydbomni:latest -c google_columnar_engine.enabled=on
	sleep 10
	psql -f tiny-import.sql postgresql://pgeltestuser:pgeltest@localhost:4481/pgeltestdb
	PGURI=postgresql://pgeltestuser:pgeltest@127.0.0.1:4481/pgeltestdb $(MAKE) test
	${DOCKER} stop alloydb


# YDB by Yandex
# https://ydb.tech/docs/en/postgresql/docker-connect
test-ydb: peg
	${DOCKER} run --rm --name ydb \
          --pull=newer \
          --publish 127.0.0.1:5411:5432 \
          --publish 127.0.0.1:8765:8765 \
          -e POSTGRES_USER=pgeltestuser \
          -e POSTGRES_PASSWORD=pgeltest \
          -e YDB_EXPERIMENTAL_PG=1 \
          -e YDB_USE_IN_MEMORY_PDISKS=true \
          -d ghcr.io/ydb-platform/local-ydb:nightly
	sleep 10
	psql -f tiny-import.sql postgresql://pgeltestuser:pgeltest@localhost:5411/local
	PGURI=postgresql://pgeltestuser:pgeltest@localhost:5411/local $(MAKE) test
	${DOCKER} stop ydb


# QuestDB doesn't know the OID for the PostgreSQL type "text". It also fails on queries like
# "SELECT 1::integer" because it doesn't recognize integer as a type, and DELETE statements are not
# supported.
#
# https://questdb.io/docs/configuration/
test-questdb: peg
	${DOCKER} run --rm --name questdb \
          --pull=newer \
          --publish 8812:8812 \
          --publish 9000:9000 \
          -e QDB_PG_USER=pgeltestuser \
          -e QDB_PG_PASSWORD=pgeltest \
	  -d docker.io/questdb/questdb:latest
	sleep 5
	psql -f tiny-noprimary-import.sql postgresql://pgeltestuser:pgeltest@127.0.0.1:8812/postgres
	PGURI=postgresql://pgeltestuser:pgeltest@127.0.0.1:8812/postgres $(MAKE) test
	${DOCKER} stop questdb


# RisingWave event streaming database.
# https://github.com/risingwavelabs/risingwave
# Also at ghcr.io/risingwavelabs/risingwave
# https://docs.risingwave.com/get-started/quickstart
#
# Fails on query "SELECT 'gday'::varchar(20)"
#
# Could test TLS support with environment variables RW_SSL_CERT and RW_SSL_KEY.
test-risingwave: peg
	${DOCKER} run --rm --name risingwave \
	  --pull=newer \
	  -e ENABLE_TELEMETRY=false \
	  --publish 127.0.0.1:4566:4566 \
	  -d docker.io/risingwavelabs/risingwave:latest single_node
	sleep 5
	psql -f tiny-noprimary-import.sql postgresql://root@127.0.0.1:4566/dev
	PGURI=postgresql://root@127.0.0.1:4566/dev $(MAKE) test
	${DOCKER} stop risingwave



# Start PostgreSQL with some preloaded data
#
# Available tags are dellstore, iso3166, pagila, sportsdb, usda, world, all and latest
# https://hub.docker.com/r/aa8y/postgres-dataset/
start-preloaded: peg
	${DOCKER} run --rm --name pgsql \
	  --publish 5778:5778 \
	   -e POSTGRES_DB=pgeltestdb \
	   -e POSTGRES_USER=pgeltestuser \
	   -e POSTGRES_PASSWORD=pgeltest \
	   -e PGPORT=5778 \
	   -d docker.io/aa8y/postgres-dataset:latest
	sleep 5
	${EMACS} -Q -l require-pg.el -l ../../pg-el/pg.el -l ../pgmacstbl.el -l ../pgmacs.el --eval '(pgmacs-open-string "dbname=usda hostname=localhost port=5778 user=pgeltestuser password=pgeltest")'
# cd ~/tmp/pgmacs-hang-gist && emacs -Q -l peg.el -l pg.el -l pgmacstbl.el -l pgmacs.el --eval '(pgmacs-open-string "dbname=usda hostname=localhost port=5778 user=pgeltestuser password=pgeltest")'
	${DOCKER} stop pgsql


nativecomp: peg
	${EMACS} -Q -l require-pg.el -l load-nativecomp.el -l test.el -f pgmacs-test
