APP:=harbour
ARGS=""

.PHONY: help
help:
	# -----------------------------------------------------------------------------
	# Targets:
	#
	# ** Local Commands **
	# 	shell 	:	launch service in a shell
	# 	build 	:	build service locally
	# 	start 	:	start service locally
	# 	stop  	:	stop service locally
	# 	package	:	create tar of service locally
	#
	# ** Container Commands **
	# 	cbuild	:	build containerized service via docker compose
	# 	cstart	:	start as containerized service via docker compose
	# 	cstop	:	stop as containerized service via docker compose
	# 	clog	:	tail the containerized service stack via docker compose
	#
	# ** Misc **
	# 	selftest:	run through {build|start|stop} {locally|container}
	#
	# end.
	# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#  LOCAL COMMANDS
# -----------------------------------------------------------------------------
.PHONY: shell
shell:  
	cd ${APP} && $(MAKE) shell ARGS=${ARGS}

.PHONY: build
build:  
	cd ${APP} && $(MAKE) build
	cd installer && $(MAKE) build

.PHONY: release
release:  
	cd ${APP} && $(MAKE) release

.PHONY: start
start:  
	cd ${APP} && $(MAKE) start

.PHONY: stop
stop: 
	cd ${APP} && $(MAKE) stop

.PHONY: attach
attach: 
	cd ${APP} && $(MAKE) attach

.PHONY: package
package: 
	cd ${APP} && $(MAKE) package

.PHONY: clean
clean: 
	rm -rf ${APP}/_build

.PHONY: install
install: 
	cd installer && $(MAKE) run

# -----------------------------------------------------------------------------
#  CONTAINER COMMANDS
# -----------------------------------------------------------------------------
.PHONY: cstart
cstart: 
	cd monitor && $(MAKE) start
	watch docker ps

.PHONY: cstop
cstop: 
	cd monitor && $(MAKE) stop

.PHONY: cbuild
cbuild:  
	docker build -t localtag/${APP} .

.PHONY: clog
clog: 
	cd monitor && docker-compose logs -f --tail="all"

# -----------------------------------------------------------------------------
#  SELFTEST
# -----------------------------------------------------------------------------

.PHONY: selftest
selftest: build release start cbuild stop cstart cstop
