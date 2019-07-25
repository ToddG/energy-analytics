APP:=parse_xml
ARGS=""

.PHONY: help
help:
	# -----------------------------------------------------------------------------
	# Targets:
	#
	# 	shell 	:	launch service 'installer' in a shell
	# 	clean 	:	clean service 'installer'
	# 	build 	:	build service 'installer'
	# 	start 	:	start service 'installer'
	# 	stop  	:	stop service 'installer'
	# 	release	:	release service 'installer'
	# 	package	:	tar service 'installer'
	#
	# end.
	# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# COMMANDS
# -----------------------------------------------------------------------------
.PHONY: clean
clean:  
	rebar3 clean

.PHONY: build
build:  
	rebar3 escriptize

.PHONY: shell
shell:  
	rebar3 shell console

.PHONY: run
run: build 
	# _build/default/bin/${APP}  ~/scratch/parsing-xml/foo.xml
	_build/default/bin/${APP}  ~/scratch/parsing-xml/foo-short.xml /appdata/db/sqlite3/oasis.db
