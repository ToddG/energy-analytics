APP:=harbour
ARGS=""

.PHONY: help
help:
	# -----------------------------------------------------------------------------
	# Targets:
	#
	# 	shell 	:	launch service 'harbour' in a shell
	# 	clean 	:	clean service 'harbour'
	# 	build 	:	build service 'harbour'
	# 	start 	:	start service 'harbour'
	# 	stop  	:	stop service 'harbour'
	# 	release	:	release service 'harbour'
	# 	package	:	tar service 'harbour'
	#
	# end.
	# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# COMMANDS
# -----------------------------------------------------------------------------
.PHONY: shell
shell:  
	rebar3 shell ${APP} ${ARGS}

.PHONY: clean
clean:  
	rebar3 clean

.PHONY: build
build:  
	rebar3 do clean, compile, eunit, dialyzer

.PHONY: release
release:  
	# dev release
	rebar3 release 

.PHONY: start
start: release 
	# dev release
	./_build/default/rel/${APP}/bin/${APP} start

.PHONY: stop
stop: 
	# dev release
	./_build/default/rel/${APP}/bin/${APP} stop

.PHONY: attach
attach: 
	# dev release
	./_build/default/rel/${APP}/bin/${APP} attach

.PHONY: console
console: 
	# dev release
	./_build/default/rel/${APP}/bin/${APP} console

.PHONY: package
package: 
	# prod release
	rebar3 as prod tar
