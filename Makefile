.SILENT: state stop

###############################################################################
## Make parameters
###############################################################################
APP=rsp
node=$(APP)
master=undefined
port=80
sport=443
cookie=thisisaninsecurecookie_makeyourown
privdir=priv
datadir=$(privdir)/data
asios=100
procs=1000000

# Compile source codes only.
compile:
	./rebar compile


# Start the server
start: compile
	mkdir -p $(datadir)
	erl -pa ebin deps/*/ebin +A $(asios) +K true +P $(procs) +W w +swt low +Mummc 99999 \
		-sname $(node) -setcookie $(cookie) -mnesia dir '"$(datadir)/$(node)"' \
		-env RSP_MASTER $(master) -env RSP_PORT $(port) -env RSP_SPORT $(sport) \
		-boot start_sasl -s reloader -s $(APP) -detached -config $(APP)\
		-ssl session_lifetime 0

# Stop the server# Debug running program in production mode.
debug:
	erl -pa ebin deps/*/ebin -remsh $(node)@`hostname -s` \
	-sname $(node)_debug -setcookie $(cookie)

stop:
	erl -pa ebin deps/*/ebin -noinput -hidden -setcookie $(cookie) -sname $(node)_control \
		-s rsp_control call $(node)@`hostname -s` stop

# Check the server state
state:
	erl -pa ebin deps/*/ebin -noinput -hidden -setcookie $(cookie) -sname $(node)_control \
		-s rsp_control call $(node)@`hostname -s` state

# Perform unit tests.
test: compile
	./rebar eunit skip_deps=true

# Clear all the binaries and dependencies.  The runtime remains intact.
clean: delete-deps
	./rebar clean

# Clear the runtime.
reset:
	rm -rf $(datadir)/$(node)

# Generate documents.
doc:
	./rebar doc

deps: get-deps
	./rebar update-deps

get-deps:
	./rebar get-deps

delete-deps:
	./rebar delete-deps
