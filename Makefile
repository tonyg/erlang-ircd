all: compile

compile:
	./rebar compile

clean:
	./rebar clean
	-rmdir ebin

rel:
	mkdir rel
	(cd rel; ../rebar create-node nodeid=ircd)
	./rebar generate
	chmod a+x rel/ircd/bin/ircd

run: compile
	erl -pa ebin \
		-boot start_sasl \
		-s ircd_app
