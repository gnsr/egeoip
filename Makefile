REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)
REBAR_FLAGS ?=

travis: test
all: compile

compile:
	$(REBAR) compile $(REBAR_FLAGS)

doc:
	$(REBAR) doc $(REBAR_FLAGS)

test: compile
	$(REBAR) eunit $(REBAR_FLAGS)

clean:
	$(REBAR) clean $(REBAR_FLAGS)

clean_plt:
	@rm -f _test/dialyzer_plt

build_plt: build-plt

build-plt:
	@ [ -d _test ] || mkdir _test
	$(REBAR) build-plt $(REBAR_FLAGS)

dialyzer:
	$(REBAR) dialyze $(REBAR_FLAGS)

update_db:
	wget -N https://geolite.maxmind.com/download/geoip/database/GeoLiteCountry/GeoIP.dat.gz -O ./priv/GeoIP.dat.gz
	wget -N https://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz -O ./priv/GeoLiteCity.dat.gz

console:
	erl -pa ebin/ \
	    -eval "code:load_file(egeoip), \
	           [application:ensure_all_started(App) \
	            || App <- [crypto,asn1,public_key,ssl,inets,egeoip]], \
	           [code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) \
	            || P <- code:get_path(), F <- filelib:wildcard(P ++ \"/*.beam\")]."
