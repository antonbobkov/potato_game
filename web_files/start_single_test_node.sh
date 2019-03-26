cd ..
rebar3 compile &&
rebar3 eunit &&    
cd web_files &&
erl -pa ebin -noshell -s start_node start ./www/single_verifier_localhost_config.json

