cd ..

rebar3 compile &&

rebar3 eunit && #-mpotato_cluster_test &&    

cd web_files &&

erl -pa main_ebin -pa jsx_ebin -noshell -s potato_cluster start_web_cluster ./www/single_verifier_localhost_config.json file_logs_codes

