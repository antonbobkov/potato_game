cd ..

rebar3 compile &&

# rebar3 eunit &&    

cd web_files &&

erl -pa ebin -pa jsx_ebin -noshell -s potato_cluster start_web_cluster ./www/single_verifier_localhost_config.json file_logs
# erl -pa ebin -noshell -s potato_cluster hi

