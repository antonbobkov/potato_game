cd ..
rebar3 compile &&
cd web_files &&
erl -pa ebin -noshell -s hello_web start
