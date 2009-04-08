LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
APP_NAME="merle"
VSN="0.3"
CFLAGS+=-fPIC -W -Wall -Werror

all: compile drivermacos

macos: compile drivermacos

docs:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" '"."' '$(VSN)' -s init stop

drivermacos:
	gcc $(CFLAGS) -DMACOSX -I. -O3 -lm -lketama -o priv/ketama_erlang_driver src/ketama_erlang_driver.c
    
driver:
	gcc $(CFLAGS) -I. -O3 -lm -lketama -o priv/ketama_erlang_driver src/ketama_erlang_driver.c

compile:
	@mkdir -p ebin
	@erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

test: all
	prove -v t/*.t

install: all
	mkdir -p ${LIBDIR}/${APP_NAME}-${VSN}/ebin
	mkdir -p ${LIBDIR}/${APP_NAME}-${VSN}/priv
	install priv/ketama_erlang_driver $(LIBDIR)/${APP_NAME}-${VSN}/priv/ketama_erlang_driver
	for i in ebin/*.beam; do install $$i $(LIBDIR)/${APP_NAME}-${VSN}/$$i ; done
