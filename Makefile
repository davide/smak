ERL		?= erl
EBIN_DIRS	:= $(wildcard lib/*/ebin)
APP		:= smak

all: erl
	make -C deps/ewgi all

erl: lib
	@./support/compile.erl ebin src/$(APP).app $(EBIN_DIRS)

docs:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam
	make -C deps/ewgi clean

lib:
	@mkdir lib

dialyzer: erl
	@dialyzer -c ebin

test: erl
	@$(ERL) -pa $(EBIN_DIRS) -pa ebin -noinput +B \
	-eval 'case lists:member(error, smak_test:test()) of true -> halt(1); _ -> halt(0) end.'
