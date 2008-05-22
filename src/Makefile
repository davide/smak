include ../support/include.mk

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

test: all
	$(ERL) -noshell -pa ../ebin -s smak test -s init stop
