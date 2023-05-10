ERLC=$(shell which erlc)
ERL_SOURCES=$(wildcard *.erl)
ERL_OBJECTS=$(ERL_SOURCES:%.erl=../ebin/%.beam)
ERLC_FLAGS= -I ../..

all: $(ERL_OBJECTS)

clean:
	rm -f $(ERL_OBJECTS) *.img

../ebin/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
