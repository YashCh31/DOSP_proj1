ERL_SRC = $(wildcard *.erl)
BEAM	= $(ERL_SRC:.erl=.beam)

all: $(BEAM)

%.beam: %.erl
	erlc +debug_info $*.erl
