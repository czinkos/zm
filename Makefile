ERLC=erlc
SRCDIR=src
BEAMDIR=ebin
INCDIR=inc
REBAR=./rebar

all: clean compile doc

compile: 
	@ $(REBAR) compile ;

clean: 
	@ $(REBAR) clean ;

doc: 
	@ $(REBAR) doc ;
