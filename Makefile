ERLC=erlc
SRCDIR=src
BEAMDIR=ebin
INCDIR=inc

all: compile 

compile:
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) -I $(INCDIR) -o $(BEAMDIR) $(SRCDIR)/*.erl ;

clean: 
	@ rm -rf $(BEAMDIR)/*.beam ;
	@ rm -f erl_crush.dump

