CA=ca65
LD=ld65
FILENAME=main
LD_FLAGS=--target nes

all:
	$(CA) $(FILENAME).s
	$(LD) $(FILENAME).o $(LD_FLAGS) -o $(FILENAME).nes

run:
	make
	fceux $(FILENAME).nes
