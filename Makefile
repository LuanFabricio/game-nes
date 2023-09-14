CC=$(CL65)
FILENAME=main
FLAGS=--verbose --target nes

all:
	$(CC) $(FILENAME).s $(FLAGS) -o $(FILENAME).nes

run:
	make
	fceux $(FILENAME).nes
