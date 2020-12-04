SRCDIR=./src
LIBS=USFs

all: $(LIBS)

%:$(SRCDIR)/%.F90
	elmerf90 $^ -o $@

clean:
	rm -f $(LIBS)
