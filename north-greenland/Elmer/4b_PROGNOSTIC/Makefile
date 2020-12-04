SRCDIR=./src
LIBS=SMB_A SUB_MELT Scalar_OUTPUT

all: $(LIBS)

%:$(SRCDIR)/%.F90
	elmerf90 $^ -o $@

clean:
	rm -f $(LIBS)
