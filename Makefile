GHC = ghc 
BUILDDIR = build 
GHCFLAGS = -W 
GHCBUILDFLAGS = -isrc -odir $(BUILDDIR) -hidir $(BUILDDIR)

SOURCE = $(shell find src/ -name "*.hs")

EXECUTABLE = dollar 

all: $(EXECUTABLE) 

$(EXECUTABLE): $(SOURCE)
	$(GHC) $(GHCFLAGS) $(GHCBUILDFLAGS) src/Main.hs -o $(EXECUTABLE)

doc: $(SOURCE)
	haddock --html -o doc $(SOURCE)

clean: 
	rm -rf build/* $(EXECUTABLE) examples/*.bc0 doc
