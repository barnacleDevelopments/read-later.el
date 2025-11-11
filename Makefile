# Makefile for read-later documentation

.PHONY: all info html pdf clean install

# Default target builds the Info manual
all: info

# Build Info format (for Emacs)
info: read-later.info

read-later.info: read-later.texi
	makeinfo read-later.texi

# Build HTML documentation
html: read-later.html

read-later.html: read-later.texi
	makeinfo --html --no-split read-later.texi

# Build PDF documentation
pdf: read-later.pdf

read-later.pdf: read-later.texi
	texi2pdf read-later.texi

# Install Info file to Emacs info directory
# You may need to adjust INFODIR for your system
INFODIR ?= ~/.local/share/info

install: read-later.info
	mkdir -p $(INFODIR)
	install -m 644 read-later.info $(INFODIR)/
	install-info --info-dir=$(INFODIR) $(INFODIR)/read-later.info

# Clean generated files
clean:
	rm -f read-later.info
	rm -f read-later.html
	rm -f read-later.pdf
	rm -f read-later.aux read-later.cp read-later.fn read-later.ky
	rm -f read-later.log read-later.pg read-later.toc read-later.tp
	rm -f read-later.vr read-later.cps read-later.fns

# Help target
help:
	@echo "Available targets:"
	@echo "  make info    - Build Info documentation (default)"
	@echo "  make html    - Build HTML documentation"
	@echo "  make pdf     - Build PDF documentation"
	@echo "  make install - Install Info file to $(INFODIR)"
	@echo "  make clean   - Remove generated files"
	@echo "  make help    - Show this help message"