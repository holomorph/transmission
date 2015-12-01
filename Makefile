VERSION = $(shell git describe --tags)

SRC = transmission.el
DISTFILES = Makefile $(SRC) LICENSE NEWS README.org

PREFIX = /usr/local
datarootdir := $(PREFIX)/share
emacsdir := $(datarootdir)/emacs/site-lisp

EMACS = emacs
# EMACSFLAGS = -f package-initialize

all: $(SRC:.el=.elc)

clean:
	$(RM) $(SRC:.el=.elc)

dist: clean
	mkdir transmission-$(VERSION)
	cp -r $(DISTFILES) transmission-$(VERSION)
	tar czf transmission-$(VERSION).tar.gz transmission-$(VERSION)
	rm -rf transmission-$(VERSION)

install:
	install -d $(DESTDIR)$(emacsdir)
	install -m644 $(SRC:=.gz) $(SRC:.el=.elc) -t $(DESTDIR)$(emacsdir)

.el.elc:
	$(EMACS) -batch $(EMACSFLAGS) -f batch-byte-compile $<

%.gz: %
	gzip -k $<

.PHONY: all clean dist install
