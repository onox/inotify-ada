CFLAGS  ?= -O2 -march=native

GNATMAKE    = gprbuild -dm -p
GNATCLEAN   = gprclean -q
GNATINSTALL = gprinstall
GNATPP      = gnatpp -q -cl2 -c3 -i3 -M79 -nM -neM -ntM -nnM -N --use-on-new-line -t -dd --eol=lf -rf

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

installcmd = $(GNATINSTALL) -p \
	--sources-subdir=$(includedir) \
	--project-subdir=$(gprdir) \
	--lib-subdir=$(libdir) \
	--ali-subdir=$(alidir) \
	--prefix=$(PREFIX)

.PHONY: build examples debug profile format clean install uninstall

build:
	$(GNATMAKE) -P tools/inotify_ada.gpr -cargs $(CFLAGS)

examples:
	$(GNATMAKE) -P tools/examples.gpr -cargs $(CFLAGS)

debug:
	$(GNATMAKE) -P tools/inotify_ada.gpr -XMode=debug -cargs $(CFLAGS)

profile:
	$(GNATMAKE) -P tools/inotify_ada.gpr -XMode=profiling -cargs $(CFLAGS)

format:
	$(GNATPP) -P tools/inotify_ada.gpr -XMode=debug -cargs $(CFLAGS)
	rm **/*.npp

clean:
	$(GNATCLEAN) -P tools/inotify_ada.gpr
	rm -rf bin build

install:
	$(installcmd) -f --install-name='inotify-ada' -P tools/inotify_ada.gpr

uninstall:
	$(installcmd) --uninstall --install-name='inotify-ada' -P tools/inotify_ada.gpr
