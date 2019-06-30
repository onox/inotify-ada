CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

GNATMAKE    = gprbuild -dm -p
GNATCLEAN   = gprclean -q
GNATINSTALL = gprinstall
GNATPP      = gnatpp -q -cl2 -c3 -i3 -M79 -nM -neM -ntM -nnM -N --use-on-new-line -t -dd --eol=lf -rf

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

.PHONY: build examples debug profile format clean install

build:
	$(GNATMAKE) -P inotify_ada.gpr -cargs $(CFLAGS) -largs $(LDFLAGS)

examples:
	$(GNATMAKE) -P examples.gpr -cargs $(CFLAGS) -largs $(LDFLAGS)

debug:
	$(GNATMAKE) -P inotify_ada.gpr -XMode=debug -cargs $(CFLAGS) -largs $(LDFLAGS)

profile:
	$(GNATMAKE) -P inotify_ada.gpr -XMode=profiling -cargs $(CFLAGS) -largs $(LDFLAGS)

format:
	$(GNATPP) -P inotify_ada.gpr -XMode=debug -cargs $(CFLAGS)
	rm **/*.npp

clean:
	$(GNATCLEAN) -P inotify_ada.gpr
	rm -rf bin lib obj

install:
	$(GNATINSTALL) --relocate-build-tree -p -q -f --install-name='inotify-ada' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P inotify_ada.gpr
