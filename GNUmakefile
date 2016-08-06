ifdef CROSS
PFX=i586-mingw32-
endif
SFX=.exe
CXX=$(PFX)g++
DBG=-g
CDEFS=$(DBG) --exec-charset=cp932 -std=c++11 -Wall -Wextra -Werror -pedantic
LDFLAGS=$(DBG)

SRCS=mckmacro.cpp
OBJS=$(SRCS:.cpp=.o)
PROG=mckmacro$(SFX)
EXPORT=mckmacro
VERSION=20160807_00
EXPORTDIR= $(EXPORT)-$(VERSION)
EXPORTZIP= $(EXPORT)-$(VERSION).zip
EXPORTFILES= $(PROG) $(SRCS) GNUmakefile README-ja

all: $(PROG)

.PHONY: export export-zip export-clean

.cpp.o:
	$(CXX) $(CDEFS) -c $<

mckmacro.o: mckmacro.cpp

$(PROG): $(OBJS)
	$(CXX) -static $(LDFLAGS) -o $@ $<

clean:
	rm -f $(OBJS) $(PROG)

export: $(PROG)
	rm -rf $(EXPORTDIR)
	mkdir $(EXPORTDIR)
	for i in $(EXPORTFILES); do cp $$i $(EXPORTDIR)/; done

export-zip: export
	rm -f $(EXPORTZIP)
	zip -r $(EXPORTZIP) $(EXPORTDIR)

export-clean:
	rm -rf $(EXPORTDIR)
	rm -f $(EXPORTZIP)
