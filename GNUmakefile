ifdef CROSS
PFX=i586-mingw32-
endif
SFX=.exe
CXX=$(PFX)g++
DBG=-g
CDEFS=$(DBG) --exec-charset=cp932 -std=c++11
LDFLAGS=$(DBG)

SRCS=mckmacro.cpp
OBJS=$(SRCS:.cpp=.o)
PROG=mckmacro$(SFX)

all: $(PROG)

.cpp.o:
	$(CXX) $(CDEFS) -c $<

mckmacro.o: mckmacro.cpp

$(PROG): $(OBJS)
	$(CXX) -static $(LDFLAGS) -o $@ $<

clean:
	-rm -f $(OBJS) $(PROG)
