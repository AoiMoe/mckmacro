PFX=i586-mingw32-
SFX=.exe
CXX=$(PFX)g++
DBG=-g
CDEFS=$(DBG) --exec-charset=cp932
LDFLAGS=$(DBG)

SRCS=mckmacro.cpp
OBJS=$(SRCS:.cpp=.o)
PROG=mckmacro$(SFX)

.cpp.o:
	$(CXX) $(CDEFS) -c $<

mckmacro.o: mckmacro.cpp

$(PROG): $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $<

clean:
	-rm $(OBJS) $(PROG)
