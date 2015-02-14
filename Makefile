mckmacro.exe: mckmacro.cpp
	cl -O -GX mckmacro.cpp
	@echo.

clean:
	-del mckmacro.obj 2> nul
	-del mckmacro.exe 2> nul
	@echo.
