all: c-struct sdl2-defs sdl2-basics sdl2-timer sdl2-events sdl2-video sdl2-ttf

sdl2-defs: sdl2-defs.scm
	csi -s gendefs.scm > sdl2-defs.scm
	csc -s sdl2-defs.scm -emit-all-import-libraries
	csc -s sdl2-defs.import.scm

sdl2-basics: sdl2-basics.scm
	csc -s sdl2-basics.scm -emit-all-import-libraries -lSDL2
	csc -s sdl2-basics.import.scm

sdl2-timer: sdl2-timer.scm
	csc -s sdl2-timer.scm -emit-all-import-libraries -lSDL2
	csc -s sdl2-timer.import.scm

sdl2-events: sdl2-events.scm
	csc -s -S sdl2-events.scm -emit-all-import-libraries -lSDL2
	csc -s sdl2-events.import.scm

sdl2-video: sdl2-video.scm
	csc -k -s -S sdl2-video.scm -emit-all-import-libraries -lSDL2
	csc -s sdl2-video.import.scm

sdl2-ttf: sdl2-ttf.scm
	csc -s -S sdl2-ttf.scm -emit-all-import-libraries -lSDL2 -lSDL2_ttf -k
	csc -s sdl2-ttf.import.scm

c-struct:
	csc -s -S c-struct.scm -emit-all-import-libraries
	csc -s c-struct.import.scm

allc:
	gcc sdl.c -DSDL_MAIN_HANDLED -lSDL2 -lSDL2main