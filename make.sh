gcc -I./include ./tools/mruby-bin-mirb/*.c ./tools/mruby-compiler/*.c ./src/*.c ./mrblib/*.c ./mrbgems/*.c ./mrbgems/mruby-error/*.c ./mrbgems/mruby-error/src/exception.c -lm -o ./bin/mirb.exe

gcc -I./include ./tools/mruby-bin-mruby/*.c ./tools/mruby-compiler/*.c ./src/*.c ./mrblib/*.c ./mrbgems/*.c ./mrbgems/mruby-error/*.c ./mrbgems/mruby-error/src/exception.c -lm -o ./bin/mruby.exe

