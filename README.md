# Chip 8 Emulation in Zig

## Status: Partial Complete 🟢

A simple Chip 8 emulator written in zig (0.14.0).

## Install Source + Build

```
git clone https://github.com/alfiehiscox/chip-8.git
cd chip-8
zig build
```

## Usage 

After building run: 

```
./zig-out/bin/chip-8 <path_to_ch8_file>
```

Configuration is only achieved through tweaking the executable. 

Example after downloading [this breakout rom](https://github.com/badlogic/chip8/blob/master/roms/breakout.rom) 
into the source directory and building:

```
./zig-out/bin/chip-8 ./breakout.ch8
```

![Demo](https://github.com/alfiehiscox/chip-8/blob/main/demo.gif)

## Resources

[Guide](https://tobiasvl.github.io/blog/write-a-chip-8-emulator/)

[raylib](https://www.raylib.com/cheatsheet/cheatsheet.html)

## Current Status

The emulator works with a raylib device ctx and should be able to 
exectute most programs (although this has not be rigourously tested). 

Only spec'd for intel macOS on `zig 0.14.0`.

There was a push to have a general interface for different devices. 
You can see this in the `main` function in `src/main.zig`. The idea 
being you provide you're own graphics and system peripheral implementations 
to the emulator which executes them and doesn't really care. 

There are two current device ctxs. Raylib works fine. There is also a 
terminal based one, which will draw and update a game loop in the the terminal 
using ASCII art. The problem is with registering key presses at the same time 
which needs to be done in a separate thread. I started on this with the 
KeyQueue implementations found in the repository but never got it to 
work. 

I don't currently have the time to work on this project so moving it 
into a `partial-complete` status and using the `main` path with Raylib 
for demo purposes. 

