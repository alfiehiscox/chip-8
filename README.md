# Chip 8 Emulation in Zig

A simple Chip 8 emulator written in zig (0.14.0).

## Install

```
git clone https://github.com/alfiehiscox/chip-8.git
cd chip-8
zig build run
```

## Resources

[Guide](https://tobiasvl.github.io/blog/write-a-chip-8-emulator/)

[raylib](https://www.raylib.com/cheatsheet/cheatsheet.html)

## Roadmap  

- Extract the emulator out of the global space
    - An emulator needs to have a screen and memory (both []u8).
    - An emulator needs to have a registers, timers and stack.
    - This emulator should make sure to run at 60FPS. 
    - The emulator manipulates registers and the screen. 
    - The renderer takes the screen and renders it out to the display. 
- Create a custom generic stack implementation
- Use a general allocator instead of global state
- Use a general renderer instead of hardcoded raylib. 
    - The emulator construct can be fed a renderer interface. 
    - Write an renderer interface for raylib, GLFW and the terminal
- Test the entire project (this becomes easier when you extract the emulator)

