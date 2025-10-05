# spindle

A GLSL 450 to SPIRV compiler. Specifically made for taking SDL_Gpu compatible shaders and producing SPIRV blobs. From there [SDL_Shadercross](https://github.com/libsdl-org/SDL_shadercross) can compile your SPIRV blob to any backend shader on the GPU that SDL supports. This enables a truly cross-platform shader solution. Shaders could then be compiled online or offline without dealing with large dependencies such as glslang.

A big annoyance with the current state of SDL_Gpu is the lack of web access. If one were to implement an OpenGL ES 300 renderer, and an SDL_Gpu renderer, they would have a truly cross-platform solution that works today. Since SPIRV Cross (used by SDL_Shadercross) can actually output GLES300, as long as we get a SPIRV blob we have cross-platform shaders unlocked.

**Note**: This project is experimental and, at least for now, almost entirely vibe-coded with Codex. The plan is to get a decent working version going with AI and then massage/refactor the code with human touch afterwards.

# Progress

So far the compiler seems to handle a variety of valid vertex/fragment shader inputs, perform type checking, report errors, and produces IR. Here are some next steps in mind:
- Audit the code as a human and try to identify ways to reduce scope, reduce implementation LoC count/complexity
- Try out a variety of hand-written shaders just to ensure it's all sane and working so far with more real-world use cases
- Get in actual SPIRV output, this will not be optimized and only produce functionally equivalent shader blobs. Backends will be doing their own optimization passes anyways
- If it's not much trouble it might be worth adding in some light optimization passes as a final step
- Again, another final human massaging, audit, refactoring would be needed, especially in designing a user-facing API
