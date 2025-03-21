# RAL
Single header fixed point software renderer

## GIF demonstration
![](https://github.com/Ilya3point999K/RAL/blob/main/ral_demo.gif)
![](https://github.com/Ilya3point999K/RAL/blob/main/ral_demo_tex.gif)

Basically a rewrite of [BOOTLEG3D](https://github.com/malespiaut/bootleg3d) to fixed point and local states instead of global state.

Now with a perspective correct texture mapping support! A bit clunky, but still.

Also added support for elegant and precise mouse picking with object buffers, see functions with OBJ postfix.

**ral_demo_tex.c** contains simple textured spinning cube demo.

**cubes.c** contains original benchmark from bootleg3d translated to RAL.

**old** directory contains original RAL implementation, flat color tris only, screenspace clipping. Still usable, if that's all you need. 
