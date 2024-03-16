# OGLCScene
Tiny homemade 2D game engine for FreePascal/Lazarus.  
# Dependencies
You need packages LazOpenGlContext and BGRABitmap installed in the IDE.  
# Release
The current release use OpenGL 3.3 to render graphics.
# Support
- only 2D !
- OpenGL 3.3
- multi layers architecture
- sprites and other kind of surface with automated effect like tint, opacity, moves...
- all kind of surface that can be displayed have script support: they can execute scripts written in external files in order to perform certain actions on the surface.
- particle engine and its tool 'Particles Editor' to make your own particle effects.
- tiled map engine and its tool 'Tile map Designer' usefull to edit and construct your maps.
- velocity curve applied to any value that can be changed in time, like coordinates, rotation, opacity, tint,...
# Credits
A large part of the code of the particle engine comes from ZENGL version 0.3.8 originaly written by Andrey Kemka, and actually maintained by Seenkao https://forum.lazarus.freepascal.org/index.php/topic,49143.0.html

Matrix computation located in file 'gl_core_matrix.inc' was originaly written by Chris Rorden and modified for the use in OGLCScene. Thanks !
You can find the original at https://github.com/neurolabusc/OpenGLCoreTutorials/gl_core_matrix.pas

The code of TWaterSurface in file 'oglcEnvironment.inc' is written by Circular, the creator and maintainer of BGRABitmap library, widly used in OGLCScene.
#Note from the author
I'm an amateur programmer who loves the Pascal language. OGLCScene is an attempt to go as far as possible in my understanding of OpenGL and game engines. The path has been sometimes frustrating, sometimes happy, but very interesting.