# OGLCScene
2D game engine for FreePascal/Lazarus. Tested under Windows and Linux. Should work on MacOS.  
# Dependencies
You need packages LazOpenGlContext and BGRABitmap installed in the IDE.  
# Release
The current release use OpenGL 3.3 to render graphics.
# Support
- Only 2D !
- OpenGL 3.3
- Renderers with batch system to improve performance.
- Multi layers architecture.
- Multi screens architecture to separates the different parts of the game.
- Sprites and other kind of surface with self-managed effects like tint, opacity, moves, angle...
- UI with Button, Label, Image, Panel, CheckBox, RadioButton, ScrollBar, ProgressBar, ListBox, ScrollBox, TextArea.
- UI support nested clipping and customizable border and filling with solid color or gradient color.
- All kind of surface that can be displayed have scenario (script) support: they can execute scenarios written in external files or programmatically in order to perform actions on the surface: 48 commands available to perform Rotation, Moves, Scale, Blink, Tint, Opacity, Frame Animation, Flip, Loop, Goto, Label declaration, Wait and Send Message.
- Messages system to synchronize surfaces between them.
- Particle engine and its tool 'Particles Editor' to make your own particle effects.
- Tiled map engine and its tool 'Tile map Designer' usefull to edit and construct your tiled maps.
- Velocity curve applied to any value that can be changed in time, like coordinates, rotation, opacity, tint,...
- Log file.
# Credits
A large part of the code of the particle engine comes from ZENGL version 0.3.8 originaly written by Andrey Kemka, and actually maintained by Seenkao https://forum.lazarus.freepascal.org/index.php/topic,49143.0.html

Matrix computation located in file 'gl_core_matrix.inc' was originaly written by Chris Rorden and modified for the use in OGLCScene. The file 'glcorearb.pas' was also written by Chris Rorden. Thanks !
You can find the original at https://github.com/neurolabusc/OpenGLCoreTutorials/gl_core_matrix.pas

The code of TWaterSurface in file 'oglcEnvironment.inc' is written by Circular, the creator and maintainer of BGRABitmap library, widly used in OGLCScene. https://github.com/bgrabitmap/bgrabitmap
# Note from the author
I'm an amateur programmer who loves the Pascal language. OGLCScene is an attempt to go as far as possible in my understanding of OpenGL and game engines. The path has been sometimes frustrating, sometimes happy, but very interesting.