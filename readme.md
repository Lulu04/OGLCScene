# OGLCScene
Tiny homemade 2D game engine for FreePascal/Lazarus.  
# Dependencies
You need packages LazOpenGlContext and BGRABitmap installed on the IDE.  
# Release
The current release use legacy OpenGL 1.2 "glBegin" / glEnd" to render graphics.
A new version based on OpenGL 3.3 will be available soon.  
# Support
- only 2D !
- multi layers architecture
- sprites and other kind of surface with automated effect like tint, opacity, moves...
- all kind of surface that can be displayed have script support: they can execute scripts written in external files in order to perform certain actions on the surface.
- particle engine and its tool 'OGLCScene Particles Editor' to make your own particle effects.
- tiled map engine and its tool 'Tile map Designer' usefull to edit and construct your maps.
- velocity curve (like ease effect) applied to anything that can be changed in time, like coordinates, rotation, opacity, tint,...
        
