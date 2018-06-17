# OGLCScene
2D game library written with Lazarus

  This library use OpenGL through TOpenGLControl. I got inspired by ZENGL written by Andrey Kemka.
  
  This is not modern OpenGL programming. It use "glBegin" / glEnd" batch to render graphics.
  It use also BGRABitmap to load and prepare some images like text with nice decorations.
  Thanks to Circular and Handoko for their help !
  
Support:
  - only 2D scene
  - multi layers
  - sprites and other kind of surface with automated effect like tint, opacity, moves...
  - all kind of surface that can be displayed have scenario support: they can execute scripts written in external files
    in order to perform certain actions on the surface.
  - particle engine and its tool 'OGLCScene Particles Editor' to make your own particle effects.
  - tiled map engine and its tool 'Tile map Designer' usefull to edit and construct your maps.
  - velocity curve (like ease effect) applied to anything that can be changed in time, like coordinates, rotation, opacity, tint,...
        
