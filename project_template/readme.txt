This is a project template to write a game based on OGLCScene.

1) Copy the content of this folder (with the sub-folders) in your game folder
2) in your game folder, open 'project_oglcscene.lpi' with Lazarus
3) rename it  with Project->Save As-> the name of your project
4) delete all files starting with 'project_oglcscene'
5) in unit "u_common.pas" modify the values of the constant SCREEN_WIDTH_AT_DESIGN_TIME, SCREEN_HEIGHT_AT_DESIGN_TIME and SCREEN_PPI_AT_DESIGN_TIME according to the values the window when you designed the graphics. Also modify the number of layers used by the game.

About scene
-----------
An OGLCScene application use a single instance of TOGLCScene. A variable 'FScene' is declared in unit 'u_common'. This variable is instanciated when the main form is instanciated.
 - a scene contains layers (see oglcLayer.inc).
 - a layer contains surfaces.
 - a surface can be a sprite or any other kind of object that descend of class TSimpleSurfaceWithEffect.

About screens
-------------
There is one screen for one part of the game (for example: screen_title, screen_game, screen_halloffame, etc...) each ones in a separate unit and encapsulated in a class descendant of TScreenTemplate.
TScreenTemplate is implemented in unit oglcScreenTemplate.inc

About texture atlas
-------------------
Why use atlas ?... because it avoid switching opengl texture each time something textured is rendered on the screen. And switching constantly texture is slow. So, it's better to group all the game textures in a single atlas.
 - If your game need only one atlas to contain your game graphics, declare it as global variable in unit 'u_common', build it in MainForm.LoadCommonData and free it in MainForm.FreeCommonData.
 - If a single atlas is not enough to contain your game graphics, declare an atlas variable per screen, build it in the screen CreateObjects method, and free it in the screen FreeObjects method.
TAtlas is declared in unit oglcTextureAtlas.inc
Texture are implemented in unit oglcTexture.inc

About layers
------------
(implemented in oglcLayer.inc)
The layer with index 0 is rendered at last position (on top).
The last layer is rendered first (furthest in the background).
The first surface added in a layer is rendered first (furthest in the background).
The last surface added in a layer is rendered at last position (on top).

About surfaces
--------------
the base class for all kind of surfaces is TSimpleSurfaceWithEffect, implemented in oglcSurface.inc

 - unit oglcSpriteTemplate.inc
   - TSprite: the classic textured surface
   - TSpriteWithElasticCorner: a textured sprite that can be deformed by stretching its corner
   - TPolarSprite: a textured sprite with the ability to be moved with polar coordinate
   - TShapeOutline: a sprite to draw shape outline, antialiased or not
   - TGradientRectangle: a sprite to draw a rectangular gradient
   - TQuad4Color: a sprite to draw a quadrilateral with different color on its corners
   - TSpriteContainer: a sprite container to compose complex sprite
   - TOGLCPathToFollow: a surface to draw a path on screen
   - TSpriteOnPathToFollow: a textured sprite that moves along a TOGLCPathToFollow instance

 - unit oglcFreeText.inc
   - TFreeText: A surface that draw text. The text can be multiline. Lines are separated by #10
   - TFreeTextOnPathToFollow: a text that moves along a TOGLCPathToFollow instance
   - TFreeTextClockLabel: a surface that can count/count down time and show the current time
   - TFreeTextAligned: Draws a text in a specified rectangular area with horizontal align (left, center or right). The text can be multiline: use #10 to separate the lines

 - unit oglcFXScrolledSprite.inc
   - TScrollableSprite: a surface with a texture that can be scrolled. Work only if the texture is not a part of an atlas

 - unit oglcGlow.inc
   - TOGLCGlow: this surface display a colored glow that can be used to add light effects on your scene
   - TOGLCSpotLight: this surface display a colored spot light

 - unit oglcParticle.inc
   - TParticleEmitter: a particle engine

  - unit oglcDeformationGrid.inc
   - TDeformationGrid: a textured surface with a virtual grid, each node can be moved to stretch the image

 - unit oglcElectricalFX.inc
   - TOGLCElectricalBeam: a surface to simulate an electric arc

 - unit oglcEnvironment.inc
   - TWaterSurface: a surface that display water drop (very slow)
   - TSnow: a textured surface to simulate snowfall

 - unit oglcTileEngine.inc
   - TTileEngine: a tile engine