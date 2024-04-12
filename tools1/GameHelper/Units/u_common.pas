unit u_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLCScene;


const

// Scene layers
LAYER_COUNT = 3;
   LAYER_UI     = 0;
   LAYER_OBJECT = 1;
   LAYER_BG     = 2;


{
 - ajout d'une texture à l'atlas
 - suppression d'une texture de l'atlas

 - nouvel écran
 - load/save écran
 - un écran a une grille réglable. les objets sont alignés dessus.
 - ajout d'un object sur l'écran. l'objet peut être n'importe lequel des successeur de TSimpleSurfaceWithEffect
 - sélection d'un objet à l'écran, déplacement à la souris
 - fenêtre flottante des propriétés de l'objet sélectionné
           - X, Y, Tint...

 - édition de UIGradient
}


var
  FScene: TOGLCScene;
  FLayerCount: integer=2;
  LayerNames: TStringArray;


implementation


end.

