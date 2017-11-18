# lucid
The λucid typesetting system.

## Composition
Composition is the fundamental construct in λucid (aka "lucid").  Rendered images are represented as transforms ColorPlane -> ColorPlane.  To obtain an actual render, apply this transform to the all black, zero alpha infinite plane.

## Fundamental Composition Operators
atop -> place an image on top of another
under -> place an image under another
xor -> xor two images
add
sub
mul
div -> add/subtract/multiply/divide the images
mask -> zero out an image outside the support of another

## Concepts
support f = supp f: closure of set of f such that f /= id
bound f: smallest bounding box containing supp f
