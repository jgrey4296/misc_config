# -*- mode: snippet -*-
# name: setup
# key: setup
# group: cairo
# --
#import cairocffi as cairo #For py 3.6
import cairo               #for py 3.5

N = $1
X = pow(2,N)
Y = pow(2,N)
surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, X,Y)
ctx = cairo.Context(surface)
ctx.scale(X,Y) #coords in 0-1 range

$0
