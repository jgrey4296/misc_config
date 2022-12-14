#!/usr/bin/env -S godot --no-window -s
##-- setup
class_name Test
extends SceneTree

"""

"""
##-- end setup

func _init():
    # Constructor
    print("init")
    atest()
    quit()

func atest():
    print("a function")
