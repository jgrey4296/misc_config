#!/Users/johngrey/anaconda3/bin/python3
from time import sleep
import sys
print("Testing")

curr_value = input(": ")
while curr_value != "quit":
    print("Got Value: {}".format(curr_value))
    # print("Test", file=sys.stderr)
    curr_value = input(": ")

print("Quitting")
