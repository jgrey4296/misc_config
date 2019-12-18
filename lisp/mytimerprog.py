from random import random
from time import sleep
import sys

print("Timer test")
count = 0
curr_value = input(":")
while curr_value != 'quit':
    print("Step: {} {}".format(count, curr_value), flush=True)
    if count < 5:
        count += 1
        sleep(5)
    else:
        curr_value = "quit"

print("Quitting Timer Py")
