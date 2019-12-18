#http://www.dsclose.com/linux/python/2016/04/26/non-blocking-reads-clearing-stdin-in-python.html
import select
import sys
import time

last_input = ""
while last_input != "quit":
    print("Last Input: {}".format(last_input))
    # print("Position: {}".format(sys.stdin.tell()))
    if sys.stdin in select.select([sys.stdin],[],[], 0.2)[0]:
        last_input = sys.stdin.readline().strip()
    else:
        print("Waiting\n", flush=True)
        time.sleep(2)

print("Quitting")
