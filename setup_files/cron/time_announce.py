#!/usr/bin/env python3

from os import system
from datetime import datetime

out_format = "%I:%M %p"

if __name__ == "__main__":
    now = datetime.utcnow()
    system('say -v Moira -r 50 "The Time is {}"'.format(now.strftime(out_format)))
