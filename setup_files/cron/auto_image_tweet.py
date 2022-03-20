#!/opt/anaconda3/envs/bookmark/bin/python
# Setup root_logger:
from os.path import splitext, split
import logging as root_logger
from os.path import join, isfile, exists, abspath, getsize
from os.path import split, isdir, splitext, expanduser
from os import listdir, system
import subprocess
from random import choice
import json
import twitter
import configparser

LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
TEMP_LOC = "/Volumes/documents/DCIM/__temp/output.jpg"
downsize_cmd = join(split(__file__)[0], "downsize")
convert_cmd = "convert"
conversion_arg = "jpeg:extent=4800KB"

dcim_whitelist_path = join(split(__file__)[0], "dcim_whitelist")

expander = lambda x: abspath(expanduser(x))

def compress_file(filepath):
    #logging.info("Attempting compression of: {}".format(filepath))
    ext = splitext(filepath)[1][1:]

    # retcode = subprocess.call([downsize_cmd,
    #                            "-s", "4900",
    #                            filepath, TEMP_LOC])

    retcode = subprocess.call([convert_cmd, filepath,
                               "-define", conversion_arg,
                               TEMP_LOC])

    if retcode == 0 and getsize(TEMP_LOC) < 5000000:
        return TEMP_LOC
    else:
        logging.warning("Failure converting: {}".format(filepath))
        exit(1)


if __name__ == "__main__":
    #logging.info("Running Auto Image Tweet")
    config = configparser.ConfigParser()
    with open(expander('~/github/py_bookmark_organiser/secrets.config'),'r') as f:
        config.read_file(f)

    twit = twitter.Api(consumer_key=config['DEFAULT']['consumerKey'],
                       consumer_secret=config['DEFAULT']['consumerSecret'],
                       access_token_key=config['DEFAULT']['accessToken'],
                       access_token_secret=config['DEFAULT']['accessSecret'],
                       sleep_on_rate_limit=config['DEFAULT']['sleep'],
                       tweet_mode='extended')

    with open(expander(dcim_whitelist_path), 'r') as f:
        whitelist = f.readlines()

    if not bool(whitelist):
        logging.warning("Nothing to tweet from whitelist")
        exit(1)

    selected = choice(whitelist).split(":")
    if not bool(selected) or not exists(selected[0]):
        quit()

    #logging.info("Attempting: {}".format(selected))
    msg = ""
    the_file = expander(selected[0]).strip()
    if len(selected) > 1:
        msg = selected[1].strip()

    #logging.info(f"File size: {getsize(the_file)}")
    if getsize(the_file) > 4500000:
        the_file = compress_file(the_file)

    assert(getsize(the_file) < 5000000)
    assert(exists(the_file))
    assert(splitext(the_file)[1].lower() in [".jpg", ".png", ".gif"])
    twit.PostUpdate(msg, media=the_file)

    #logging.info("Finished")
