#!/opt/anaconda3/envs/bookmark/bin/python
# Setup root_logger:
from os.path import splitext, split
import logging as root_logger
from os.path import join, isfile, exists, abspath
from os.path import split, isdir, splitext, expanduser
from os import listdir, system
import subprocess
from random import choice
import json

from bibtexparser import customization as c
from bibtexparser.bparser import BibTexParser
import bibtexparser as b

LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################
MAX_ATTEMPTS = 20
TWURL_CMD = "twurl"
TWURL_TARGET = "/1.1/statuses/update.json"
BIBTEX_LOC = "~/github/writing/resources/bibliography"
REQUIRED_KEYS = ["year", "author", "title", "tags"]
ONE_OF_KEYS = ["doi", "url", "isbn"]
tweeted_log = "~/github/writing/resources/bibliography/.emacs_tweet_rand_bib_log"

expander = lambda x: abspath(expanduser(x))

def select_bibtex():
    logging.info("Selecting bibtex")
    bibs = [x for x in listdir(expander(BIBTEX_LOC)) if splitext(x)[1] == ".bib"]
    selected = join(BIBTEX_LOC, choice(bibs))
    return selected

def parse_bibtex(file_path):
    logging.info("Parsing: {}".format(file_path))
    with open(expander(file_path)) as f:
        database = b.load(f)

    return database

def select_entry(db, already_tweeted):
    logging.info("Selecting Entry")
    entry = None
    tried_alts = 0

    while entry is None and tried_alts < len(db.entries) and tried_alts < MAX_ATTEMPTS:
        poss_entry = choice(db.entries)
        tried_alts += 1

        has_keys = all([x in poss_entry for x in REQUIRED_KEYS])
        one_of = any([x in poss_entry for x in ONE_OF_KEYS])
        not_tweeted_before = poss_entry['ID'] not in already_tweeted

        if has_keys and one_of and not_tweeted_before:
            entry = poss_entry

    if entry is None:
        logging.warning("No Appropriate Entry Found for db")
        exit()

    return entry

def format_tweet(entry):
    # TODO convert strings to appropriate unicode
    logging.info("Formatting Entry")

    author = entry['author']
    if len(author) > 30:
        author = f"{author[:30]}..."

    result = f"{entry['title']}\n"
    result += f"({entry['year']}) : {entry['author']}\n"
    if "doi" in entry:
        result += f"DOI: https://doi.org/{entry['doi']}\n"
    elif "url" in entry:
        result += f"url: {entry['url']}\n"
    elif "isbn" in entry:
        result += f"isbn: {entry['isbn']}\n"
    else:
        logging.warning("Bad Identifier")
        exit()

    tags = " ".join(["#{}".format(x.strip()) for x in entry['tags'].split(',')])
    result += f"{tags}\n"
    result += "#my_bibtex"
    if len(result) >= 250:
        logging.warning(f"Resulting Tweet too long: {len(result)}")
        exit()

    return (entry['ID'], result)

def call_twurl(tweet_text):
    logging.info(f"Tweeting: {tweet_text}")
    full_arg = f"status={tweet_text}"
    result = subprocess.run([TWURL_CMD,
                             "-d",
                             full_arg,
                             TWURL_TARGET],
                            capture_output=True
                            )
    twurl_output = json.loads(result.stdout)

    if 'errors' not in twurl_output:
        return True

    else:
        error_obj = twurl_output['errors'][0]
        err_code = error_obj['code']
        err_msg = error_obj['message']
        logging.warning(f"Twurl Failed: ({err_code}) {err_msg}")
        system("say Auto-Tweet Failed")
    return False

if __name__ == "__main__":
    logging.info("Running Auto Bibtex Tweet")
    with open(expander(tweeted_log)) as f:
        tweeted = [x.strip() for x in f.readlines()]

    bib        = select_bibtex()
    db         = parse_bibtex(bib)
    entry      = select_entry(db, tweeted)
    id_str, tweet_text = format_tweet(entry)
    if call_twurl(tweet_text):
        with open(expander(tweeted_log), 'a') as f:
            f.write(f"{id_str}\n")
        logging.info("Completed")
