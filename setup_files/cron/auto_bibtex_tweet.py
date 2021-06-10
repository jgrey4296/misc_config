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
import twitter
import configparser

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
MAX_ATTEMPTS     = 20
BIBTEX_LOC       = "~/github/writing/resources/bibliography"
SECRETS_LOC      = '~/github/py_bookmark_organiser/secrets.config'

tweeted_log      = "~/github/writing/resources/bibliography/.emacs_tweet_rand_bib_log"
too_long_log     = "~/.doom/setup_files/cron/rejected_tweets.log"
bibtex_blacklist = "~/.doom.d/setup_files/cron/bibtex_blacklist"

REQUIRED_KEYS    = ["year", "author", "title", "tags"]
ONE_OF_KEYS      = ["doi", "url", "isbn"]

expander = lambda x: abspath(expanduser(x))

def select_bibtex():
    # logging.info("Selecting bibtex")
    # load blacklist
    with open(expander(bibtex_blacklist), 'r') as f:
        blacklist = [x.strip() for x in f.readlines() if bool(x.strip())]


    bibs = [x for x in listdir(expander(BIBTEX_LOC)) if splitext(x)[1] == ".bib"]
    filtered = [x for x in bibs if x not in blacklist]

    assert(len(filtered) < len(bibs))
    selected = join(BIBTEX_LOC, choice(filtered))

    return selected

def parse_bibtex(file_path):
    # logging.info("Parsing: {}".format(file_path))
    with open(expander(file_path)) as f:
        database = b.load(f)

    return database

def select_entry(db, already_tweeted, filename):
    # logging.info("Selecting Entry")
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
        logging.warning(f"No Appropriate Entry Found for db: {filename}")

    return entry

def maybe_blacklist_file(db, file_path, already_tweeted):
    has_fields       = lambda poss_entry: any([x in poss_entry for x in ONE_OF_KEYS])
    not_tweeted_yet  = lambda poss_entry: poss_entry['ID'] not in already_tweeted

    sufficient_entry = lambda entry: has_fields(entry) and not_tweeted_yet(entry)

    with open(expander(bibtex_blacklist), 'r') as f:
        blacklisted = [x.strip() for x in f.readlines()]

    assert(file_path not in blacklisted)
    if not any([sufficient_entry(x) for x in db.entries]):
        logging.info(f"Bibtex failed check, blacklisting: {file_path}")
        with open(expander(bibtex_blacklist), 'a') as f:
            f.write(f"{split(file_path)[1]}\n")

    exit()


def format_tweet(entry):
    # TODO convert strings to appropriate unicode
    # logging.info("Formatting Entry")

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
        logging.warning(f"Bad Entry: {entry['ID']}")
        exit()

    tags = " ".join(["#{}".format(x.strip()) for x in entry['tags'].split(',')])
    if len(result) <= 250:
        diff = 250 - len(result)
        result += tags[:diff]

    result += "\n#my_bibtex"

    return (entry['ID'], result)

if __name__ == "__main__":
    # logging.info("Running Auto Bibtex Tweet")
    with open(expander(tweeted_log)) as f:
        tweeted = [x.strip() for x in f.readlines()]

    bib        = select_bibtex()
    db         = parse_bibtex(bib)
    entry      = select_entry(db, tweeted, bib)

    if entry is None:
        maybe_blacklist_file(db, bib, tweeted)

    id_str, tweet_text = format_tweet(entry)

    # If the tweet is too long, log it as as single line
    if len(tweet_text) >= 280:
        logging.warning(f"Resulting Tweet too long: {len(tweet_text)}\n{tweet_text}")
        single_line = tweet_text.replace("\n", " ")
        with open(too_long_log, 'a') as f:
            f.write(f"({id_str}) : {single_line}\n")

        exit()


    config = configparser.ConfigParser()
    with open(expander(SECRETS_LOC),'r') as f:
        config.read_file(f)

    twit = twitter.Api(consumer_key=config['DEFAULT']['consumerKey'],
                       consumer_secret=config['DEFAULT']['consumerSecret'],
                       access_token_key=config['DEFAULT']['accessToken'],
                       access_token_secret=config['DEFAULT']['accessSecret'],
                       sleep_on_rate_limit=config['DEFAULT']['sleep'],
                       tweet_mode='extended')

    try:
        # logging.info(f"Tweeting: {tweet_text}")
        result = twit.PostUpdate(tweet_text)
        with open(expander(tweeted_log), 'a') as f:
            f.write(f"{id_str}\n")
            # logging.info("Completed")
    except Exception as err:
        logging.warning(f"Failure: {err}")
