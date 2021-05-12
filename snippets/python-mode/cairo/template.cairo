# -*- mode: snippet -*-
# name: template.cairo
# uuid: template.cairo
# group: cairo
# --
import sys
import time
import math
import cairo
import logging
import numpy as np
import cairo_utils as utils
from cairo_utils.dcel.constants import VertE, EdgeE, FaceE
import argparse
from noise import pnoise2, snoise2
from os.path import splitext, split
import IPython

#Constants:
N = 11
SIZE = pow(2,N)
SCALER = 1 / SIZE
TIME = 100
imgPath = "./imgs/"
imgName = "initialTest"
dcel_filename = "theDCEL.pickle"
currentTime = time.gmtime()
FONT_SIZE = 0.03
SCALE = False

def tick(dc, i):
    """ Where the main drawing routine goes """

    return dc

if __name__ == "__main__":
    parser = argparse.ArgumentParser("")
    parser.add_argument('-l', "--loaddcel", action="store_true")
    parser.add_argument('-s', '--static', action="store_true")
    parser.add_argument('-d', '--dontdraw',action="store_true")
    parser.add_argument('--drawsteps', action="store_true")
    parser.add_argument('-n', '--numpoints',type=int, default=N)
    parser.add_argument('-t', '--timesteps', type=int, default=TIME)
    parser.add_argument('--ipython', action="store_true")
    args = parser.parse_args()

    #format the name of the image to be saved thusly:
    saveString = "{}{}_{}-{}_{}-{}".format(imgPath,
                                           imgName,
                                           currentTime.tm_min,
                                           currentTime.tm_hour,
                                           currentTime.tm_mday,
                                           currentTime.tm_mon,
                                           currentTime.tm_year)

    #setup logging:
    LOGLEVEL = logging.DEBUG
    LOG_FILE_NAME = "log.{}".format(splitext(split(__file__)[1])[0])
    logging.basicConfig(filename=LOG_FILE_NAME,level=LOGLEVEL,filemode='w')

    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    logging.getLogger('').addHandler(console)

    if args.loaddcel:
        theDCEL = utils.dcel.DCEL.loadfile(dcel_filename)
    else:
        theDCEL = utils.dcel.DCEL(bbox=[0, 0, 2000, 2000])

    if args.drawsteps or not args.dontdraw:
        logging.info("Setting up Cairo, size: {}".format(N))
        surface, ctx, size, N = utils.drawing.setup_cairo(N=args.numpoints, scale=SCALE, background=[0,0,0,1])
        ctx.set_source_rgba(0, 0, 0, 1)
        utils.drawing.drawRect(ctx, 0, 0, size, size)

    if not args.static:
        logging.info("Generating")
        for x in range(args.timesteps):
            logging.info("Step: {}".format(x))
            theDCEL = tick(theDCEL, x)

            if args.drawsteps:
                logging.info("Drawing Step: {}".format(x))
                if args.ipython:
                    IPython.embed(simple_prompt=True)
                utils.dcel.drawing.drawDCEL(ctx, theDCEL, faces=True, edges=True, verts=True)
                utils.drawing.write_to_png(surface, saveString, i=x)

    #save the dcel
    if not args.loaddcel:
        logging.info("Saving DCEL")
        theDCEL.savefile(dcel_filename)

    #draw it
    if not args.dontdraw:
        final_name = "{}_FINAL".format(saveString)
        logging.info("Drawing to: {}".format(final_name))
        utils.dcel.drawing.drawDCEL(ctx, theDCEL, faces=True, edges=True, background_colour=[0,1,0,1])
        utils.drawing.write_to_png(surface, final_name)


    if args.ipython:
        IPython.embed(simple_prompt=True)
