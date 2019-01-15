import itertools
import threading
from copy import deepcopy
from math import atan2, degrees, pi
from time import sleep

import cv2
import flask
from flask_cors import CORS
import numpy as np

from component import Component
from utils import generate_hierarchy

c = threading.Condition()
globalA = []

# types:
# None -> div
# --- -> paragraph, header, title, decided upon area
# | -> image
# / -> button
# \ -> not yet decided


class Analyzer(threading.Thread):
    '''
        Class which analyzes a given video feed. It detects rectangles, and if there
        are existing lines in them, their angle, and upon those decides on an angle.
        After 20 frames it updates a global variable used by the webserver
    '''

    def __init__(self):
        threading.Thread.__init__(self)
        self.A = []
        self.iterator = 0
        # Open the video/stream we want to review.
        self.cap = cv2.VideoCapture('videos/DSC_0350.MOV')

    def run(self):
        global globalA
        while True:
            # read a frame from the feed
            a, src = self.cap.read()
        # rotate only necessary if filmed in portrait
            src = cv2.rotate(src, cv2.ROTATE_90_COUNTERCLOCKWISE)
            # dimensions of the image
            height, width, _ = src.shape
            # convert the image to grayscale
            gray = cv2.cvtColor(src, cv2.COLOR_BGR2GRAY)
            # binarize the image upon given threshold parameters
            _, binary = cv2.threshold(gray, 90, 255, cv2.THRESH_BINARY_INV)
            # create a variable in which we store the drawing results
            res = src.copy()
            # find contours for the rectangle detection
            contours, _ = cv2.findContours(
                binary.copy(), cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
            # find edges for line detection
            canny = cv2.Canny(gray, 50, 150, apertureSize=3)

            B = []
            # iterate through found contours
            for contour in contours:
                hull = cv2.convexHull(contour)

                area = cv2.contourArea(hull)
                perimeter = cv2.arcLength(hull, True)

                try:
                    circularity = (4 * pi * area) / (perimeter * perimeter)

                    box = cv2.boundingRect(contour)
                    # only add the item to our found list if it passes certain criteria
                    if area > 1500 and area < width * height:
                        # only add rectangles (mostly), one will notice certain amount of triangles
                        if circularity < 0.9 and circularity > 0.65:
                            # if it's the first iteration, we must fill up the A array
                            # if self.iterator == 0:
                            #     self.A.append(Component(*box, area, None))

                            B.append(Component(*box, area, None))
                except ZeroDivisionError as e:
                    pass

            C = []

            # decide what's new, what's old, what to keep
            while self.A and B:
                distances = [[a, b, Component.distance(
                    a, b)] for a, b in itertools.product(self.A, B)]
                a, b, error = min(distances, key=lambda o: o[2])

                if error > 0.02:
                    break
                b.setid(a.getid())
                b.angle = a.angle

                C.append(b)
                self.A.remove(a)
                B.remove(b)

            D = B + C

            # generate first hierarchy
            generate_hierarchy(D)

            self.A = []

            # only add the outer detected contours (findContours will find the inner and the outer contour aswell)
            for a in D:
                if a.parent != None:
                    if a.parent.getarea() - a.getarea() > 10000:
                        self.A.append(a)
                elif a.parent == None:
                    self.A.append(a)

            # fix hierarchy
            generate_hierarchy(self.A)

            # define the angle of the object
            for obj in self.A:
                # if object is the most outer, or has children, it means it can't have a type, or it's a container
                if obj.parent != None and obj.getarea() < 300 * 300 and not obj.child:
                    # hack to mostly ignore the lines of the rectangle itself
                    border = {
                        'x': 10,
                        'y': 20
                    }

                    lineimage = canny[obj.y + border['x']:obj.y+obj.height -
                                      border['y'], obj.x + border['x']:obj.x+obj.width - border['y']]

                    # parameters for HoughLinesP
                    rho = 1.5
                    theta = pi / 180.0
                    threshold = 15
                    min_line_length = 15
                    max_line_gap = 4

                    lines = cv2.HoughLinesP(
                        lineimage, rho, theta, threshold, min_line_length, max_line_gap)

                    try:
                        _sum = []
                        # calculate the average angle of the component
                        for line in lines:
                            for x1, y1, x2, y2 in line:
                                # cv2.line(res, (x1 + obj.x + border['x'], y1 + obj.y + border['x']), (obj.x + x2 + border['x'], obj.y + y2 + border['x']), (0, 255, 0))
                                _sum.append(degrees(atan2(y2 - y1, x2 - x1)))

                        angle = np.mean(np.asarray(_sum))
                        new_sum = []

                        for a in _sum:
                            if abs(a) >= abs(angle):
                                new_sum.append(a)
                        # check for cases when the outer contours weren't removed

                        angle1 = np.mean(np.asarray(new_sum))

                        # if the body of the if below is executed, it means that the outer contours weren't removed, we
                        # should get a more exact angle
                        # if abs(angle1 - angle) >= 22.5:
                        #     angle = angle1

                        # if the angle is whitin a certain range of error, keep the old angle
                        if obj.angle != None:
                            if abs(obj.angle - angle) >= 22.5:
                                obj.angle = angle
                                obj.set_type(str(angle))
                            else:
                                obj.set_type(str(obj.angle))
                        else:
                            obj.angle = angle
                            obj.set_type(str(angle))

                    except Exception as e:
                        pass

            # draw the components onto the result image
            for obj in globalA:
                obj.draw(res)

            cv2.imshow('', res)
#            cv2.imshow('canny', canny)
#            cv2.imshow('binary', binary)
#            cv2.imshow('gray', gray)
#            blur = cv2.GaussianBlur(gray, (5, 5), 0)
#            cv2.imshow('blur', blur)
            waitkey = cv2.waitKey(1)

            self.iterator += 1

            # every other 20 frames update the global variable
            if self.iterator % 20 == 0:
                globalA = deepcopy(self.A)

            # controls for the GUI
            # space -> pause, resume
            # l -> list current components
            # q -> quit

            if waitkey == ord(' '):
                while True:
                    waitkey = cv2.waitKey(0)
                    if waitkey == ord('l'):
                        print('\n')
                        for a in self.A:
                            a.print()
                    elif waitkey == ord(' '):
                        break
                    elif waitkey == ord('q'):
                        exit(1)
            elif waitkey == ord('q'):
                        exit(1)


class Server(threading.Thread):
    '''
        HTTP server which on it's root returns a json formated tree representation of our indentified
        components
    '''

    def __init__(self):
        threading.Thread.__init__(self)
        self.app = flask.Flask(__name__)
        CORS(self.app)
        # root

        @self.app.route('/', methods=['GET'])
        def root():
            global globalA

            ls = [a.jsonify() for a in globalA]

            return flask.jsonify(ls)

    def run(self):
        self.app.run(host='0.0.0.0', port=8080)


if __name__ == '__main__':
    server = Server()
    analyzer = Analyzer()

    server.start()
    analyzer.start()

    server.join()
    analyzer.join()
