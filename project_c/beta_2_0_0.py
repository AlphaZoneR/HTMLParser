import itertools
import threading
from copy import deepcopy
from math import atan2, degrees, pi, isnan
from time import sleep

import cv2
import flask
from flask_cors import CORS
import numpy as np

def show_angle(out_weight, mag_final, dir_final, min_mag, theta_min, theta_max):
    """
        Return points based on magnitude and angle constraints
    """


    out_img = np.multiply(
        (
            # (mag_final > min_mag) &
            (dir_final >= theta_min) &
            (dir_final <= theta_max)
        ).astype(int),

        out_weight
    ).astype('uint8')

    return out_img

class Component:
    def __init__(self, x, y, w, h, area, parent, contour, id):
        self.x = x
        self.y = y
        self.width = w
        self.height = h
        self.parent = parent
        self.contour = contour
        self.id = id
        self.area = area
        self.type = None
    
    def draw(self, res):        
        cv2.rectangle(res, (self.x, self.y), (self.x + self.width, self.y + self.height), (0, 255, 0), thickness=3)
        cv2.putText(res, str(self.id) + ' ' + str(self.parent) + ' ' + str(self.type), (self.x, self.y), cv2.FONT_HERSHEY_COMPLEX, 0.4, (0, 0, 0))


cap = cv2.VideoCapture('videos/capture-5.avi')

while True:
    try:
        a, src = cap.read()
        if not src.any():
            break
        res = src.copy()
        height, width, _ = src.shape
        # convert the image to grayscale
        gray = cv2.cvtColor(src, cv2.COLOR_BGR2GRAY)
        # binarize the image upon given threshold parameters
        _, binary = cv2.threshold(gray.copy(), 90, 255, cv2.THRESH_BINARY_INV)
        # create a variable in which we store the drawing results
        res = src.copy()


        # find contours for the rectangle detection
        canny = cv2.Canny(gray.copy(), 50, 150, apertureSize=3)
        contours, hierarchy = cv2.findContours(
            binary.copy(), cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

        components = []

        for i in range(len(contours)):
            contour = contours[i]
            hull = cv2.convexHull(contour)

            area = cv2.contourArea(hull)
            perimeter = cv2.arcLength(hull, True)

            try:
                circularity = (4 * pi * area) / (perimeter * perimeter)

                box = cv2.boundingRect(contour)
                if circularity < 0.9 and circularity > 0.5 and area > 1500:
                    components.append(Component(box[0], box[1], box[2], box[3], area, hierarchy[0][i][3], contour, i))
            except Exception as e:
                print(e)


        new_components = []
        for component in components:
            if (component.parent >= 0):
                parent = list(filter(lambda o: o.id == component.parent, components))[0]
                if abs(parent.x - component.x) > 10 and abs(parent.y - component.y) > 10:
                    component.parent = parent.parent
                    grandparent = list(filter(lambda o: o.id == parent.parent, components))[0]
                    grandparent.type = None
                    component.type = 'child'

                    border = {
                        'x': 10,
                        'y': 20
                    }

                    lineimage = canny[component.y + border['x']:component.y+component.height -
                                      border['y'], component.x + border['x']:component.x+component.width - border['y']]

                    # parameters for HoughLinesP
                    rho = 1.5
                    theta = pi / 180.0
                    threshold = 50
                    min_line_length = 15
                    max_line_gap = 4

                    lines = cv2.HoughLines(lineimage, rho, theta, threshold)

                    try:
                        _sum = []
                        # calculate the average angle of the component
                        for line in lines:
                            rho, theta = line[0]

                            ang = 180 * theta / pi

                            if not isnan(ang):
                                _sum.append(ang)
                    except Exception:
                        pass

                    component.type = np.mean(np.asarray(_sum))
                    new_components.append(component)
            else:
                new_components.append(component)

        for component in new_components:
            component.draw(res)

        cv2.imshow('res', res)

        waitkey = cv2.waitKey(1)
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
    except Exception as e:
        print(e)


