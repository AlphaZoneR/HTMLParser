from cv2 import rectangle, putText, FONT_HERSHEY_SIMPLEX
from uuid import uuid4
import json
class Component:
    x = 0
    y = 0
    width = 0
    height = 0
    area = 0
    angle = None
    lines = 0
    putin = False
    color = (40, 40, 40)
    parent = None
    child = False
    _id = None
    _type = None

    def __init__(self, x, y, width, height, area, parent, lines=0):
        self.x, self.y = x, y
        self.width, self.height = width, height
        self.area = area
        self.lines = lines
        self.parent = parent
        self._id = uuid4()

    @staticmethod
    def distance(lhs, rhs):
        return (lhs.x - rhs.x) ** 2 * 0.01
    
    def draw(self, img):
        rectangle(img, (self.x, self.y), (self.x + self.width, self.y + self.height), (0, 255, 0), thickness=3)
        putText(img, str(self._id).split('-')[0] + ' ' + str(self._type), (self.x, self.y), FONT_HERSHEY_SIMPLEX, 0.3, (0, 0, 0))
        if self.parent != None:
            putText(img, str(self.parent.getid()).split('-')[0], (self.x, self.y + 20), FONT_HERSHEY_SIMPLEX, 0.3, (0, 0, 0))

    def setid(self, id):
        self._id = id
    
    def getid(self):
        return self._id

    def contains(self, rhs):
        return rhs.x >= self.x and rhs.y >= self.y and rhs.x + rhs.width <= self.x + self.width and rhs.y + rhs.height <= self.y + self.height
    
    def equals(self, rhs):
        return self._id == rhs._id
    
    def getarea(self):
        return self.width * self.height

    def print(self):
        print(f'<Component x={self.x}  y={self.y} width={self.width} height={self.height} id={str(self._id)} parent={self.parent}')
    
    def setparent(self, parent):
        self.parent = parent
    
    def jsonify(self):
        return {
            'id': self._id,
            'x': self.x,
            'y': self.y,
            'width': self.width,
            'height': self.height,
            'parent': self.get_parent_id(),
            'angle': self.angle
        }
    
    def get_parent_id(self):
        if not self.parent:
            return None
        
        return self.parent.getid()
    
    def partially_contains(self, rhs):
        return rhs.x >= (self.x and rhs.y >= self.y and rhs.x + rhs.width <= self.x + self.width and rhs.y + rhs.height <= self.y + self.height) or (self.x and rhs.y >= self.y and rhs.x + rhs.width >= self.x + self.width and rhs.y + rhs.height <= self.y + self.height) or (self.x and rhs.y >= self.y and rhs.x + rhs.width <= self.x + self.width and rhs.y + rhs.height >= self.y + self.height) or (self.x and rhs.y >= self.y and rhs.x + rhs.width >= self.x + self.width and rhs.y + rhs.height >= self.y + self.height)

    def set_color(self, color):
        self.color = color
    
    def set_type(self, _type):
        self._type = _type
    
    def get_type(self):
        return self._type