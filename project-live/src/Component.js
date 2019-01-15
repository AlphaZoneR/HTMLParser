class MComponent {
    constructor(x, y, width, height, angle, id, parent) {
        this.x = x;
        this.y = y;
        this.angle = angle;
        this.id = id;
        this.height = height;
        this.width = width;
        this.parent = parent
        this.cssproperties = {};
        this.deleted = false;
        this.children = [];
    }
    /**
     * @param component Component
     */

    is_above(component) {
        return !this.is_below(component)
    }

    has_above(components) {
        for (let component of components) {
            if (component.id !== this.id) {
                if (component.is_above(this) && component.id !== this.parent) {
                    return true;
                }
            }
        }

        return false;
    }

    closest_above(components) {
        if (!this.has_above(components)) {
            return null;
        }
        let min = 99999999;
        let current = null;
        for (let component of components) {
            if (component.id !== this.id && component.is_above(this)) {
                if (Math.abs(component.y - this.y) < min) {
                    min = Math.abs(component.y - this.y) < min;
                    current = component;
                }
            }
        }

        return current;
    }

    is_below(component) {
        return this.y > component.y && this.parent === component.parent;
    }

    is_halligned(component) {
        return (Math.abs(this.x - component.x) < 40.0);
    }

    is_valligned(component) {
        return (Math.abs(this.y - component.y) < 50.0);
    }

    is_oneline(component) {
        return Math.abs(this.y - component.y) < 5 && Math.abs(this.height - component.height) < 50 ;
    }

    setWidth(width) {
        this.width = width;
    }

    set_property(key, value) {
        this.cssproperties[key] = value;
    }

    set_deleted(val) {
        this.deleted = val;
    }

    setY(y) {
        this.y = y;
    }
}

export default MComponent