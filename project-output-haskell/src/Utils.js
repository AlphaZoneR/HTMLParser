class Utils {
    constructor() {
        this.width = 0
        this.height = 0
        this.ratiox = 1
        this.ratioy = 1
    }

    static setHeight(h) {
        this.height = h
    }

    static setWidth(w) {
        this.width = w
    }

    static getHeight() {
        return this.height
    }

    static getWidth() {
        return this.width
    }

    static setRatiox(r) {
        this.ratiox = r
    }

    static setRatioy(r) {
        this.ratioy = r
    }

    static getRatiox() {
        return this.ratiox
    }

    static getRatioy() {
        return this.ratioy
    }
}

export default Utils