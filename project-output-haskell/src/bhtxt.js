import React, { Component } from 'react';
import Utils from './Utils'
class Bhtxt extends Component {
    constructor(props) {
        super(props)
        this.state = {
            x: props.x,
            y: props.y,
            width: props.width,
            height: props.height
        }

        this.changeValue.bind(this)
    }

    render() {
        let style = {
            display: 'inline-block',
            position: 'absolute',
            top: String(parseFloat(this.state.y) * Utils.getRatioy()) + 'px',
            left: String(parseFloat(this.state.x) * Utils.getRatiox())+'px',
            width: String(parseFloat(this.state.width) * Utils.getRatiox())+'px',
            height: String(parseFloat(this.state.height) * Utils.getRatioy())+'px',
            margin: 0
        }
        return (
            <textarea style={style}></textarea>
        )
    }

    changeValue(key, val) {
        this.setState({key, val})
    }
}

export default Bhtxt