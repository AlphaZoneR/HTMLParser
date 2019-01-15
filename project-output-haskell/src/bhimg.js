import React, { Component } from 'react';
import Utils from './Utils';

class Bhimg extends Component {
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
            position: 'absolute',
            top: String(parseFloat(this.state.y) * Utils.getRatioy()) + 'px',
            left: String(parseFloat(this.state.x) * Utils.getRatiox())+'px',
            width: String(parseFloat(this.state.width) * Utils.getRatiox())+'px',
            height: String(parseFloat(this.state.height) * Utils.getRatioy())+'px',
            margin: 0
        }
        return (
            <img alt='lorem' src={'https://picsum.photos/' + parseInt(this.state.width) + '/' + parseInt(this.state.height) + '/?random&t=' + Date.now()} style={style}></img>
        )
    }

    changeValue(key, val) {
        this.setState({key, val})
    }
}

export default Bhimg