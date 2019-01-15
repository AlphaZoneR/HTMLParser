import React, { Component } from 'react';

class Bhbtn extends Component {
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
            top: this.state.x,
            left: this.state.y,
            width: this.state.height,
            height: this.state.width,
            margin: 0
        }
        return (
            <button style={style}></button>
        )
    }

    changeValue(key, val) {
        this.setState({key, val})
    }
}

export default Bhbtn;