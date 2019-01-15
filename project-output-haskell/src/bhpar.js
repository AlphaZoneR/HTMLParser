import React, { Component } from 'react';
import Utils from './Utils'

class Bhpar extends Component {
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
            overflow: 'hidden',
            textAlign: 'justify',
            margin: 0,
            border: '1px solid black',
            padding: '2px 2px 2px 2px',
            fontSize: '14pt',
            backgroundColor: '#d1d1d1'
        }
        return (
            <p style={style}>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed egestas lobortis iaculis. In vitae velit non ex venenatis dignissim. Interdum et malesuada fames ac ante ipsum primis in faucibus. Vestibulum a fermentum sapien. Suspendisse ut venenatis sem, sit amet dignissim felis. Ut non pretium orci. Praesent dapibus quam et leo sodales consequat sit amet vel risus. Curabitur a quam a augue eleifend vehicula. Nunc quis libero tristique</p>
        )
    }

    changeValue(key, val) {
        this.setState({key, val})
    }
}

export default Bhpar