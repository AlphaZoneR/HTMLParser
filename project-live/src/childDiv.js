import React, { Component } from 'react';


class ChildDiv extends Component {
    constructor(props) {
        super(props);
        this.state = {
            x: props.x,
            y: props.y,
            width: props.width,
            height: props.height,
            css: props.css,
            children: props.children
        }
    }

    render() {
        if (!('width' in this.props.css)) {
            return (
                <div style={{
                    position: 'absolute',
                    top: this.state.y,
                    left: this.state.x,
                    width: this.state.width,
                    height: this.state.height,
                    backgroundColor: 'green'
                }} >
                    {this.state.children}
                </div>
            )
        } else {
            return <div style={this.props.css}>
            {this.state.children}</div>
        }
    }
}

export default ChildDiv;