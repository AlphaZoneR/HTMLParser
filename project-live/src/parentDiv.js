import React, { Component } from 'react';

import './parentDiv.css';

class ParentDiv extends Component {
    constructor(props) {
        super(props);
        this.state = {
            portrait: props.portrait,
            children: props.children
        }
    }

    render() {
        if (this.state.portrait === 'true') {
            return (
                <div className="div-portait">
                  {this.state.children}
                </div>
            )
        }

        return (
            <div className="div-landscape"></div>
        )
    }
}

export default ParentDiv;