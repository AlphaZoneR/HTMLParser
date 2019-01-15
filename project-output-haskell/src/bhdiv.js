import React, { Component } from 'react';
import Utils from './Utils'
import Bhbtn from './bhbtn'
import Bhpar from './bhpar'
import Bhtxt from './bhtxt'
import Bhimg from './bhimg'

class Bhdiv extends Component {
    constructor(props) {
        super(props)
        this.state = {
            x: props.x,
            y: props.y,
            width: props.width,
            height: props.height,
            children: props.children,
            parent: props.parent
        }

        this.changeValue.bind(this)
    }

    render() {

        console.log(Utils.getRatiox())
        
        if (this.props.parent === '' || this.props.parent === undefined) {
            Utils.setRatiox(960.0 / parseFloat(this.state.width))
            Utils.setRatioy(Utils.getHeight() / parseFloat(this.state.height))
        }


        let style = {
            display: 'inline-block',
            position: 'absolute',
            top: String(parseFloat(this.state.y) * Utils.getRatioy()) + 'px',
            left: String(parseFloat(this.state.x) * Utils.getRatiox())+'px',
            width: String(parseFloat(this.state.width) * Utils.getRatiox())+'px',
            height: String(parseFloat(this.state.height) * Utils.getRatioy())+'px',
            margin: 0,
            backgroundColor: '#1f1f1f'
        }
        let diffx = 0;
        let diffy = 0;

        if (!this.props.parent || this.props.parent === '') {
            style = {
                display: 'inline-block',
                position: 'absolute',
                top: 0,
                left: 0,
                width: String(parseFloat(this.state.width) * Utils.getRatiox())+'px',
                height: String(parseFloat(this.state.height) * Utils.getRatioy())+'px',
                margin: 0,
                backgroundColor: '#f1f1f1',
            }

            diffx = parseFloat(this.state.x)
            diffy = parseFloat(this.state.y)

        }
        
        try {
            let props = React.Children.map(this.props.children, child => {
                return { unique: child.props.unique, x: String(parseFloat(child.props.x) - diffx) + 'px', y: String(parseFloat(child.props.y) - diffy) + 'px', width: child.props.width, parent: child.props.parent, height: child.props.height, name: child.type.name, key: child.props.unique }
            })

            if (props !== undefined) {
                for (let prop1 of props) {
                    for (let prop2 of props) {
                        if (prop1['unique'] !== prop2['unique']) {
                            if (this.is_halligned(prop1, prop2)) {
                                let x1 = parseFloat(prop1['x'])
                                let x2 = parseFloat(prop2['x'])

                                let avg = (x1 + x2) / 2

                                prop1['x'] = String(avg) + 'px'
                                prop2['x'] = String(avg) + 'px'
                            }

                            if (this.is_valligned(prop1, prop2)) {
                                let y1 = parseFloat(prop1['y'])
                                let y2 = parseFloat(prop2['y'])

                                let avg = (y1 + y2) / 2

                                prop1['y'] = String(avg) + 'px'
                                prop2['y'] = String(avg) + 'px'
                            }
                        }
                    }
                }

                let children = props.map(o => {
                    if (o.name === 'Bhdiv') {
                        return <Bhdiv x={o.x} y={o.y} width={o.width} height={o.height} key={o.key} parent={o.parent}></Bhdiv>
                    } else if (o.name == 'Bhbtn') {
                        return <Bhbtn x={o.x} y={o.y} width={o.width} height={o.height} key={o.key} parent={o.parent}></Bhbtn>
                    } else if (o.name == 'Bhimg') {
                        return <Bhimg x={o.x} y={o.y} width={o.width} height={o.height} key={o.key} parent={o.parent}></Bhimg>
                    } else if (o.name == 'Bhtxt') {
                        return <Bhtxt x={o.x} y={o.y} width={o.width} height={o.height} key={o.key} parent={o.parent}></Bhtxt>
                    } else if (o.name == 'Bhpar') {
                        return <Bhpar x={o.x} y={o.y} width={o.width} height={o.height} key={o.key} parent={o.parent}></Bhpar>
                    }
                })

                return (
                    <div style={style}>{children}</div>
                )
            }


        } catch (e) {
            console.log(e)
        }

        return (
            <div style={style}>{this.props.children}</div>
        )
    }

    not_in(child, children) {
        let t = true
        React.Children.map(children, c => {
            if (c !== child && c.props['key'] === child.props['key']) {
                t = false
            } 
        })

        return t
    }

    changeValue(key, val) {
        this.setState({key, val})
    }

    is_halligned(c1, c2) {
        let x1 = parseFloat(c1['x'])
        let x2 = parseFloat(c2['x'])
        return (Math.abs(x1 - x2) < 40.0);
    }

    is_valligned(c1, c2) {
        let y1 = parseFloat(c1['y'])
        let y2 = parseFloat(c2['y'])
        return (Math.abs(y1 - y2) < 50.0);
    }

    is_oneline(c1, c2) {
        let y1 = parseFloat(c1['y'])
        let y2 = parseFloat(c2['y'])

        let height1 = parseFloat(c1['height'])
        let height2 = parseFloat(c2['height'])
        return Math.abs(y1 - y2) < 5 && Math.abs(height1 - height2) < 50 ;
    }
}

export default Bhdiv