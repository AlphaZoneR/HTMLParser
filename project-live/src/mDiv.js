import React, {Component} from 'react'
import Mimg from './mImg'
import Mpara from './mPara'
import Mtext from './mText'

class Mdiv extends Component {
    constructor(props) {
        super(props)
        
        this.state = {
            component: props.component
        };
        
    }

    render() {
        try {
        let children = this.state.component.children.map((o) => {
            if (o.angle != null && o.angle >= 15.0 && o.angle <= 45 + 22.5) {
                return <Mimg key={o.id} component={o}/>
            } else if (o.angle != null && o.angle <= 15.0 && o.angle >= -15.0) {
                return <Mpara key={o.id} component={o}/>
            } else if (o.angle != null && o.angle <= -15.0 && o.angle >= -45.0 - 22.5) {
                return <Mtext key={o.id} component={o}/>
            } else {
                return <Mdiv key={o.id} component={o}/>
            }
        })
            return (
                <div data-angle={this.state.component.angle} key={this.state.component.id} style={{
                    position: 'absolute',
                    top: this.state.component.y + 'px',
                    left: this.state.component.x + 'px',
                    height: this.state.component.height + 'px',
                    width: this.state.component.width + 'px',
                    backgroundColor : '#f1f1f1'
                }}>
                    {children}
                    {this.state.component.angle}
                </div>
            )
        } catch (e) {
            return <div></div>
        }
    }
}

export default Mdiv;