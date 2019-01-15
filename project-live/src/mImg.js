import React, {Component} from 'react'

class Mimg extends Component {
    constructor(props) {
        super(props)
        
        this.state = {
            component: props.component
        };
        
    }

    render() {
        try {
            return (
                <img alt="lorem ipsum" src={"https://picsum.photos/" + this.state.component.width + "/" + this.state.component.height +"/?random"} key={this.state.id} style={{
                    position: 'absolute',
                    top: this.state.component.y + 'px',
                    left: this.state.component.x + 'px',
                    height: this.state.component.height + 'px',
                    width: this.state.component.width + 'px',
                    border: '1px solid black'
                }}></img>
            )
        } catch (e) {
            console.log(e)
            return <div></div>
        }
    }
}

export default Mimg;