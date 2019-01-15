import React, {Component} from 'react'

class Mpara extends Component {
    constructor(props) {
        super(props)
        
        this.state = {
            component: props.component
        };
        
    }

    render() {
        try {
            return (
                <div alt="lorem ipsum" src={"https://picsum.photos/" + this.state.component.width + "/" + this.state.component.height +"/?random"} key={this.state.id} style={{
                    position: 'absolute',
                    top: this.state.component.y + 'px',
                    left: this.state.component.x + 'px',
                    height: this.state.component.height + 'px',
                    maxHeight: this.state.component.height + 'px',
                    width: this.state.component.width + 'px',
                    overflow: 'hidden',
                    padding: '5px 5px 5px 5px',
                    textAlign: 'justify',
                    border: '1px solid black'
                }}>Donec mollis, ipsum nec mollis vehicula, diam augue dapibus velit, eget tempus nibh nisi at augue. Duis ultricies suscipit ipsum eget auctor. Vestibulum sit amet aliquam nisl, ut gravida lacus. Sed vitae turpis vestibulum, congue ex at, ullamcorper lorem. Praesent eget sollicitudin sem, nec accumsan est. Quisque diam dui, mattis ac laoreet non</div>
            )
        } catch (e) {
            console.log(e)
            return <div></div>
        }
    }
}

export default Mpara;