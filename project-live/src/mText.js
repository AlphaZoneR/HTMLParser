import React, {Component} from 'react'

class Mtext extends Component {
    constructor(props) {
        super(props)
        
        this.state = {
            component: props.component
        };
        
    }

    render() {
        try {
            return (
                <textarea key={this.state.id} style={{
                    position: 'absolute',
                    top: this.state.component.y + 'px',
                    left: this.state.component.x + 'px',
                    height: this.state.component.height + 'px',
                    maxHeight: this.state.component.height + 'px',
                    width: this.state.component.width + 'px',
                    overflow: 'hidden',
                    padding: '5px 5px 5px 5px'
                }} defaultValue="Nullam magna ante, fringilla in lacus in, gravida viverra sapien. Morbi volutpat urna velit, ac euismod nulla ultrices a. Quisque fermentum urna ac lectus convallis, eu ullamcorper tortor mollis."></textarea>)
        } catch (e) {
            console.log(e)
            return <div></div>
        }
    }
}

export default Mtext;