import React, { Component } from 'react';

import './App.css';

import ParentDiv from './parentDiv.js'
import ChildDiv from './childDiv.js'
import MComponent from './Component'

class App extends Component {
  constructor(props) {
    super(props)
    this.state = {
      components: [],
      width: 0,
      height: 0,
      ratiox: 0,
      ratioy: 0
    }
  }

  componentDidMount() {
    this.updateWindowDimensions();
    window.addEventListener('resize', this.updateWindowDimensions);
    setInterval(() => {
      fetch("http://localhost:8080")
        .then((response) => response.json())
        .then((json) => {
          let divs = []
          
          console.log(json)

          json = json.sort((a, b) => a.y - b.y)
          
          console.log(json)

          for (let dom of json) {
            if (!dom.angle) {
              divs.push(dom)
            }
          }

          let biggest = Math.max(...divs.map((object) => {
            return object.height * object.width;
          }))

          let bigges = divs.filter((object) => {
            return object.width * object.height === biggest
          })

          let parent = bigges[0];

          let children = divs.filter((object) => {
            return object.parent === parent.id;
          })
          
          this.setState({
            ratiox: 960.0 / parent.width,
            ratioy: this.state.height / parent.height
          })

          let xoff = (this.state.width - 960) / 2;
          
          let components = json.map((o) => {
            return new MComponent(o.x, o.y, o.width, o.height, o.angle, o.id, o.parent);
          })

  

          for (let component1 of components) {
            for (let component2 of components) {
              if (component1.width / parent.width > 0.80) {
                component1.set_property('width', '90%')
                component1.set_property('background', 'green')
                component1.set_property('margin', 'auto')
                component1.set_property('height', component1.height * this.state.ratioy)
                component1.set_property('display', 'block')
                component1.set_property('position', 'relative')
                continue;
              }
              if (component1.id != component2.id && !component1.deleted) {
                if (component1.is_halligned(component2) && !component2.deleted) {
                  let avg = (component1.x + component2.x) / 2;
                  component2.x = avg;
                  component1.x = avg;
                }

                if (component1.is_valligned(component2)) {
                  let avg = (component1.y + component2.y) / 2;
                  component2.y = avg;
                  component1.y = avg;
                }
              }
            }
          }

          let jsx_children = []

          for (let component1 of components) {
            let oneline = [component1]
            for (let component2 of components) {
              if (component1.id !== component2.id && component1.parent === component2.parent && !component1.deleted && !component2.deleted) {
                if (component1.is_oneline(component2)) {
                  oneline.push(component2)
                }
              }
            }
            let result = this.divideComponents(parent.width, oneline);

            if (result) {
              jsx_children.push(result)
            }
            
          }
          
          
          for (let child of components) {
            if (child.id === parent.id || child.deleted) {
              continue
            }
            jsx_children.push(<ChildDiv key={child.id} x={child.x * this.state.ratiox + xoff} y={child.y * this.state.ratioy} height={child.height * this.state.ratioy} width={child.width * this.state.ratiox} css={child.cssproperties} />);
          }

          let parentDiv = <ParentDiv key={parent.id} portrait="true" children={jsx_children} />

          if (parent.width > parent.height) {
            parentDiv = <ParentDiv key={parent.id} portrait="false" />
          }
          
          this.setState({
            components: [parentDiv]
          })

        })
        .catch((error) => console.log(error))
    }, 1000)
  }

  render() {
    return this.state.components;
  }

  componentWillUnmount() {
    window.removeEventListener('resize', this.updateWindowDimensions);
  }

  updateWindowDimensions() {
    let w = window,
    d = document,
    documentElement = d.documentElement,
    body = d.getElementsByTagName('body')[0],
    width = w.innerWidth || documentElement.clientWidth || body.clientWidth,
    height = w.innerHeight|| documentElement.clientHeight|| body.clientHeight;
    this.setState(
      {
        width: width,
        height: height
      }
    )
  }
  
  
  divideComponents(width, components) {
    
    if (components.length === 1) {
      return null;
    }

    let avg = (width - (width - components.map((o) => o.width).reduce((a, b) => a + b))) / components.length;
    

    
    for (let component of components) {
      if (Math.abs(component.width - avg) > 30) {
        return null;
      }
    }
    
    let cssproperties = {
      'width': '90%',
      'height': components[0].height * this.state.ratioy,
      'display': 'list',
      'margin': 'auto',
      'positon': 'absolute',
      
    }



    for (let component of components) {
      component.set_property('display', 'block')
      component.set_property('width', '48.003%')
      component.set_property('float', 'left')
      component.set_property('position', 'relative')
      component.set_property('height', component.height * this.state.ratioy)
      component.set_property('background', 'green');
      component.set_property('marginRight', '2%')
      component.set_property('top', components[0].y * this.state.ratioy)
      component.set_deleted(true)
    }

    let jsxchildren = components.map((o) => <ChildDiv key={o.id} css={o.cssproperties}/>);

    console.log(jsxchildren)


    return <ChildDiv key={this.guid()} css={cssproperties} children={jsxchildren} /> ;
  }

  guid() {
    function s4() {
      return Math.floor((1 + Math.random()) * 0x10000)
        .toString(16)
        .substring(1);
    }
    return s4() + s4() + '-' + s4() + '-' + s4() + '-' + s4() + '-' + s4() + s4() + s4();
  }
}

export default App;
