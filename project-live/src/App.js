import React, { Component } from 'react';

import './App.css';

import MComponent from './Component'
import Mdiv from './mDiv'
import Mimg from './mImg'

class App extends Component {
  constructor(props) {
    super(props)
    this.state = {
      components: [],
      width: 0,
      height: 0,
      ratiox: 0,
      ratioy: 0,
      pause: false
    }

    this.pausefunc = () => {
      this.setState({
        pause: !this.state.pause
      })
    }

    this.pausefunc.bind(this)
  }

  componentDidMount() {
    this.updateWindowDimensions();
    window.addEventListener('resize', this.updateWindowDimensions);
    let headers = new Headers();
    headers.set('Cache-Control', 'no-cache');
    headers.set('Pragma', 'no-cache');
    headers.set('Expires', '0');
    setInterval(() => {
      if (this.state.pause) {
        return;
      }
      fetch("http://localhost:8080/?t=" + Date.now(), {
        method: 'GET',
        headers: headers
      })
        .then((response) => response.json())
        .then((json) => {
          this.setState({
            components: []
          })
          let sorted = json.sort((a, b) => -a.y + b.y)
          
          let components = sorted.map((o) => new MComponent(o.x, o.y, o.width, o.height, o.angle, o.id, o.parent))

          for (let component1 of components) {
            for (let component2 of components) {
              if (component1.id !== component2.id) {
                if (component1.is_halligned(component2)) {
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

          let samelines = []

          for (let component1 of components) {
            let sameline = [component1]
            for (let component2 of components) {
              if (component1.id !== component2.id && component1.parent === component2.parent) {
                if (component1.is_oneline(component2) ) {
                  sameline.push(component2)
                }
              }
            }

            if (sameline.length !== 1) {
              samelines.push(sameline);
            }
          }

          samelines = samelines.map((o) => {
            return o.sort((a, b) => a.x - b.x)
          })

          let names = new Set(samelines.map((o) => {
            let name = o.map((o1) => o1.id.split('-')[0]).join('');
            return name
          }))

          let namesobjects = new Set(samelines.map((o) => {
            let name = o.map((o1) => o1.id.split('-')[0]).join('');
            return [name, o]
          }))

          let final_samelines = []

          for (let name of names) {
            for (let foo of namesobjects) {
              if (foo[0] === name) {
                final_samelines.push(foo[1]);
                break;
              }
            }
          }

          for (let component of components) {
            if (component.parent) {
              let parent = components.filter((o) => o.id === component.parent)[0]
              if (component.width / parent.width > 0.80) {
                component.width = parent.width * 0.90;

                component.x = (parent.width - component.width) / 2;
              }
            } else {
              component.x = 0;
              component.y = 0;
            }
          }

          for (let list of final_samelines) {
            let parent = components.filter((obj) => obj.id === list[0].parent)[0]

            if (parent === null || parent === undefined) {
              break;
            }

            let realestate = parent.width * 0.90 - 5 * list.length

            let average = realestate / list.length
            let average_height = list.map((o) => o.height).reduce((a, b) => a+b, 0) / list.length;
            let available = list.filter((o) => Math.abs(o.width - average) < 50)

            let xoff = (parent.width - realestate) / 2 - 5

            if (available.length === list.length) {
              for (let component of list) {
                component.width = average;
                component.height = average_height;

                component.x = xoff;
                xoff += average + 10;
              }
            }
          }
          
          let solos = []

          for (let component1 of components) {
            let solo = true;
            for (let component2 of components) {
              if (component1 !== component2) {
                if (component1.is_oneline(component2) || !component1.parent) {
                  solo = false;
                }
              }
            }

            if (solo) {
              solos.push(component1);
            }
          }

          for(let component of solos) {
            let parent = components.filter((obj) => obj.id === component.parent)[0] 
            
            if (parent === null || parent === undefined) {
              break;
            }
            
            let realestate = parent.width * 0.90
            
            if (component.width / realestate < 0.95) {
              component.x = (parent.width - realestate) / 2;
              component.width = realestate / 2 - 5;
            }
          }  

          

          let sorted_horizontal = components.sort((a, b) => -a.y + b.y)
          let diff = 0;
          try {
            let most_top = sorted_horizontal.filter((obj) => !obj.has_above(sorted_horizontal) && obj.parent)[0]
            if (most_top.y > 10) {
              diff = most_top.y - 10;
            }
          } catch (e) {}

          sorted_horizontal = sorted_horizontal.map((o) => {
            if (o.parent) {
              o.y -= diff;
            }

            return o;
          })
          
          
          let grouped = this.groupBy(sorted_horizontal, 'y')

          for (let y in grouped) {
            grouped[y] = {
              group: grouped[y],
              used: false
            }
          }
          
          let biggest = components.filter((o) => o.width * o.height === Math.max(...components.map((obj) => obj.width * obj.height)))[0]

          this.setState({
            ratiox: 960.0 / biggest.width,
            ratioy: this.state.height / biggest.height
          })

          
          components = components.map((o) => {
            o.x *= this.state.ratiox;
            o.y *= this.state.ratioy;
            o.width *= this.state.ratiox
            o.height *= this.state.ratioy

            return o;
          })



          for (let component1 of components) {
            if (!component1.deleted) {
              for (let component2 of components) {
                if (component2.parent === component1.id) {
                  component1.children.push({...component2})
                  component2.deleted = true;
                }
              }
            }
          }

          
          let renderable = sorted_horizontal.filter((o) => ! o.deleted).map((o) => {
            if (o.angle >= 15.0 && o.angle <= 45 + 22.5) {
              return <Mimg key={o.id} component={o}/>
            } else {
              return <Mdiv key={o.id} component={o}/>
            }
          })
          
          this.setState({
            components: [...renderable, <button style={{position: 'absolute', top: this.state.height - 50}} onClick={this.pausefunc}>Pause</button>]
          })
          
        })
      }, 1500)
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

  groupBy (xs, key) {
    return xs.reduce((rv, x)  => {
      (rv[x[key]] = rv[x[key]] || []).push(x);
      return rv;
    }, {});
  };
  

}

export default App;
