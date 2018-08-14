import * as React from 'react';
import './App.css';

import ProgramEditor from './ProgramEditor';
import Text from './Text';

interface IState {
  program: string;
  text: string;
  regex?: any;
}

class App extends React.Component<{}, IState> {
  constructor(props: {}) {
    super(props);
    this.state = { program: "main = 2Times(char)", text: "aa" }
  }

  setProgram = (event: React.FormEvent<HTMLTextAreaElement>) => {
    this.setState({ program: event.currentTarget.value })
  }

  setText = (event: React.FormEvent<HTMLTextAreaElement>) => {
    this.setState({ program: event.currentTarget.value })
  }

  compile = () => {
    this.setState((prevState) => {
      const { program } = prevState;
      const req = new Request(
        "http://localhost:3000/compile",
        { method: "POST", body: `"${program}"`, headers: new Headers({'Content-Type': 'application/json'}) }
      );
      fetch(req, { method: "POST" }).then(response => response.json()).then((body) => {
        this.setState({ regex: new RegExp(body, 'g') });
      });
    });
  }

  public render() {
    const { program, text } = this.state;

    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Irregular</h1>
        </header>
        <ProgramEditor program={program} onChange={this.setProgram}/><br />
        <Text text={text} onChange={this.setText} /><br />
        <button onClick={this.compile}>Compile</button>
      </div>
    );
  }
}

export default App;
