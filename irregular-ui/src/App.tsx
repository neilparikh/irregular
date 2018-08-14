import * as React from 'react';
import './App.css';

import ProgramEditor from './ProgramEditor';
import Text from './Text';

interface IState {
  program: string;
  text: string;
  highlightedString?: any;
}

class App extends React.Component<{}, IState> {
  constructor(props: {}) {
    super(props);
    this.state = { program: "main = 2Times(digit)", text: "I have 10 apples." }
  }

  setProgram = (event: React.FormEvent<HTMLTextAreaElement>) => {
    this.setState({ program: event.currentTarget.value })
  }

  setText = (event: React.FormEvent<HTMLTextAreaElement>) => {
    this.setState({ text: event.currentTarget.value })
  }

  compile = () => {
    this.setState((prevState) => {
      const { program, text } = prevState;
      const req = new Request(
        "http://localhost:3000/match",
        {
          method: "POST",
          body: JSON.stringify({ program, text}),
          headers: new Headers({'Content-Type': 'application/json'}),
        }
      );
      fetch(req, { method: "POST" }).then(response => response.json()).then((body) => {
        this.setState({ highlightedString: body });
      });
    });
  }

  public render() {
    const { program, text, highlightedString } = this.state;

    return (
      <div className="App">
        <header className="App-header">
          <h1 className="App-title">Irregular</h1>
        </header>
        <ProgramEditor program={program} onChange={this.setProgram}/><br />
        <Text text={text} onChange={this.setText} /><br />
        <button onClick={this.compile}>Find Matches</button>
        {highlightedString &&
          <div id="highlightedString" dangerouslySetInnerHTML={{ __html: highlightedString }}/>}
      </div>
    );
  }
}

export default App;
