import * as React from 'react';

import './ProgramEditor.css';

export interface IProps {
  program: string;
  onChange: (event: React.FormEvent<HTMLTextAreaElement>) => void;
}

class ProgramEditor extends React.Component<IProps> {
  public render() {
    const { program, onChange } = this.props;
    return (
      <textarea
        rows={6}
        cols={60}
        className="programEditor"
        value={program}
        onChange={onChange}
      />
    );
  }
}

export default ProgramEditor;
