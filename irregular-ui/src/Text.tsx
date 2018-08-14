import * as React from 'react';

export interface IProps {
  text: string;
  onChange: (event: React.FormEvent<HTMLTextAreaElement>) => void;
}

class Text extends React.Component<IProps> {
  public render() {
    const { text, onChange } = this.props;
    return (
      <textarea
        rows={6}
        cols={60}
        className="text"
        value={text}
        onChange={onChange}
      />
    );
  }
}

export default Text;
