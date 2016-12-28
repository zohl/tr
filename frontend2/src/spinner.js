import Inferno from 'inferno';

const spinner = className => (
  <div class = {"loading" + (className ? ' '+className : '')}>
    <div class = "container">
      <span class = "spinner"/>
    </div>
  </div>
);

export default spinner;

