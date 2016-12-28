import Inferno from 'inferno';

import app from './app';

const start = (init, view) => {
  var state = {};
  var root = document.getElementById('app');

  const render = () => Inferno.render(view(state, dispatch), root);

  const dispatch = action => {
    action(state, dispatch);
    render();
  }

  init(state, dispatch);
  render();
}

start(app.init, app.view);

