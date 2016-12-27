import Inferno from 'inferno';

import app from './app';

const render = node => Inferno.render(node, document.getElementById('app'));

const start = (init, view) => {
  var state = {};

  const dispatch = action => {
    action(state, dispatch);
    render(view(state, dispatch));
  }

  init(dispatch);
}

start(app.init, app.view);

