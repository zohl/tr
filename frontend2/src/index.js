import Inferno from 'inferno';

import app from './app';

const render = node => Inferno.render(node, document.getElementById('app'));

const start = (init, view, update) => {
  var state = {};

  const dispatch = action => {
    update(action, state);
    render(view(dispatch, state));
  }

  init(dispatch);
}

start(app.init, app.view, app.update);

