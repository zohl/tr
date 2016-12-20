// inferno module
import Inferno from 'inferno';

// routing modules
import { Router, Route } from 'inferno-router';
import createBrowserHistory from 'history/createBrowserHistory';

// app components
import app from './app';

// import VersionComponent from './VersionComponent';

const browserHistory = createBrowserHistory();
// component={ VersionComponent } 

const routes = (
  <Router history={browserHistory}>
    <Route component={app}>
    </Route>
  </Router>
);

      // <Route path="/" />
Inferno.render(routes, document.getElementById('app'));

