import Inferno from 'inferno';
import Component from 'inferno-component';

import {compose, update, getJSON} from './common';
import category from './category';

// import Dictionary from './dictionary';
// import Translation from './translation';

const actions = {
  modifyState: f => state => f(state)
, updateQuery: e => state => {state.query = e.target.value;}
};


const init = (dispatch) => {

  var cb = compose(dispatch, actions.modifyState);
 
  cb(state => {
    state.query = "";
    state.categories = undefined;
  });

  getJSON('/api/categories', data => {

    cb(state => {
      state.categories = [];
    });
   
    data.forEach(name => {
      getJSON(`/api/categories/${name}`, info => {
        cb(state => {
          state.categories.push({
            name: info.name
          , description: info.description 
          });
        });
      });
    });

  });
};


const view = (dispatch, state) => (
  <div className = "container">
    <form className = "search">
      <input type = "input"
             placeholder = "_"
             onInput = {compose(dispatch, actions.updateQuery)}/>

      <div className = "categories">
        {(!state.categories) ? "**LOADING**" : (state.categories.map(category))}
      </div>
    </form>
  </div>
);


export default {init, view, update};
