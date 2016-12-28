import Inferno from 'inferno';
import Component from 'inferno-component';

import {compose, getJSON} from './common';
import {category, setCurrentCategory} from './category';
import {dictionary} from './dictionary';

// import Translation from './translation';

const updateQuery = e => (state, dispatch) => {
  state.query = e.target.value;
}

const init = (dispatch) => {

  var cb = compose(dispatch, f => (state, _) => f(state));
 
  cb(state => {
    state.query = "";
  });

  getJSON('/api/categories', data => {

    cb(state => {
      state.categories = [];
    });
   
    data.forEach(name => {
      getJSON(`/api/categories/${name}`, info => {
        cb(state => {
          state.categories.push(Object.assign({}, info, {loaded: false}));

          if (undefined === state.category) {
            dispatch(setCurrentCategory(info.name));
          }
        });
        
      });
    });
  });
}


const view = (state, dispatch) => (
  <div className = "container">
    <form className = "search">
      <input type = "input"
             placeholder = "_"
             onInput = {compose(dispatch, updateQuery)}/>

      <div className = "categories">
        {(!state.categories)
         ? "**LOADING**"
         : state.categories.map(category(state, dispatch))}
      </div>

      <div className = "dictionaries">
        {(!state.dictionaries)
         ? null
         : state.dictionaries
           .filter(d => d.category == state.category)
           .map(dictionary(state, dispatch))}
      </div>
    </form>
  </div>
);


export default {init, view};

