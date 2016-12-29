import Inferno from 'inferno';
import Component from 'inferno-component';

import {compose, getJSON, modifyState} from './common';
import {category, loadCategory, setCurrentCategory} from './category';
import {dictionary} from './dictionary';
import spinner from './spinner';


// import Translation from './translation';

const updateQuery = e => (state, dispatch) => {
  state.query = e.target.value;
}

const init = (state, dispatch) => {
  state.query = "";
  getJSON('/api/categories', data => data.forEach(compose(dispatch, loadCategory)));
}

const view = (state, dispatch) => (
  <div className = "container">
    <form className = "search">
      <input type = "input"
             placeholder = "_"
             onInput = {compose(dispatch, updateQuery)}/>

      {(undefined === state.categories) ? spinner('categories'): (
        <div class = "categories">
          {state.categories.map(category(state, dispatch))}
        </div>
      )}

      {(undefined == state.dictionaries)
       ? '' 
       : (!state.dictionaries.filter(d => d.category == state.category).length)
         ? spinner('dictionaries')
         : (
           <div class = "dictionaries">
            {state.dictionaries
             .filter(d => d.category == state.category)
             .map(dictionary(state, dispatch))}
           </div>
      )}

    </form>
  </div>
);


export default {init, view};

