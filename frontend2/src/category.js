import Inferno from 'inferno';
import {compose} from './common';

const setCurrentCategory = name => (state, dispatch) => {
  state.category = name;
  
  var cb = compose(dispatch, f => (state, _) => f(state));

  if (undefined === state.dictionaries) {
    // cb(state => {
    //   state.dictionaries = [];
    //   state.translations = [];
    // });
  }
}

const category = (state, dispatch) => c => (
  <label className = "category">
    <input type = "checkbox"
           name = "category"
           value = {c.name}
           checked = {state.category == c.name}
           onInput = {compose(dispatch, setCurrentCategory, e => e.target.value)}
    />
    <div className = "widget"/>
    <div className = "contents">
      <p>{c.name}</p>
      <p>{c.description}</p>
    </div>
  </label>
);

export {category, setCurrentCategory};

