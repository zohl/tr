import Inferno from 'inferno';
import {compose, getJSON} from './common';

const setCurrentCategory = name => (state, dispatch) => {

  var cIndex = state.categories.findIndex(category => category.name == name);
  if (-1 == cIndex) {
    return;
  }

  state.category = name;
  if (state.categories[cIndex].loaded) {
    return;
  }  

  var cb = compose(dispatch, f => (state, _) => f(state));

  if (undefined === state.dictionaries) {
    cb(state => {
      state.dictionaries = [];
    });
  }
 
  getJSON(`/api/categories/${name}/dictionaries`, data => data.forEach(dname => 
    getJSON(`/api/categories/${name}/dictionaries/${dname}`, info => {
      cb(state => {
        state.dictionaries.push(Object.assign({}, info, {
          name: dname
        , enabled: false
        , category: name
        }));

        state.categories[cIndex].loaded = true;
      });
    })));
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

