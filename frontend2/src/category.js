import Inferno from 'inferno';
import {compose, getJSON, modifyState} from './common';
import spinner from './spinner';


const loadCategory = name => (state, dispatch) => {

  if (undefined === state.categories) {
    state.categories = [];
  }

  var cIndex = state.categories.length;

  state.categories.push({
    name: name
  , loaded: false
  });

  var cb = compose(dispatch, modifyState);

  getJSON(`/api/categories/${name}`, info => cb(state => {
    state.categories[cIndex] = Object.assign({}, info, {
      loaded: true
    , dictsLoaded: false
    });

    if (undefined === state.category) {
      dispatch(setCurrentCategory(info.name));
    }
  }));
}


const setCurrentCategory = name => (state, dispatch) => {

  var cIndex = state.categories.findIndex(category => category.name == name);
  if (-1 == cIndex) {
    return;
  }

  state.category = name;
  if (state.categories[cIndex].dictsLoaded) {
    return;
  }  

  if (undefined === state.dictionaries) {
    state.dictionaries = [];
  }
 
  var cb = compose(dispatch, modifyState);

  getJSON(`/api/categories/${name}/dictionaries`, data => data.forEach(dname => {
    getJSON(`/api/categories/${name}/dictionaries/${dname}`, info => {
      cb(state => {
        state.dictionaries.push(Object.assign({}, info, {
          name: dname
        , enabled: false
        , category: name
        }));

        state.categories[cIndex].loaded = true;
      });
    })
  }));
}


const category = (state, dispatch) => c => (!c.loaded) ? spinner("category"): (
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

export {category, setCurrentCategory, loadCategory};

