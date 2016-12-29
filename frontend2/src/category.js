import Inferno from 'inferno';
import {compose, getJSON, modifyState} from './common';
import spinner from './spinner';
import {loadDictionaries} from './dictionary';


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
    Object.assign(state.categories[cIndex], info, {
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

  state.categories[cIndex].dictsLoaded = true;
  dispatch(loadDictionaries(name));
}


const category = (state, dispatch) => c => (!c.loaded) ? spinner("category"): (
  <label class = "category">
    <input type = "checkbox"
           name = "category"
           value = {c.name}
           checked = {state.category == c.name}
           onInput = {compose(dispatch, setCurrentCategory, e => e.target.value)}
    />
    <div class = "widget"/>
    <div class = "contents">
      <p>{c.name}</p>
      <p>{c.description}</p>
    </div>
  </label>
);

export {category, setCurrentCategory, loadCategory};

