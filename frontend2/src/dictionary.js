import Inferno from 'inferno';
import {compose, modifyState, getJSON} from './common';
import spinner from './spinner';


const loadDictionary = cname => dname => (state, dispatch) => {
  var dIndex = state.dictionaries.length;

  state.dictionaries.push({
    name: dname
  , loaded: false
  , category: cname
  });

  var cb = compose(dispatch, modifyState);

  getJSON(`/api/categories/${cname}/dictionaries/${dname}`, info => cb(state =>
    Object.assign(state.dictionaries[dIndex], info, {
      loaded: true
    , enabled: false
    })
  ));
}


const loadDictionaries = cname => (state, dispatch) => {
  if (undefined === state.dictionaries) {
    state.dictionaries = [];
  }

  getJSON(`/api/categories/${cname}/dictionaries`, data =>
    data.forEach(compose(dispatch, loadDictionary(cname))));
}


const toggleDictionary = name => (state, dispatch) => {
  var dIndex = state.dictionaries.findIndex(dictionary => dictionary.name == name);
  if (-1 == dIndex) {
    return;
  }

  var enabled = !state.dictionaries[dIndex].enabled;
  state.dictionaries[dIndex].enabled = enabled;

  if (!enabled) {
    return;
  }

  if (undefined === state.translations) {
    state.translations = [];
  }
}


const renderDictionary = (state, dispatch) => d => (!d.loaded) ? spinner('dictionary'): (
  <label class = "dictionary">
    <input type = "checkbox"
           name = {d.name}
           checked = {d.enabled}
           onInput = {compose(dispatch, toggleDictionary, e => e.target.name)}
    />
    <div class = "widget"/>
    <div class = "contents">
      <p>{d.name}</p>
    </div>
  </label>
)

const renderDictionaries = (state, dispatch) => (undefined == state.dictionaries)
  ? ''
  : (!state.dictionaries.filter(d => d.category == state.category).length)
    ? spinner('dictionaries')
    : (
      <div class = "dictionaries">
       {state.dictionaries
        .filter(d => d.category == state.category)
        .map(renderDictionary(state, dispatch))}
      </div>
    );

export {loadDictionary, loadDictionaries, toggleDictionary, renderDictionary, renderDictionaries};
