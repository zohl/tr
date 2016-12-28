import Inferno from 'inferno';
import {compose} from './common';

const toggleDictionary = name => (state, dispatch) => {
  var dIndex = state.dictionaries.findIndex(dictionary => dictionary.name == name);
  if (-1 == dIndex) {
    return;
  }

  state.dictionaries[dIndex].enabled = !state.dictionaries[dIndex].enabled;
}


const dictionary = (state, dispatch) => d => (
  <label className = "dictionary">
    <input type = "checkbox"
           name = {d.name}
           checked = {d.enabled}
           onInput = {compose(dispatch, toggleDictionary, e => e.target.name)}
    />
    <div className = "widget"/>
    <div className = "contents">
      <p>{d.name}</p>
    </div>
  </label>
);

export {dictionary, toggleDictionary};

