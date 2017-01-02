import Inferno from 'inferno';
import Component from 'inferno-component';

import {compose, getJSON, modifyState} from './common';
import {initDebug, renderDebug} from './debug';
import {loadCategories, renderCategories} from './category';
import {renderDictionaries} from './dictionary';
import {loadTranslations, renderTranslations} from './translation';
import spinner from './spinner';


const updateQuery = query => (state, dispatch) => {
  state.query = query;
  dispatch(loadTranslations(state.category, query));
}


const init = (state, dispatch) => {
  state.query = "";
  dispatch(initDebug());
  dispatch(loadCategories);
}


const view = (state, dispatch) => (
  <div className = "container">
    <form className = "search">
      <input type = "input"
             placeholder = "_"
             onInput = {compose(dispatch, updateQuery, e => e.target.value)}/>
    </form>
    {renderCategories(state, dispatch)}
    {renderDictionaries(state, dispatch)}
    {renderTranslations(state, dispatch)}
    {renderDebug(state, dispatch)}
  </div>
);


export default {init, view};

